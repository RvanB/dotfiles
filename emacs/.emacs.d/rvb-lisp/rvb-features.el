;;; rvb-features.el --- Multi-repo features via git worktrees  -*- lexical-binding: t; -*-

;; A "feature" is a unit of work that spans several Git repositories.
;; It lives in its own directory under `rvb-feature-directory', holding
;; one Git worktree per member repository:
;;
;;   ~/features/
;;     add-sso/
;;       .feature.eld       ; base refs and provenance
;;       auth-service/      ; worktree of ~/code/auth-service
;;       web-ui/            ; worktree of ~/code/web-ui
;;
;; Because every member is a worktree, the filesystem *is* the database:
;; membership is "which worktrees exist here", and every piece of status
;; comes from git on demand.  The record file only stores what git will
;; not remember -- which ref each branch started from.
;;
;; Repositories are added one at a time with `rvb-feature-add-repo',
;; called from anywhere inside the repo, so a feature can grow as you
;; discover what it touches.
;;
;; `rvb-feature-status' is the way in, and the prefix argument moves
;; between features.  Opening it sets `default-directory' to the feature
;; directory, and feature directories are registered as `project.el'
;; roots (see rvb-projects.el), so `project-find-file' and
;; `project-find-regexp' are scoped to the feature from then on.  They
;; are deliberately kept out of the `project-switch-project' prompt:
;; a feature has its own entry point, and its directory is not a
;; repository, so `project-switch-commands' would offer a Magit entry
;; that cannot work.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'cus-edit)
(require 'json)
(require 'magit)
(require 'org)
(require 'transient)

;; Defined in rvb-projects.el, which this module does not require.
(defvar rvb/project-extra-roots-functions)

;; Keep declarations for the Org entry points used below explicit.
(declare-function org-narrow-to-subtree "org" (&optional element))
(declare-function org-fold-hide-subtree "org-fold" ())

;; Optional: rvb-github.el supplies issue lookups and the issue body
;; sync.  Everything here degrades gracefully without it.
(declare-function rvb/github-fetch-body "rvb-github" (key callback))
(declare-function rvb/github-set-body "rvb-github" (key body callback))

;; Made buffer-local further down, with the status buffer it belongs to.
(defvar rvb-feature--buffer-feature)

(defgroup rvb-feature nil
  "Orchestrate work that spans multiple Git repositories."
  :group 'tools
  :prefix "rvb-feature-")

(defcustom rvb-feature-directory (expand-file-name "~/features/")
  "Directory holding one subdirectory per feature.
Each feature subdirectory contains a Git worktree per member
repository plus a `.feature.eld' record."
  :type 'directory
  :group 'rvb-feature)

(defcustom rvb-feature-branch-function #'rvb-feature-default-branch-name
  "Function returning the branch name for a feature member.
Called with the feature name and the repository name, so the branch
may differ per repository if you want it to."
  :type 'function
  :group 'rvb-feature)

(defcustom rvb-feature-setup-functions nil
  "Abnormal hook run in each newly created worktree.
Each function is called with the worktree directory and the
originating clone, with `default-directory' bound to the worktree.

A fresh worktree has none of the files Git ignores -- .env, venv,
node_modules, build caches -- so this hook is what makes a new
member actually usable.  Dispatch on what exists in the origin
clone, for example:

  (add-hook \\='rvb-feature-setup-functions
            (lambda (worktree origin)
              (dolist (f \\='(\".env\" \".envrc\"))
                (let ((src (expand-file-name f origin)))
                  (when (file-exists-p src)
                    (make-symbolic-link
                     src (expand-file-name f worktree) t))))))"
  :type 'hook
  :group 'rvb-feature)

(defcustom rvb-feature-fetch-before-add t
  "Whether to fetch before branching a repository off its remote.
A repository added weeks into a feature should start from current
upstream, not from whatever the clone last pulled."
  :type 'boolean
  :group 'rvb-feature)

(defcustom rvb-feature-fill-column 70
  "Column that prose is filled to in the feature buffers.
Applies to descriptions only."
  :type 'integer
  :group 'rvb-feature)

(defconst rvb-feature-record-name ".feature.eld"
  "Name of the record file inside a feature directory.")

(defconst rvb-feature-org-name "feature.org"
  "Name of the Org file holding a feature's descriptions.

One file per feature rather than one per repository, so the whole
feature reads as a single document: the text before the first heading
describes the feature, and a top-level heading per member repository
describes that repository's share of it.

    #+title: add-sso
    #+issue: https://github.com/cdlib/zephir-reports/issues/42

    Single sign-on across the estate.

    * auth-service
    Adds the OIDC callback endpoint.

    * web-ui
    Swaps the login form for a redirect.

`rvb-feature-pr-body' joins the preamble and a repository's subtree
into the body of that repository's pull request.

An optional `#+issue:' keyword links the feature to a GitHub issue or
pull request, shown under the feature's name in both the list and the
status buffer.  See `rvb-feature-issue'.")


;;; Git plumbing

(defun rvb-feature--git (dir &rest args)
  "Run git ARGS in DIR.  Return trimmed output, or nil on failure/empty."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil t nil args))
        (let ((out (string-trim (buffer-string))))
          (unless (string-empty-p out) out))))))

(defun rvb-feature--git-lines (dir &rest args)
  "Run git ARGS in DIR and return the output as a list of lines."
  (when-let* ((out (apply #'rvb-feature--git dir args)))
    (split-string out "\n" t)))

(defun rvb-feature--git-ok (dir &rest args)
  "Return non-nil if git ARGS exits successfully in DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (zerop (apply #'process-file "git" nil nil nil args))))

(defun rvb-feature--git! (dir &rest args)
  "Run git ARGS in DIR, signalling an error that includes git's output."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (error "git %s: %s" (string-join args " ") (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun rvb-feature--toplevel (&optional dir)
  "Return the working tree root containing DIR, or nil."
  (rvb-feature--git (or dir default-directory) "rev-parse" "--show-toplevel"))

(defun rvb-feature--main-worktree (dir)
  "Return the main worktree of DIR's repository.
Never a linked worktree, so adding a repo while visiting one feature
still branches from the canonical clone."
  (when-let* ((first (car (rvb-feature--git-lines dir "worktree" "list" "--porcelain"))))
    (when (string-prefix-p "worktree " first)
      (file-name-as-directory (substring first (length "worktree "))))))

(defun rvb-feature--branch-worktree (repo branch)
  "Return the worktree of REPO that has BRANCH checked out, if any."
  (let ((ref (concat "branch refs/heads/" branch))
        current found)
    (dolist (line (rvb-feature--git-lines repo "worktree" "list" "--porcelain") found)
      (cond ((string-prefix-p "worktree " line)
             (setq current (substring line (length "worktree "))))
            ((equal line ref)
             (setq found current))))))

(defun rvb-feature--branch-p (repo branch)
  "Return non-nil if BRANCH exists locally in REPO."
  (rvb-feature--git-ok repo "show-ref" "--verify" "--quiet" (concat "refs/heads/" branch)))

(defun rvb-feature--remote-branch-p (repo branch)
  "Return non-nil if origin/BRANCH exists in REPO."
  (rvb-feature--git-ok repo "show-ref" "--verify" "--quiet"
                       (concat "refs/remotes/origin/" branch)))

(defun rvb-feature--dirty-p (dir)
  "Return non-nil if DIR has uncommitted changes."
  (and (rvb-feature--git dir "status" "--porcelain") t))

(defun rvb-feature--default-base (repo)
  "Return the ref REPO's feature branches should start from."
  (or (rvb-feature--git repo "symbolic-ref" "--short" "refs/remotes/origin/HEAD")
      (cl-find-if (lambda (ref)
                    (rvb-feature--git-ok repo "rev-parse" "--verify" "--quiet" ref))
                  '("origin/main" "origin/master"))
      (rvb-feature--git repo "rev-parse" "--abbrev-ref" "HEAD")))

(defun rvb-feature--park-args (repo base)
  "Return checkout arguments moving REPO off a feature branch.
Prefers the local branch matching BASE, falling back to a detached
checkout of BASE itself."
  (let ((local (replace-regexp-in-string "\\`[^/]+/" "" base)))
    (if (rvb-feature--branch-p repo local)
        (list local)
      (list "--detach" base))))

(defun rvb-feature--refs (repo)
  "Return candidate base refs in REPO, for completion."
  (rvb-feature--git-lines repo "for-each-ref" "--format=%(refname:short)"
                          "refs/heads" "refs/remotes"))

(defun rvb-feature--branch-candidates (repo)
  "Return branch names in REPO usable as a feature branch.
Local branches, plus remote branches with their remote prefix stripped,
since checking out `origin/feat/x' means creating `feat/x'."
  (let ((locals (rvb-feature--git-lines repo "for-each-ref"
                                        "--format=%(refname:short)" "refs/heads"))
        (remotes (rvb-feature--git-lines repo "for-each-ref"
                                         "--format=%(refname:short)" "refs/remotes")))
    (delete-dups
     (append locals
             (delq nil
                   (mapcar (lambda (r)
                             (let ((short (replace-regexp-in-string "\\`[^/]+/" "" r)))
                               (unless (equal short "HEAD") short)))
                           remotes))))))


;;; Features and their records

(defun rvb-feature-default-branch-name (feature _repo)
  "Return the default branch name for FEATURE."
  (concat "feat/" feature))

(defun rvb-feature--dir (name)
  "Return the absolute directory of feature NAME."
  (file-name-as-directory (expand-file-name name rvb-feature-directory)))

(defun rvb-feature--names ()
  "Return the names of all features, sorted."
  (when (file-directory-p rvb-feature-directory)
    (sort (cl-remove-if-not
           (lambda (n) (file-directory-p (expand-file-name n rvb-feature-directory)))
           (directory-files rvb-feature-directory nil directory-files-no-dot-files-regexp))
          #'string<)))

(defun rvb-feature--enclosing (&optional dir)
  "Return the feature DIR belongs to, or nil."
  (let ((root (file-name-as-directory (expand-file-name rvb-feature-directory)))
        (dir (file-name-as-directory (expand-file-name (or dir default-directory)))))
    (when (string-prefix-p root dir)
      (car (split-string (substring dir (length root)) "/" t)))))

(defun rvb-feature--record-file (feature)
  (expand-file-name rvb-feature-record-name (rvb-feature--dir feature)))

(defun rvb-feature--read-record (feature)
  (let ((file (rvb-feature--record-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (ignore-errors (read (current-buffer)))))))

(defun rvb-feature--write-record (feature record)
  (let ((file (rvb-feature--record-file feature)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";;; -*- lisp-data -*-  Managed by rvb-features.el\n")
      (let ((print-length nil) (print-level nil))
        (pp record (current-buffer))))
    record))

(defun rvb-feature-members (feature)
  "Return member plists for FEATURE.
Membership is the recorded members plus any worktree found in the
feature directory that is not recorded, so a worktree added by hand is
still picked up.  Keys: :name :dir :origin :branch :base :base-commit
:missing."
  (let* ((dir (rvb-feature--dir feature))
         (record (rvb-feature--read-record feature))
         (members
          (mapcar (lambda (m)
                    (let ((abs (file-name-as-directory
                                (expand-file-name (plist-get m :dir) dir))))
                      (list :name (plist-get m :dir)
                            :dir abs
                            :origin (plist-get m :origin)
                            :branch (plist-get m :branch)
                            :base (plist-get m :base)
                            :base-commit (plist-get m :base-commit)
                            :missing (not (file-directory-p abs)))))
                  (plist-get record :members)))
         (known (mapcar (lambda (m) (plist-get m :name)) members))
         extra)
    (dolist (f (and (file-directory-p dir)
                    (directory-files dir nil directory-files-no-dot-files-regexp)))
      (let ((abs (file-name-as-directory (expand-file-name f dir))))
        (when (and (not (member f known))
                   (file-directory-p abs)
                   (file-exists-p (expand-file-name ".git" abs)))
          (push (list :name f
                      :dir abs
                      :origin (rvb-feature--main-worktree abs)
                      :branch (rvb-feature--git abs "rev-parse" "--abbrev-ref" "HEAD")
                      :base nil :base-commit nil :missing nil)
                extra))))
    (append members (nreverse extra))))

(defun rvb-feature--record-member (feature member)
  "Append MEMBER to FEATURE's record."
  (let ((record (or (rvb-feature--read-record feature)
                    (list :version 1 :name feature))))
    (rvb-feature--write-record
     feature
     (plist-put record :members (append (plist-get record :members) (list member))))))

(defun rvb-feature--forget-member (feature name)
  "Drop the member named NAME from FEATURE's record."
  (when-let* ((record (rvb-feature--read-record feature)))
    (rvb-feature--write-record
     feature
     (plist-put record :members
                (cl-remove-if (lambda (m) (equal (plist-get m :dir) name))
                              (plist-get record :members))))))

;;; Descriptions

(defun rvb-feature--org-file (feature)
  "Return the Org description file for FEATURE."
  (expand-file-name rvb-feature-org-name (rvb-feature--dir feature)))

(defun rvb-feature--ensure-org (feature)
  "Create FEATURE's Org file if it does not exist.  Return its path."
  (let ((file (rvb-feature--org-file feature)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "#+title: " feature "\n\n")))
    file))

(defun rvb-feature--heading-regexp (name)
  (format "^\\* %s[ \t]*$" (regexp-quote name)))

(defconst rvb-feature--top-heading-regexp "^\\* +\\(.*?\\)[ \t]*$"
  "Match a top-level heading line, capturing everything after the star.")

(defun rvb-feature--todo-keywords ()
  "Return the configured TODO keywords, without their fast-access keys."
  (let (words)
    (dolist (spec (and (boundp 'org-todo-keywords)
                       (default-value 'org-todo-keywords)))
      (dolist (word (if (consp spec) (cdr spec) (list spec)))
        (when (and (stringp word) (not (equal word "|")))
          (push (replace-regexp-in-string "(.*)\\'" "" word) words))))
    (or words '("TODO" "DONE"))))

(defun rvb-feature--heading-text (raw)
  "Return the plain text of heading RAW, without keyword, priority or tags.

A repository's heading is matched on this rather than on the whole
line, so `* TODO auth-service :urgent:' still belongs to the
auth-service worktree.  Anything Org lets you decorate a heading with
stays decoration.

The keyword is matched against `org-todo-keywords' rather than against
a shape like \"a leading capitalised word\", which would eat the first
word of \"Open questions\"."
  (let ((text (string-trim (or raw "")))
        (case-fold-search nil))
    ;; Tags first: they are anchored to the end of the line.
    (when (string-match "\\`\\(.*?\\)[ \t]+:[[:alnum:]_@#%:]+:\\'" text)
      (setq text (match-string 1 text)))
    ;; Then a TODO keyword, which Org requires to come first.
    (when-let* ((word (car (split-string text)))
                ((member word (rvb-feature--todo-keywords))))
      (setq text (string-trim (substring text (length word)))))
    ;; Then a priority cookie, which follows the keyword.
    (when (string-match "\\`\\[#[A-Z0-9]\\][ \t]*\\(.*\\)\\'" text)
      (setq text (match-string 1 text)))
    (string-trim text)))

(defun rvb-feature--member-names (feature)
  "Return the names of FEATURE's member repositories."
  (mapcar (lambda (m) (plist-get m :name)) (rvb-feature-members feature)))

(defun rvb-feature--ensure-heading (feature name)
  "Ensure FEATURE's Org file has a top-level heading for NAME."
  (let ((file (rvb-feature--ensure-org feature)))
    (with-temp-buffer
      (insert-file-contents file)
      (unless (rvb-feature--goto-heading name)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (unless (looking-back "\n\n" 2) (insert "\n"))
        (insert "* " name "\n")
        (write-region (point-min) (point-max) file nil 'quiet)))
    file))

(defun rvb-feature--fill-prose ()
  "Fill this Org buffer's prose to `rvb-feature-fill-column'.

Filling is done by Org, so paragraphs, list items and their
continuation indents come out right.  Everything starting `#+' is
stepped over, because `fill-region' treats those lines as ordinary
prose: it reflows the inside of a `#+begin_src' block, where code
decides its own line length, and it runs consecutive keyword lines
together into one paragraph."
  (let ((fill-column rvb-feature-fill-column)
        (case-fold-search t)
        ;; Markers, because filling one region moves everything after it.
        (start (copy-marker (point-min)))
        (end (copy-marker (point-max))))
    (save-excursion
      (while (progn (goto-char start)
                    (re-search-forward "^[ \t]*#\\+" end t))
        (let ((skip-start (copy-marker (line-beginning-position))))
          (fill-region start skip-start)
          (goto-char skip-start)
          (cond
           ;; A block: step over its contents as well as its delimiters.
           ((looking-at "[ \t]*#\\+begin_")
            (if (re-search-forward "^[ \t]*#\\+end_.*$" end t)
                (progn (forward-line 1)
                       (set-marker start (min (point) (marker-position end))))
              (set-marker start end)))
           ;; A keyword: just this line.
           (t
            (forward-line 1)
            (set-marker start (min (point) (marker-position end)))))))
      (when (< (marker-position start) (marker-position end))
        (fill-region start end)))))

(defun rvb-feature--face-to-font-lock-face ()
  "Move every `face' property in the current buffer to `font-lock-face'.
This preserves fontification when text is copied into a buffer whose
font-lock machinery manages ordinary `face' properties itself."
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let ((next (or (next-single-property-change pos 'face) (point-max)))
            (face (get-text-property pos 'face)))
        (when face
          (put-text-property pos next 'font-lock-face face)
          (remove-text-properties pos next '(face nil)))
        (setq pos next)))))

(defun rvb-feature--org-sections (feature &optional members)
  "Return an alist of (KEY . BODY) for FEATURE's Org file.

BODY is fontified as Org would show it.  Keys are:

  nil     the feature's own writing: the text before the first
          heading, minus keyword lines, followed by every top-level
          heading that does not name a repository, kept whole
  NAME    a top-level heading naming one of MEMBERS -- that
          repository's description, sub-headings and all

Only headings that name a member are repositories.  The rest is yours:
notes, TODO entries, whatever structure you want, at any level.  Below
a repository's heading it all belongs to that repository; elsewhere it
is the feature's own, and shown as one piece rather than split into a
separate section you cannot get at.

MEMBERS defaults to the feature's member names.  The whole file is
fontified once and sliced up, so a refresh activates `org-mode' a
single time no matter how many repositories the feature has."
  (let ((file (rvb-feature--org-file feature))
        (members (or members (rvb-feature--member-names feature)))
        notes result)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        ;; The full `org-mode-hook', not `delay-mode-hooks': whatever
        ;; styles Org buffers should style this text too, and naming
        ;; those packages here would be this file's business to keep up
        ;; to date.
        (org-mode)
        ;; Fill before fontifying: filling moves text, and the faces are
        ;; what we are about to slice out.
        (rvb-feature--fill-prose)
        (font-lock-ensure)
        (rvb-feature--face-to-font-lock-face)
        (cl-flet ((slice (start end)
                    (let ((s (string-trim (buffer-substring start end))))
                      (unless (string-empty-p s) s))))
          (goto-char (point-min))
          (let ((end (if (re-search-forward "^\\* " nil t)
                         (match-beginning 0)
                       (point-max))))
            ;; Skip #+title: and friends; they are not prose.
            (goto-char (point-min))
            (while (and (< (point) end) (looking-at "^#\\+"))
              (forward-line))
            (when-let* ((text (slice (point) end)))
              (push text notes)))
          (goto-char (point-min))
          (while (re-search-forward rvb-feature--top-heading-regexp nil t)
            (let* ((raw (substring-no-properties (match-string 1)))
                   (name (rvb-feature--heading-text raw))
                   (heading-start (line-beginning-position))
                   (body-start (min (point-max) (1+ (line-end-position))))
                   (end (if (re-search-forward "^\\* " nil t)
                            (goto-char (match-beginning 0))
                          (point-max))))
              (if (member name members)
                  (push (cons name (slice body-start end)) result)
                ;; Not a repository: keep the heading, it is part of
                ;; what was written.
                (when-let* ((text (slice heading-start end)))
                  (push text notes)))))
          (when notes
            (push (cons nil (string-join (nreverse notes) "\n\n")) result)))))
    ;; The feature's own writing first, then the repositories.
    (let ((own (assoc nil result)))
      (cons (or own (cons nil nil))
            (nreverse (delq own result))))))

(defun rvb-feature--goto-heading (name)
  "Move point past the top-level heading naming NAME.  Return non-nil if found.
Matches on the heading's text, so a TODO keyword or tags on it make no
difference."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (re-search-forward rvb-feature--top-heading-regexp nil t))
      (when (equal name (rvb-feature--heading-text
                         (substring-no-properties (match-string 1))))
        (setq found t)))
    found))

(defun rvb-feature-description (feature name)
  "Return the description text for repository NAME in FEATURE, unfontified."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (when (rvb-feature--goto-heading name)
          (forward-line)
          (let* ((start (point))
                 (end (if (re-search-forward "^\\* " nil t)
                          (match-beginning 0)
                        (point-max)))
                 (s (string-trim (buffer-substring-no-properties start end))))
            (unless (string-empty-p s) s)))))))

;;; The linked issue

(defconst rvb-feature--issue-keyword-regexp
  (rx bol (* (any " \t")) "#+issue:" (* (any " \t")) (group (+ nonl)))
  "Match the `#+issue:' keyword in a feature's Org file.")

(defun rvb-feature-issue (feature)
  "Return the GitHub issue FEATURE is linked to, as \"owner/repo#number\".

Read from a `#+issue:' keyword in the feature's Org file, which takes
either a full GitHub URL or the short form:

    #+issue: https://github.com/cdlib/zephir-reports/issues/42
    #+issue: cdlib/zephir-reports#42

Returns nil when there is no such keyword or it cannot be parsed."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward rvb-feature--issue-keyword-regexp nil t)
          (let ((value (string-trim (match-string 1))))
            (cond
             ((string-match (rx "github.com/"
                                (group (+ (not (any "/")))) "/"
                                (group (+ (not (any "/")))) "/"
                                (or "issues" "pull") "/" (group (+ digit)))
                            value)
              (format "%s/%s#%s" (match-string 1 value) (match-string 2 value)
                      (match-string 3 value)))
             ((string-match (rx bos (group (+ (not (any "/ ")))) "/"
                                (group (+ (not (any "# ")))) "#"
                                (group (+ digit)) eos)
                            value)
              (format "%s/%s#%s" (match-string 1 value) (match-string 2 value)
                      (match-string 3 value))))))))))

(defvar-keymap rvb-feature-issue-map
  :doc "Keymap on the linked-issue line."
  "RET" #'rvb-feature-visit-issue
  "<mouse-1>" #'rvb-feature-visit-issue)

(defun rvb-feature-visit-issue (&optional event)
  "Open the issue linked at point, or at EVENT's position."
  (interactive (list last-nonmenu-event))
  (let* ((pos (if (and event (listp event))
                  (posn-point (event-end event))
                (point)))
         (url (get-text-property pos 'rvb-feature-issue-url)))
    (if url (browse-url url) (user-error "No issue here"))))

(defun rvb-feature--issue-line (feature refresh)
  "Return the display line for FEATURE's linked issue, or nil.

The issue's title is the clickable text -- that is the part worth
reading -- falling back to the `owner/repo#number' reference while the
title is unknown, or when rvb-github.el is not loaded to look it up.
REFRESH is called when a pending lookup lands."
  (when-let* ((key (rvb-feature-issue feature)))
    (let* ((info (and (fboundp 'rvb/github-lookup)
                      (rvb/github-lookup key refresh)))
           (url (if (fboundp 'rvb/github-url)
                    (rvb/github-url key)
                  (concat "https://github.com/"
                          (replace-regexp-in-string "#" "/issues/" key))))
           (state (and info (fboundp 'rvb/github-state-string)
                       (rvb/github-state-string (plist-get info :state))))
           (text (or (plist-get info :title) key)))
      (rvb-feature--string-faces-to-font-lock
       (concat
        (propertize text
                    'font-lock-face 'rvb-feature-issue
                    'mouse-face 'highlight
                    'help-echo url
                    'keymap rvb-feature-issue-map
                    'rvb-feature-issue-url url)
        (if (and state (not (string-empty-p state)))
            (concat "  " (copy-sequence state))
          ""))))))


(defun rvb-feature--scan-regions (members)
  "Return the current Org buffer's regions as (KIND START END NAME).

KIND is `preamble' for the text after the `#+' keyword lines and
before the first heading, `repo' for a top-level heading naming one of
MEMBERS together with everything under it, and `own' for any other
top-level heading and its contents.  NAME is the heading's text, or
nil for the preamble."
  (let (regions)
    (goto-char (point-min))
    (let ((first (if (re-search-forward "^\\* " nil t)
                     (match-beginning 0)
                   (point-max))))
      (goto-char (point-min))
      (while (and (< (point) first) (looking-at "^#\\+"))
        (forward-line))
      (push (list 'preamble (point) first nil) regions)
      (goto-char (point-min))
      (while (re-search-forward rvb-feature--top-heading-regexp nil t)
        (let* ((name (rvb-feature--heading-text
                      (substring-no-properties (match-string 1))))
               (start (line-beginning-position))
               (end (if (re-search-forward "^\\* " nil t)
                        (goto-char (match-beginning 0))
                      (point-max))))
          (push (list (if (member name members) 'repo 'own) start end name)
                regions))))
    (nreverse regions)))


(defun rvb-feature-preamble (feature)
  "Return FEATURE's introduction, unfontified: the text above the first
heading, minus the `#+' keyword lines."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (when-let* ((region (assq 'preamble
                                  (rvb-feature--scan-regions
                                   (rvb-feature--member-names feature)))))
          (let ((s (string-trim (buffer-substring-no-properties
                                 (nth 1 region) (nth 2 region)))))
            (unless (string-empty-p s) s)))))))

(defun rvb-feature-own-text (feature)
  "Return FEATURE's own writing, unfontified.

Everything that is not a repository's: the text above the first
heading, plus any top-level heading that does not name a member, in
file order.  This is what `rvb-feature-issue-push' sends."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (let ((members (rvb-feature--member-names feature)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((parts
                 (delq nil
                       (mapcar (pcase-lambda (`(,kind ,start ,end ,_name))
                                 (unless (eq kind 'repo)
                                   (let ((s (string-trim
                                             (buffer-substring-no-properties
                                              start end))))
                                     (unless (string-empty-p s) s))))
                               (rvb-feature--scan-regions members)))))
            (and parts (string-join parts "\n\n"))))))))

(defun rvb-feature--set-own-text (feature text)
  "Replace FEATURE's own writing with TEXT, keeping the repository sections.

The inverse of `rvb-feature-own-text': TEXT lands where the preamble
was, and any other feature-level heading is removed, because TEXT
already contains whatever those said.  Without that the two would not
round-trip -- pushing and then pulling would leave every note in the
file twice."
  (let ((file (rvb-feature--ensure-org feature))
        (members (rvb-feature--member-names feature)))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((regions (rvb-feature--scan-regions members))
             (preamble (assq 'preamble regions)))
        ;; Back to front, so earlier positions stay valid.
        (dolist (region (reverse regions))
          (when (eq (car region) 'own)
            (delete-region (nth 1 region) (nth 2 region))))
        (delete-region (nth 1 preamble) (nth 2 preamble))
        (goto-char (nth 1 preamble))
        (insert "\n" (string-trim text) "\n\n"))
      (write-region (point-min) (point-max) file nil 'quiet))
    file))

;;; Org on this side, Markdown on GitHub's
;;
;; Pandoc converts between them, so the file stays Org -- headings,
;; TODO entries, links, the lot -- while the issue gets Markdown that
;; renders properly.  The flags matter:
;;
;;   -raw_html            org's TODO keywords otherwise arrive as
;;                        <span class="todo TODO">TODO</span>
;;   -gfm_auto_identifiers  Markdown headings otherwise come back with
;;                        a :PROPERTIES: :CUSTOM_ID: drawer each
;;   --wrap=preserve      leaves line breaks alone rather than
;;                        rewrapping, so a push does not rewrite text
;;                        it did not need to touch

(defcustom rvb-feature-pandoc-executable "pandoc"
  "Program used to convert between Org and GitHub-flavoured Markdown."
  :type 'string
  :group 'rvb-feature)

(defun rvb-feature--pandoc (text from to)
  "Convert TEXT FROM one format TO another with Pandoc."
  (unless (executable-find rvb-feature-pandoc-executable)
    (user-error "%s is not installed; it is needed to convert to and from \
GitHub's Markdown" rvb-feature-pandoc-executable))
  (with-temp-buffer
    (insert text)
    (let ((status (call-process-region
                   (point-min) (point-max) rvb-feature-pandoc-executable
                   t t nil "-f" from "-t" to "--wrap=preserve")))
      (unless (eq status 0)
        (error "pandoc %s -> %s failed: %s" from to
               (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun rvb-feature--to-markdown (org)
  "Convert ORG text to GitHub-flavoured Markdown."
  (rvb-feature--pandoc org "org" "gfm-raw_html"))

(defun rvb-feature--from-markdown (markdown)
  "Convert GitHub-flavoured MARKDOWN to Org."
  (rvb-feature--pandoc markdown "gfm-gfm_auto_identifiers" "org"))

(defun rvb-feature--issue-or-error (feature)
  "Return FEATURE's linked issue, or explain that there is not one."
  (or (rvb-feature-issue feature)
      (user-error
       "No issue linked; add a `#+issue:' line to %s"
       (abbreviate-file-name (rvb-feature--org-file feature)))))

(defun rvb-feature--after-issue-sync (feature)
  "Redraw whatever is showing FEATURE."
  (dolist (name (list (format "*feature: %s*" feature) "*features*"))
    (when-let* ((buf (get-buffer name)))
      (with-current-buffer buf
        (cond ((derived-mode-p 'rvb-feature-status-mode) (rvb-feature-refresh))
              ((derived-mode-p 'rvb-feature-list-mode)
               (rvb-feature-list-refresh)))))))

;;;###autoload
(defun rvb-feature-issue-pull (feature)
  "Replace FEATURE's description with the body of its linked issue.

Replaces everything of the feature's own -- the text above the first
heading and any feature-level heading below it -- because that is what
`rvb-feature-issue-push' sends.  The repository sections are left
alone; they have no counterpart on GitHub.

Feature-level headings you already have are removed, since the fetched
body contains whatever they said.  That is what makes push and pull
inverse instead of duplicating your notes on every round trip.

The body arrives as Markdown and is converted to Org by Pandoc, so the
file stays Org throughout."
  (interactive (list (or rvb-feature--buffer-feature (rvb-feature--read-name t))))
  (let ((key (rvb-feature--issue-or-error feature))
        (current (rvb-feature-own-text feature)))
    (rvb/github-fetch-body
     key
     (lambda (body)
       (when body
         (let ((body (string-trim body)))
           (cond
            ((string-empty-p body)
             (message "%s has an empty body; nothing to pull" key))
            ((equal body (string-trim (or current "")))
             (message "%s already matches %s" feature key))
            ((and current
                  (not (yes-or-no-p
                        (format "Replace %s's description with the body of %s? "
                                feature key))))
             (message "Kept the local description"))
            (t
             (rvb-feature--set-own-text feature (rvb-feature--from-markdown body))
             (rvb-feature--after-issue-sync feature)
             (message "Pulled %s into %s" key feature)))))))))

;;;###autoload
(defun rvb-feature-issue-push (feature)
  "Set the body of FEATURE's linked issue from its description.

Sends everything of the feature's own: the text above the first
heading and any feature-level heading below it.  Only the repository
sections are held back -- those are per-repository and belong in their
pull requests, not on the issue.

The text is converted from Org to GitHub-flavoured Markdown by Pandoc,
so it renders on the issue as it reads here.

This rewrites the issue on GitHub, where other people can see it and
where there is no undo, so it always asks first."
  (interactive (list (or rvb-feature--buffer-feature (rvb-feature--read-name t))))
  (let* ((key (rvb-feature--issue-or-error feature))
         (body (rvb-feature--to-markdown
                (or (rvb-feature-own-text feature)
                    (user-error "%s has no description to push" feature)))))
    (unless (yes-or-no-p
             (format "Replace the body of %s on GitHub with %s's description? "
                     key feature))
      (user-error "Aborted"))
    (rvb/github-set-body
     key body
     (lambda (result)
       (when result
         (rvb-feature--after-issue-sync feature)
         (message "Pushed %s's description to %s" feature key))))))

(defun rvb-feature-pr-body (feature name)
  "Return the pull-request body for repository NAME in FEATURE.
The feature's introduction followed by that repository's own section,
as Org.  A future \"create PRs\" command converts it with
`rvb-feature--to-markdown', the same way pushing an issue does."
  (let ((parts (delq nil (list (rvb-feature-preamble feature)
                               (rvb-feature-description feature name)))))
    (and parts (string-join parts "\n\n"))))

(defun rvb-feature--read-name (&optional require-match)
  "Prompt for a feature name, defaulting to the enclosing one."
  (let ((features (rvb-feature--names))
        (default (rvb-feature--enclosing)))
    (when (and require-match (null features))
      (user-error "No features yet"))
    (completing-read (format-prompt "Feature" default)
                     features nil require-match nil nil default)))


;;; Adding a repository

(defun rvb-feature--read-repository (&optional default)
  "Prompt for a repository, defaulting to DEFAULT.

Completes over the repositories Magit knows about, which is the list
you already curate through `magit-repository-directories', rather than
making you navigate the filesystem to somewhere you visit constantly.
Any path can still be typed, and if Magit knows of none this falls
back to reading a directory."
  (let ((candidates (and (fboundp 'magit-list-repos)
                         (mapcar (lambda (dir)
                                   (abbreviate-file-name
                                    (directory-file-name dir)))
                                 (magit-list-repos))))
        (default (and default
                      (abbreviate-file-name (directory-file-name default)))))
    (expand-file-name
     (if candidates
         (completing-read (format-prompt "Repository" default)
                          candidates nil nil nil nil default)
       (read-directory-name "Repository: " default default t)))))

;;;###autoload
(defun rvb-feature-create (feature)
  "Create FEATURE with no repositories yet and write its description.

A feature usually starts as an intention rather than a set of
repositories -- you know what you are about to do before you know
everything it touches.  This makes the directory and its Org file and
opens its status buffer, where the description is written directly;
add repositories with `rvb-feature-add-repo' as you discover them."
  (interactive (list (read-string "New feature: ")))
  (let ((feature (string-trim feature)))
    (when (string-empty-p feature)
      (user-error "Feature name may not be empty"))
    (when (string-match-p "/" feature)
      (user-error "Feature name may not contain a slash"))
    (when (string-prefix-p "." feature)
      (user-error "Feature name may not start with a dot"))
    (if (file-directory-p (rvb-feature--dir feature))
        (progn (message "Feature %s already exists" feature)
               (rvb-feature-status feature))
      (make-directory (rvb-feature--dir feature) t)
      (rvb-feature--ensure-org feature)
      ;; The status buffer is where the description is written now.
      (rvb-feature-status feature))))

;;;###autoload
(defun rvb-feature-add-repo (feature &optional repo branch base)
  "Add REPO to FEATURE as a Git worktree, creating FEATURE if needed.

REPO is prompted for, completing over the repositories Magit knows
about (see `magit-repository-directories'), with any path still
typeable.  Being inside a repository is a convenience, not a
requirement: it is offered as the default, so adding the one you are
looking at is a single RET.  Whatever you pick is resolved to its main
worktree, never a linked one.

BRANCH defaults to `rvb-feature-branch-function' and BASE to the
repository's default branch.  With a prefix argument, prompt for both.
Use that when a repository needs a branch name of its own, or to pull
work that already lives on some other branch into the feature: naming
an existing local or remote branch checks it out in the new worktree
rather than creating one."
  (interactive
   (let* ((guess (when-let* ((top (rvb-feature--toplevel)))
                   (rvb-feature--main-worktree top)))
          (feature (rvb-feature--read-name))
          (chosen (rvb-feature--read-repository guess))
          (repo (or (when-let* ((top (rvb-feature--toplevel chosen)))
                      (rvb-feature--main-worktree top))
                    (user-error "%s is not inside a Git repository"
                                (abbreviate-file-name chosen)))))
     (list feature repo
           (when current-prefix-arg
             (let ((default (funcall rvb-feature-branch-function
                                     feature
                                     (file-name-nondirectory
                                      (directory-file-name repo)))))
               (completing-read (format-prompt "Branch" default)
                                (rvb-feature--branch-candidates repo)
                                nil nil nil nil default)))
           (when current-prefix-arg
             (let ((default (rvb-feature--default-base repo)))
               (completing-read (format-prompt "Base ref" default)
                                (rvb-feature--refs repo) nil nil nil nil default))))))
  (let* ((repo (file-name-as-directory
                (or repo (rvb-feature--main-worktree
                          (or (rvb-feature--toplevel)
                              (user-error "Not inside a Git repository"))))))
         (dir (rvb-feature--dir feature))
         (name (file-name-nondirectory (directory-file-name repo)))
         (branch (or branch (funcall rvb-feature-branch-function feature name)))
         worktree)
    (when (string-empty-p (string-trim feature))
      (user-error "Feature name may not be empty"))
    ;; Two repos can share a basename; let the user pick another directory.
    (setq worktree (expand-file-name name dir))
    (while (file-exists-p worktree)
      (setq name (read-string
                  (format "`%s' already exists in feature %s; directory name: "
                          name feature)
                  name)
            worktree (expand-file-name name dir)))
    ;; Fetch even when BASE was given: naming an existing branch is how
    ;; you adopt a teammate's work, and that ref has to be current.
    (when rvb-feature-fetch-before-add
      (message "Fetching %s..." name)
      (rvb-feature--git repo "fetch" "--quiet"))
    (setq base (or base (rvb-feature--default-base repo)))
    (let ((local (rvb-feature--branch-p repo branch))
          (remote (rvb-feature--remote-branch-p repo branch)))
      ;; Git refuses to create a worktree for a branch checked out elsewhere.
      (when-let* ((holder (and local (rvb-feature--branch-worktree repo branch))))
        (unless (y-or-n-p (format "%s is checked out in %s; move that worktree to %s? "
                                  branch (abbreviate-file-name holder) base))
          (user-error "Aborted"))
        (when (rvb-feature--dirty-p holder)
          (user-error "%s has uncommitted changes; deal with those first"
                      (abbreviate-file-name holder)))
        (apply #'rvb-feature--git! holder "checkout" "--quiet"
               (rvb-feature--park-args holder base)))
      (make-directory dir t)
      (message "Creating worktree %s..." (abbreviate-file-name worktree))
      (cond
       (local  (rvb-feature--git! repo "worktree" "add" worktree branch))
       (remote (rvb-feature--git! repo "worktree" "add" "--track" "-b" branch
                                  worktree (concat "origin/" branch)))
       (t      (rvb-feature--git! repo "worktree" "add" "-b" branch worktree base))))
    (let ((default-directory (file-name-as-directory worktree)))
      (run-hook-with-args 'rvb-feature-setup-functions worktree repo))
    (rvb-feature--record-member
     feature
     (list :dir name
           :origin repo
           :branch branch
           :base base
           ;; The fork point, so "commits in this feature" survives the
           ;; base ref moving on.
           :base-commit (or (rvb-feature--git worktree "merge-base" "HEAD" base)
                            (rvb-feature--git repo "rev-parse" "--verify" base))
           :added (format-time-string "%F")))
    ;; Give the new member somewhere to write its half of the story.
    (rvb-feature--ensure-heading feature name)
    (message "Added %s to feature %s on %s" name feature branch)
    (rvb-feature-status feature)))


;;; Collecting status

(defconst rvb-feature--sep "--rvb-feature-sep--")

(defun rvb-feature--probe-script (base)
  "Return a shell script reporting one repository's state.
One process per repository, emitting four separator-delimited sections:
the porcelain status (branch, upstream, ahead/behind and every changed
file in a single call), the fork point with BASE, the files that differ
from it, and the commits since it.

The file list compares the *working tree* to the fork point, not
HEAD, so it answers \"what does this feature change\" including work
that is not committed yet."
  (let ((sep (concat "printf '%s\\n' " (shell-quote-argument rvb-feature--sep) "\n")))
    (concat
     "base=$(git merge-base HEAD " (shell-quote-argument (or base "HEAD")) " 2>/dev/null)\n"
     "git status --porcelain=v2 --branch\n"
     sep
     "printf '%s\\n' \"$base\"\n"
     sep
     "[ -n \"$base\" ] && git diff --name-status -M \"$base\"\n"
     sep
     "[ -n \"$base\" ] && git log --format='%h%x09%s' \"$base\"..HEAD\n"
     "exit 0\n")))

(defun rvb-feature--parse-status (text dir)
  "Parse porcelain v2 TEXT for the repository at DIR."
  (let ((head nil) (upstream nil) (ahead 0) (behind 0) changes)
    (dolist (line (split-string text "\n" t))
      (cond
       ((string-prefix-p "# branch.head " line)
        (setq head (substring line (length "# branch.head "))))
       ((string-prefix-p "# branch.upstream " line)
        (setq upstream (substring line (length "# branch.upstream "))))
       ((string-match "\\`# branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" line)
        (setq ahead (string-to-number (match-string 1 line))
              behind (string-to-number (match-string 2 line))))
       ;; 1 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <path>
       ((string-match "\\`1 \\(..\\)\\(?: [^ ]+\\)\\{6\\} \\(.*\\)\\'" line)
        (push (list :xy (match-string 1 line) :path (match-string 2 line)
                    :kind 'tracked :dir dir)
              changes))
       ;; 2 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <X><score> <path>TAB<orig>
       ((string-match "\\`2 \\(..\\)\\(?: [^ ]+\\)\\{7\\} \\(.*\\)\\'" line)
        (push (list :xy (match-string 1 line)
                    :path (car (split-string (match-string 2 line) "\t"))
                    :kind 'tracked :dir dir)
              changes))
       ;; u <XY> <sub> <m1> <m2> <m3> <mW> <h1> <h2> <h3> <path>
       ((string-match "\\`u \\(..\\)\\(?: [^ ]+\\)\\{8\\} \\(.*\\)\\'" line)
        (push (list :xy (match-string 1 line) :path (match-string 2 line)
                    :kind 'unmerged :dir dir)
              changes))
       ((string-prefix-p "? " line)
        (push (list :xy "??" :path (substring line 2) :kind 'untracked :dir dir)
              changes))))
    (list :head head :upstream upstream :ahead ahead :behind behind
          :changes (nreverse changes))))

(defun rvb-feature--parse-diff (text)
  "Parse `git diff --name-status' TEXT into file plists."
  (delq nil
        (mapcar (lambda (line)
                  (let* ((f (split-string line "\t"))
                         (code (car f)))
                    (when (and code (cdr f))
                      (list :status (substring code 0 1)
                            ;; Renames and copies name both paths; the
                            ;; new one is last.
                            :path (car (last f))
                            :orig (and (> (length f) 2) (nth 1 f))))))
                (split-string text "\n" t))))

(defun rvb-feature--parse-commits (text)
  "Parse abbreviated hashes and subjects from tab-separated git log TEXT."
  (mapcar (lambda (line)
            (let ((tab (string-search "\t" line)))
              (list :hash (if tab (substring line 0 tab) line)
                    :subject (if tab (substring line (1+ tab)) ""))))
          (split-string text "\n" t)))

(defun rvb-feature--parse (member output)
  "Merge probe OUTPUT into MEMBER, returning an enriched plist."
  (let* ((dir (plist-get member :dir))
         (parts (mapcar #'string-trim
                        (split-string output
                                      (concat "^" (regexp-quote rvb-feature--sep) "$"))))
         (status (rvb-feature--parse-status (or (nth 0 parts) "") dir))
         (fork (let ((s (nth 1 parts))) (unless (string-empty-p (or s "")) s)))
         (changed (rvb-feature--parse-diff (or (nth 2 parts) "")))
         (commits (rvb-feature--parse-commits (or (nth 3 parts) "")))
         ;; Untracked files never appear in a diff against the base, but
         ;; they are part of what the feature adds.
         (untracked (mapcar (lambda (c) (list :status "?" :path (plist-get c :path)))
                            (cl-remove-if-not
                             (lambda (c) (eq (plist-get c :kind) 'untracked))
                             (plist-get status :changes))))
         (uncommitted (mapcar (lambda (c) (plist-get c :path))
                              (plist-get status :changes))))
    (append member
            status
            (list :fork fork
                  :commits commits
                  :changed (mapcar (lambda (c)
                                     (append c (list :dirty (and (member (plist-get c :path)
                                                                         uncommitted)
                                                                 t))))
                                   (append changed untracked))
                  :probed t))))

(defun rvb-feature--collect (members callback)
  "Probe each of MEMBERS concurrently, then call CALLBACK with the results."
  (if (null members)
      (funcall callback nil)
    (let* ((n (length members))
           (results (make-vector n nil))
           (pending n))
      (cl-loop
       for member in members for i from 0 do
       ;; Bind fresh per iteration: `cl-loop' reuses one binding for the
       ;; iteration variable, which the sentinel closures would share.
       (let ((m member) (idx i))
         (if (plist-get m :missing)
             (progn (aset results idx m) (cl-decf pending))
           (let* ((default-directory (plist-get m :dir))
                  (buf (generate-new-buffer " *rvb-feature-probe*")))
             (make-process
              :name "rvb-feature-probe"
              :buffer buf
              :noquery t
              :connection-type 'pipe
              :command (list shell-file-name shell-command-switch
                             (rvb-feature--probe-script (plist-get m :base)))
              :sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((out (with-current-buffer (process-buffer proc)
                               (buffer-string))))
                    (kill-buffer (process-buffer proc))
                    (aset results idx (rvb-feature--parse m out))
                    (cl-decf pending)
                    (when (zerop pending)
                      (funcall callback (append results nil)))))))))))
      (when (zerop pending)
        (funcall callback (append results nil))))))


;;; Feature list

(defun rvb-feature--relative-time (time)
  "Describe TIME, a Lisp timestamp, relative to now."
  (if (null time)
      "never"
    (let* ((secs (max 0 (floor (float-time (time-subtract nil time)))))
           (days (/ secs 86400)))
      (cond ((< secs 3600) "just now")
            ((< days 1) (format "%dh ago" (/ secs 3600)))
            ((= days 1) "yesterday")
            ((< days 7) (format "%d days ago" days))
            ((< days 14) "last week")
            ((< days 60) (format "%d weeks ago" (/ days 7)))
            (t (format "%d months ago" (/ days 30)))))))

(defun rvb-feature--summary-script (dirs)
  "Return a script reporting the last commit time and dirtiness of DIRS."
  (concat
   (mapconcat
    (lambda (d)
      (let ((q (shell-quote-argument (directory-file-name d))))
        (concat "printf '%s\\t%s\\n' "
                "\"$(git -C " q " log -1 --format=%ct 2>/dev/null)\" "
                "\"$(git -C " q " status --porcelain 2>/dev/null | head -c 1)\"\n")))
    dirs "")
   "exit 0\n"))

(defun rvb-feature--collect-summaries (features callback)
  "Summarise each of FEATURES concurrently, then call CALLBACK with the list.
One process per feature, each reporting every member's last commit time
and whether it is dirty.  Each summary is a plist with :name :repos
:dirty :time :description."
  (if (null features)
      (funcall callback nil)
    (let* ((n (length features))
           (results (make-vector n nil))
           (pending n))
      (cl-loop
       for feature in features for i from 0 do
       (let* ((name feature)
              (idx i)
              (members (cl-remove-if (lambda (m) (plist-get m :missing))
                                     (rvb-feature-members name)))
              (dirs (mapcar (lambda (m) (plist-get m :dir)) members))
              ;; Editing the description counts as working on the feature.
              (org-time (file-attribute-modification-time
                         (file-attributes (rvb-feature--org-file name))))
              (base (list :name name
                          :repos (length members)
                          :description (rvb-feature--summary-description name))))
         (if (null dirs)
             (progn (aset results idx (append base (list :dirty 0 :time org-time)))
                    (cl-decf pending))
           (let ((buf (generate-new-buffer " *rvb-feature-summary*")))
             (make-process
              :name "rvb-feature-summary"
              :buffer buf
              :noquery t
              :connection-type 'pipe
              :command (list shell-file-name shell-command-switch
                             (rvb-feature--summary-script dirs))
              :sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((out (with-current-buffer (process-buffer proc)
                               (buffer-string)))
                        (latest org-time)
                        (dirty 0))
                    (kill-buffer (process-buffer proc))
                    (dolist (line (split-string out "\n" t))
                      (pcase-let ((`(,ct ,flag) (split-string line "\t")))
                        (unless (string-empty-p (or ct ""))
                          (let ((tm (seconds-to-time (string-to-number ct))))
                            (when (or (null latest) (time-less-p latest tm))
                              (setq latest tm))))
                        (unless (string-empty-p (or flag "")) (cl-incf dirty))))
                    (aset results idx
                          (append base (list :dirty dirty :time latest)))
                    (cl-decf pending)
                    (when (zerop pending)
                      (funcall callback (append results nil)))))))))))
      (when (zerop pending)
        (funcall callback (append results nil))))))

(defun rvb-feature--summary-description (feature)
  "Return FEATURE's first description paragraph, fontified as Org."
  (when-let* ((preamble (cdr (assoc nil (rvb-feature--org-sections feature)))))
    ;; One paragraph is enough for a list; the rest is in the status buffer.
    (let ((end (string-match "\n[ \t]*\n" preamble)))
      (if end (substring preamble 0 end) preamble))))

(defun rvb-feature--hint (string)
  "Return STRING with key substitutions applied, faced as a hint.

`substitute-command-keys' marks up key sequences with `help-key-binding'
on the `face' property, which font-lock can replace.  The hint face is
merged underneath first, then the whole lot is moved to
`font-lock-face', so the keys keep their highlighting."
  (let ((s (copy-sequence (substitute-command-keys string))))
    (add-face-text-property 0 (length s) 'rvb-feature-hint t s)
    (rvb-feature--string-faces-to-font-lock s)))

(defun rvb-feature--string-faces-to-font-lock (s)
  "Move S's `face' properties to `font-lock-face', in place.
This preserves faces on text built elsewhere when font-lock redraws it."
  (let ((pos 0))
    (while (< pos (length s))
      (let ((next (or (next-single-property-change pos 'face s) (length s)))
            (val (get-text-property pos 'face s)))
        (when val
          (remove-text-properties pos next '(face nil) s)
          (put-text-property pos next 'font-lock-face val s))
        (setq pos next))))
  s)

(defun rvb-feature--insert-entry (s)
  "Insert summary S as an Org subtree."
  (let ((name (plist-get s :name))
        (start (point)))
    (insert "* " name "  "
            (propertize (rvb-feature--relative-time (plist-get s :time))
                        'font-lock-face 'rvb-feature-count)
            "\n")
    (when-let* ((issue (rvb-feature--issue-line
                        name (rvb-feature--list-redraw))))
      (insert issue "\n"))
    (if-let* ((desc (plist-get s :description)))
        (insert desc "\n")
      (insert (propertize "(no description)"
                          'font-lock-face 'rvb-feature-count)
              "\n"))
    (insert "\n")
    (add-text-properties start (point)
                         (list 'rvb-feature-entry name
                               'rear-nonsticky t))))

(defun rvb-feature--render-list (summaries)
  "Draw SUMMARIES as Org subtrees, most recently worked on first."
  (let ((inhibit-read-only t)
        (entry (rvb-feature--entry-name)))
    ;; The buttons are overlays and outlive the text under them.
    (remove-overlays (point-min) (point-max) 'rvb-feature-button t)
    (erase-buffer)
    (insert "\n" (propertize "Features" 'font-lock-face 'rvb-feature-title)
            "\n\n")
    (if (null summaries)
        (insert (rvb-feature--hint
                 "\\<rvb-feature-list-mode-map>Press \\[rvb-feature-create] to start one.")
                "\n")
      (dolist (s (sort (copy-sequence summaries)
                       (lambda (a b)
                         (let ((ta (plist-get a :time))
                               (tb (plist-get b :time)))
                           (cond ((null ta) nil)
                                 ((null tb) t)
                                 (t (time-less-p tb ta)))))))
        (rvb-feature--insert-entry s)))
    (insert "\n"
            (rvb-feature--hint
             (concat
              "\\<rvb-feature-list-mode-map>"
              "\\[rvb-feature-create] create   "
              "\\[rvb-feature-add-repo] add repo   "
              "\\[rvb-feature-list-visit] open   "
              "\\[rvb-feature-list-refresh] refresh   "
              "\\[rvb-feature-dispatch] more"))
            "\n")
    (set-buffer-modified-p nil)
    (font-lock-flush)
    (font-lock-ensure)
    (goto-char (point-min))
    (when entry
      (let ((pos (point-min)))
        (while (and (< pos (point-max))
                    (not (equal (get-text-property pos 'rvb-feature-entry)
                                entry)))
          (setq pos (or (next-single-property-change
                         pos 'rvb-feature-entry nil (point-max))
                        (point-max))))
        (when (< pos (point-max))
          (goto-char pos))))
    ;; This is a generated dashboard, unlike the editable status page.
    (setq buffer-read-only t)))

(defvar-local rvb-feature--list-generation 0)

(defvar-local rvb-feature--list-state nil
  "Last collected summaries, so an issue lookup can redraw cheaply.")

(defun rvb-feature--list-redraw ()
  "Return a function redrawing this list buffer from its last summaries."
  (let ((buffer (current-buffer)))
    (lambda ()
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when rvb-feature--list-state
            (rvb-feature--render-list rvb-feature--list-state)))))))

(defun rvb-feature-list-refresh ()
  "Recollect and redraw the feature list."
  (interactive)
  (let ((buf (current-buffer))
        (gen (cl-incf rvb-feature--list-generation)))
    (rvb-feature--collect-summaries
     (rvb-feature--names)
     (lambda (summaries)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (= gen rvb-feature--list-generation)
             (setq rvb-feature--list-state summaries)
             (rvb-feature--render-list summaries))))))))

(defun rvb-feature-list-visit ()
  "Open the status buffer for the feature at point."
  (interactive)
  (if-let* ((name (rvb-feature--entry-name)))
      (rvb-feature-status name)
    (user-error "Point is not on a feature")))

(defun rvb-feature--entry-name ()
  "Return the feature name of the entry at point."
  (or (get-char-property (point) 'rvb-feature-entry)
      (and (> (point) (point-min))
           (get-char-property (1- (point)) 'rvb-feature-entry))))

(defun rvb-feature-list-delete ()
  "Delete the feature at point."
  (interactive)
  (if-let* ((name (rvb-feature--entry-name)))
      (progn (rvb-feature-delete name)
             (rvb-feature-list-refresh))
    (user-error "Point is not on a feature")))

(defvar-keymap rvb-feature-list-mode-map
  :parent org-mode-map
  :doc "Keymap for `rvb-feature-list-mode'."
  "g"   #'rvb-feature-list-refresh
  "RET" #'rvb-feature-list-visit
  "a"   #'rvb-feature-add-repo
  "c"   #'rvb-feature-create
  "D"   #'rvb-feature-list-delete
  "?"   #'rvb-feature-dispatch
  "q"   #'quit-window)

(define-derived-mode rvb-feature-list-mode org-mode "Features"
  "Org-based major mode listing every feature."
  :interactive nil
  (setq buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (rvb-feature-list-refresh))))

;;;###autoload
(defun rvb-feature-list-buffer ()
  "Return the feature list buffer, refreshed, without displaying it.
Split out from `rvb-feature-list' so it can serve as an
`initial-buffer-choice', which must return a buffer rather than
display one."
  (let ((buf (get-buffer-create "*features*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'rvb-feature-list-mode)
        (rvb-feature-list-mode))
      (setq default-directory (file-name-as-directory
                               (expand-file-name rvb-feature-directory)))
      (rvb-feature-list-refresh))
    buf))

;;;###autoload
(defun rvb-feature-list ()
  "Show every feature, most recently worked on first.
`RET' opens the status buffer for the feature at point."
  (interactive)
  (pop-to-buffer (rvb-feature-list-buffer)))


;;; Status buffer

(defvar-local rvb-feature--buffer-feature nil
  "Name of the feature this buffer displays.")

(defvar-local rvb-feature--state nil
  "Last collected member state, rendered while a refresh is in flight.")

(defvar-local rvb-feature--generation 0
  "Refresh counter, used to discard results from superseded refreshes.")

;;;; Faces
;;
;; Each inherits from a Magit face, so a theme that styles Magit styles
;; this buffer too, while still allowing per-face overrides.  The hues
;; carry meaning: green is work you have, red is work you lack or have
;; not staged, yellow wants attention, blue is remote-side information.

(defface rvb-feature-title '((t :inherit magit-section-heading))
  "Face for the feature buffer's title."
  :group 'rvb-feature)

(defface rvb-feature-issue '((t :inherit link))
  "Face for a feature's linked issue.
The title is the clickable text, so it is faced like a link."
  :group 'rvb-feature)

(defface rvb-feature-hint '((t :inherit rvb-feature-count))
  "Face for the key hints at the foot of the feature list."
  :group 'rvb-feature)

(defface rvb-feature-branch '((t :inherit magit-branch-local))
  "Face for a member's checked-out branch."
  :group 'rvb-feature)

(defface rvb-feature-count '((t :inherit magit-dimmed))
  "Face for neutral counts and separators."
  :group 'rvb-feature)

(defface rvb-feature-clean '((t :inherit magit-diff-added))
  "Face for the marker on a member with nothing outstanding."
  :group 'rvb-feature)

(defface rvb-feature-dirty '((t :inherit magit-diff-removed))
  "Face for the count of uncommitted changes."
  :group 'rvb-feature)

(defface rvb-feature-ahead '((t :inherit magit-diff-added))
  "Face for commits you have that the upstream does not."
  :group 'rvb-feature)

(defface rvb-feature-behind '((t :inherit magit-branch-remote))
  "Face for commits the upstream has that you do not."
  :group 'rvb-feature)

(defface rvb-feature-conflict '((t :inherit error))
  "Face for unmerged paths."
  :group 'rvb-feature)

(defface rvb-feature-missing '((t :inherit error))
  "Face for a member whose worktree is gone from disk."
  :group 'rvb-feature)

(defface rvb-feature-added '((t :inherit magit-diff-added))
  "Face for files added relative to the base ref."
  :group 'rvb-feature)

(defface rvb-feature-modified '((t :inherit warning))
  "Face for files modified relative to the base ref."
  :group 'rvb-feature)

(defface rvb-feature-deleted '((t :inherit magit-diff-removed))
  "Face for files deleted relative to the base ref."
  :group 'rvb-feature)

(defface rvb-feature-renamed '((t :inherit magit-branch-remote))
  "Face for files renamed or copied relative to the base ref."
  :group 'rvb-feature)

(defface rvb-feature-untracked '((t :inherit magit-dimmed))
  "Face for untracked paths."
  :group 'rvb-feature)

(defface rvb-feature-path '((t :inherit magit-filename))
  "Face for a changed file's path."
  :group 'rvb-feature)

(defun rvb-feature--dirty-count (m)
  (length (plist-get m :changes)))

(defun rvb-feature--conflict-count (m)
  (cl-count-if (lambda (c) (eq (plist-get c :kind) 'unmerged))
               (plist-get m :changes)))

(defun rvb-feature--badges (m)
  "Return the status badges trailing member M's heading."
  (let* ((ahead (or (plist-get m :ahead) 0))
         (behind (or (plist-get m :behind) 0))
         (conflicts (rvb-feature--conflict-count m))
         (uncommitted (> (rvb-feature--dirty-count m) 0)))
    (string-join
     (delq nil
           (list
            (when (> ahead 0)
              (propertize (format "Ahead %d" ahead)
                          'font-lock-face 'rvb-feature-ahead))
            (when (> behind 0)
              (propertize (format "Behind %d" behind)
                          'font-lock-face 'rvb-feature-behind))
            (propertize (if uncommitted
                            "Uncommitted changes"
                          "Clean")
                        'font-lock-face
                        (if uncommitted 'rvb-feature-dirty 'rvb-feature-clean))
            (when (> conflicts 0)
              (propertize (format "Conflicts %d" conflicts)
                          'font-lock-face 'rvb-feature-conflict))))
     "  ")))

(defun rvb-feature--file-face (status)
  "Return the face for a changed-file STATUS letter."
  (pcase status
    ("A" 'rvb-feature-added)
    ("M" 'rvb-feature-modified)
    ("D" 'rvb-feature-deleted)
    ((or "R" "C") 'rvb-feature-renamed)
    ("?" 'rvb-feature-untracked)
    (_ 'rvb-feature-count)))

;;; The status buffer
;;
;; The buffer is the feature's Org file with the generated parts --
;; branch state and changed files -- injected into it.  That makes
;; it an ordinary `org-mode' buffer: whatever styles Org buffers styles
;; it, links work, folding is Org's, and the prose is editable
;; where you read it rather than behind a separate command.
;;
;; Injected text carries `rvb-feature-generated' and is read-only.
;; Saving drops those regions and writes what is left back to the Org
;; file -- so what you typed is the file, and what git said is not.

;; Links use overlay buttons because Org font-lock manages the equivalent
;; text properties.  The read-only and generated markers remain ordinary
;; text properties, which Org leaves alone.

(defun rvb-feature--protect (start end &rest props)
  "Make START..END read-only, adding PROPS.

Protected text is still part of the Org file and is saved with it --
unlike `rvb-feature--generated', which is dropped.  A repository's
heading is protected rather than generated: it is the file's, but
editing it would cut the heading loose from the worktree it names and
orphan everything written under it."
  (add-text-properties
   start end
   (append (list 'read-only t
                 ;; Do not let insertion at the first character evade
                 ;; the protection by landing just before the region.
                 'front-sticky t
                 'rear-nonsticky t)
           props)))

(defun rvb-feature--generated (start end &rest props)
  "Mark START..END as generated, adding PROPS.
Generated text is read-only and is dropped when the buffer is saved,
which is what keeps git's output out of the Org file."
  (apply #'rvb-feature--protect start end 'rvb-feature-generated t props))

(defun rvb-feature--make-link (start end action help)
  "Make START..END a link running ACTION, described by HELP."
  (let ((button (make-button start end
                             'action (lambda (_) (funcall action))
                             'follow-link t
                             'help-echo help
                             'face 'link
                             'mouse-face 'highlight)))
    ;; Buttons are overlays so Org font-lock cannot strip their keymaps.
    (overlay-put button 'rvb-feature-button t)
    button))

(defun rvb-feature--insert-link (label action help)
  "Insert LABEL as a generated link running ACTION, described by HELP."
  (let ((start (point)))
    (insert label)
    (rvb-feature--make-link start (point) action help)))

(defun rvb-feature--repo-branch-string (m)
  "Return member M's branch for its heading."
  (unless (plist-get m :missing)
    (propertize (or (plist-get m :head) (plist-get m :branch) "?")
                'font-lock-face 'rvb-feature-branch)))

(defun rvb-feature--repo-status-string (m)
  "Return member M's state for its heading."
  (cond
   ((plist-get m :missing)
    (propertize "worktree missing" 'font-lock-face 'rvb-feature-missing))
   ((not (plist-get m :probed))
    (propertize "..." 'font-lock-face 'rvb-feature-count))
   (t (rvb-feature--badges m))))

(defun rvb-feature--show-member-diff (m &optional files)
  "Show member M's changes from its base in Magit, limited to FILES."
  (when (plist-get m :missing)
    (user-error "%s is missing from disk" (plist-get m :name)))
  (let ((default-directory (file-name-as-directory (plist-get m :dir)))
        ;; Use the merge-base calculated by the status probe so an
        ;; advancing base ref does not make unrelated upstream work look
        ;; like part of the feature.  Before the probe completes, the
        ;; recorded fork point and configured base are safe fallbacks.
        (base (or (plist-get m :fork)
                  (plist-get m :base-commit)
                  (plist-get m :base)
                  "HEAD")))
    (magit-diff-range base nil files)))

(defun rvb-feature--show-commit (m hash)
  "Show HASH from member M in Magit."
  (when (plist-get m :missing)
    (user-error "%s is missing from disk" (plist-get m :name)))
  (let ((default-directory (file-name-as-directory (plist-get m :dir))))
    (magit-show-commit hash)))

(defun rvb-feature--show-file-diff (m change)
  "Show member M's CHANGE in Magit."
  (let ((path (plist-get change :path)))
    (if (equal (plist-get change :status) "?")
        (let ((default-directory (file-name-as-directory (plist-get m :dir))))
          (magit-diff-paths "/dev/null" (expand-file-name path)))
      (rvb-feature--show-member-diff m (list path)))))

(defun rvb-feature--insert-commits (m)
  "Insert member M's commits as an Org subtree."
  (let ((commits (plist-get m :commits))
        (name (plist-get m :name)))
    (when commits
      (insert "** " (format "Commits (%d)" (length commits)) "\n")
      (dolist (commit commits)
        (let ((start (point))
              (hash (plist-get commit :hash))
              (subject (plist-get commit :subject)))
          (insert "- ")
          (rvb-feature--insert-link
           (format "%s  %s" hash subject)
           (lambda () (rvb-feature--show-commit m hash))
           (format "Show commit %s in Magit" hash))
          (insert "\n")
          (rvb-feature--protect start (point) 'rvb-feature-repo name))))))

(defun rvb-feature--insert-changed (m)
  "Insert the changed-file summary for member M."
  (let ((changed (plist-get m :changed))
        (name (plist-get m :name)))
    (when changed
      (insert "** " (format "Changed files (%d)" (length changed)) "  ")
      (rvb-feature--insert-link
       "View diff"
       (lambda () (rvb-feature--show-member-diff m))
       (format "View %s's diff from %s in Magit"
               name (or (plist-get m :base) "its base")))
      (insert "\n")
      (dolist (c changed)
        (let ((start (point))
              (change c)
              (path (plist-get c :path)))
          (insert "- "
                  (propertize (plist-get c :status) 'font-lock-face
                              (rvb-feature--file-face (plist-get c :status)))
                  " ")
          (rvb-feature--insert-link
           (if (plist-get c :orig)
               (format "%s -> %s" (plist-get c :orig) path)
             path)
           (lambda () (rvb-feature--show-file-diff m change))
           (format "Show %s's diff in Magit" path))
          (when (plist-get c :dirty)
            (insert (propertize " (uncommitted)"
                                'font-lock-face 'rvb-feature-dirty)))
          (insert "\n")
          (rvb-feature--protect start (point) 'rvb-feature-repo name))))))

(defun rvb-feature--insert-header (feature)
  "Insert the generated header for FEATURE below the keyword lines."
  (goto-char (point-min))
  (while (and (not (eobp)) (looking-at "^#\\+"))
    (forward-line))
  (let ((start (point)))
    (when-let* ((issue (rvb-feature--issue-line
                        feature (rvb-feature--status-redraw feature))))
      (insert issue "\n\n"))
    (when (< start (point))
      (rvb-feature--generated start (point)))))

(defun rvb-feature--inject-repo (m &optional gap-before)
  "Inject member M's metadata, adding a visual GAP-BEFORE."
  (let ((name (plist-get m :name)))
    (when (rvb-feature--goto-heading name)
      (let ((heading-start (line-beginning-position)))
        ;; A literal blank line before this heading belongs to the
        ;; preceding Org subtree and disappears when that subtree is
        ;; folded.  An overlay anchored on this heading remains visible.
        (when gap-before
          (let ((gap (make-overlay heading-start (1+ heading-start))))
            (overlay-put gap 'before-string "\n")
            (overlay-put gap 'rvb-feature-layout t)))
        (end-of-line)
        (let ((start (point)))
          (when-let* ((branch (rvb-feature--repo-branch-string m)))
            (insert "  " branch))
          (insert "  " (rvb-feature--repo-status-string m))
          (rvb-feature--generated start (point)))
        (rvb-feature--protect
         heading-start (min (point-max) (1+ (line-end-position)))
         'rvb-feature-repo name)
        (unless (plist-get m :missing)
          (save-excursion
            (goto-char heading-start)
            (when (search-forward name (line-end-position) t)
              (let ((end (point))
                    (dir (plist-get m :dir)))
                (rvb-feature--make-link
                 (- end (length name)) end
                 (lambda () (dired dir))
                 (format "Open %s in Dired" name)))))))
      (when (or (plist-get m :commits) (plist-get m :changed))
        (goto-char (save-excursion
                     (if (re-search-forward "^\\* " nil t)
                         (match-beginning 0)
                       (point-max))))
        (let ((start (point))
              (commit-heading (and (plist-get m :commits) (point))))
          (when (plist-get m :commits)
            (rvb-feature--insert-commits m))
          (when (and (plist-get m :commits) (plist-get m :changed))
            (insert "\n"))
          (when (plist-get m :changed)
            (rvb-feature--insert-changed m))
          (insert "\n")
          (rvb-feature--generated start (point) 'rvb-feature-repo name)
          (when commit-heading
            (save-excursion
              (goto-char commit-heading)
              (org-fold-hide-subtree))))))))

(defun rvb-feature--status-redraw (feature)
  "Return a function redrawing this status buffer from its last probe."
  (let ((buffer (current-buffer)))
    (lambda ()
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when rvb-feature--state
            (rvb-feature--render feature rvb-feature--state)))))))

(defun rvb-feature--render (feature members)
  "Draw FEATURE's Org file with MEMBERS' generated parts injected."
  (let ((inhibit-read-only t)
        (file (rvb-feature--org-file feature))
        (line (line-number-at-pos))
        (column (current-column))
        (window (get-buffer-window (current-buffer))))
    (let ((start (and window (window-start window))))
      ;; Buttons are overlays, and an overlay survives the text under
      ;; it being erased -- they would pile up on every refresh.
      (remove-overlays (point-min) (point-max) 'rvb-feature-button t)
      (remove-overlays (point-min) (point-max) 'rvb-feature-layout t)
      (erase-buffer)
      (when (file-readable-p file)
        (insert-file-contents file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (rvb-feature--insert-header feature)
      (let ((first t))
        (dolist (m members)
          (rvb-feature--inject-repo m (not first))
          (setq first nil)))
      (when (null members)
        (goto-char (point-max))
        (let ((at (point)))
          (insert "\nNo repositories yet.  \\[rvb-feature-add-repo] adds one.\n")
          (rvb-feature--generated at (point))))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (when window
        (set-window-start window (min start (point-max)))))))

(defun rvb-feature--editable-text ()
  "Return the buffer's text with the generated regions removed.
This is what the Org file is made of: everything typed, and nothing
git said."
  (let ((parts nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (or (next-single-property-change pos 'rvb-feature-generated)
                      (point-max))))
        (unless (get-text-property pos 'rvb-feature-generated)
          (push (buffer-substring-no-properties pos next) parts))
        (setq pos next)))
    (string-trim-right (apply #'concat (nreverse parts)))))

(defun rvb-feature-save ()
  "Write what you typed back to the feature's Org file."
  (interactive)
  (let ((feature (or rvb-feature--buffer-feature
                     (user-error "Not a feature status buffer")))
        (text (rvb-feature--editable-text)))
    (write-region (concat text "\n") nil (rvb-feature--ensure-org feature)
                  nil 'quiet)
    (set-buffer-modified-p nil)
    (message "Saved %s" (abbreviate-file-name (rvb-feature--org-file feature)))
    t))

(defun rvb-feature-refresh ()
  "Recollect and redraw this feature's status."
  (interactive)
  (unless rvb-feature--buffer-feature
    (user-error "Not a feature status buffer"))
  ;; A redraw rereads the Org file, so unsaved edits would vanish.
  (when (buffer-modified-p)
    (if (y-or-n-p "Save your edits before refreshing? ")
        (rvb-feature-save)
      (unless (yes-or-no-p "Discard them? ")
        (user-error "Refresh cancelled"))))
  (let* ((feature rvb-feature--buffer-feature)
         (buf (current-buffer))
         (gen (cl-incf rvb-feature--generation))
         (members (rvb-feature-members feature)))
    ;; Every member needs a heading to hang its status on.
    (dolist (m members)
      (rvb-feature--ensure-heading feature (plist-get m :name)))
    ;; Draw what we already know, then replace it when the probes land.
    (rvb-feature--render feature (or rvb-feature--state members))
    (rvb-feature--collect
     members
     (lambda (state)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (and (= gen rvb-feature--generation)
                      (not (buffer-modified-p)))
             (setq rvb-feature--state state)
             (rvb-feature--render feature state))))))))

(defvar-keymap rvb-feature-status-mode-map
  :doc "Keymap for `rvb-feature-status-mode'.

The buffer is editable, so single letters type themselves.  Commands
live behind `C-c C-f', and saving is ordinary."
  "C-c C-f" #'rvb-feature-dispatch
  "C-c C-r" #'rvb-feature-refresh)

(define-derived-mode rvb-feature-status-mode org-mode "Feature"
  "Major mode for a feature: its Org file, with git's answer injected.

Everything not generated is editable and saved back to the Org file
with \\[save-buffer]."
  :interactive nil
  (setq-local revert-buffer-function (lambda (&rest _) (rvb-feature-refresh)))
  ;; So `C-x C-s' saves the feature rather than asking for a file name.
  (add-hook 'write-contents-functions #'rvb-feature-save nil t))


;;;###autoload
(defun rvb-feature-status (feature)
  "Show the status buffer for FEATURE.

This is the only way into a feature, and the prefix argument is how you
move between them:

  outside a feature   \\[rvb-feature-status]      prompt, then open it
  inside a feature    \\[rvb-feature-status]      open the enclosing feature
  inside a feature    \\[universal-argument] \\[rvb-feature-status]  prompt, \
to switch to another

Opening the status buffer sets `default-directory' to the feature
directory, which `rvb/project-try' resolves to the feature -- so
`project-find-file' and friends scope to it from here on.  That is why
there is no separate switch command."
  (interactive
   (list (or (and (not current-prefix-arg) (rvb-feature--enclosing))
             (rvb-feature--read-name t))))
  (let ((buf (get-buffer-create (format "*feature: %s*" feature))))
    (with-current-buffer buf
      (unless (derived-mode-p 'rvb-feature-status-mode)
        (rvb-feature-status-mode))
      (setq rvb-feature--buffer-feature feature
            default-directory (rvb-feature--dir feature))
      (rvb-feature-refresh))
    (pop-to-buffer buf)))


;;; Commands in the status buffer

(defun rvb-feature--member (name)
  "Return the member plist named NAME, from the last probe or from disk."
  (cl-find name (or rvb-feature--state
                    (rvb-feature-members rvb-feature--buffer-feature))
           :key (lambda (m) (plist-get m :name)) :test #'equal))

(defun rvb-feature--section-repo-name ()
  "Return the repository point is inside, or nil.

Generated text says which repository it belongs to outright.  Typed
text does not, so fall back to the nearest repository heading above
point -- which is what Org structure already means."
  (or (get-text-property (point) 'rvb-feature-repo)
      (and rvb-feature--buffer-feature
           (let ((members (rvb-feature--member-names rvb-feature--buffer-feature)))
             (save-excursion
               (catch 'found
                 (while (re-search-backward rvb-feature--top-heading-regexp nil t)
                   (let ((name (rvb-feature--heading-text
                                (substring-no-properties (match-string 1)))))
                     (when (member name members)
                       (throw 'found name))))
                 nil))))))

(defun rvb-feature--section-member ()
  "Return the member plist for the repository point is inside."
  (when-let* ((name (rvb-feature--section-repo-name)))
    (rvb-feature--member name)))

(defun rvb-feature-dired-repo ()
  "Open the worktree of the repository at point in Dired."
  (interactive)
  (let ((m (or (rvb-feature--section-member)
               (user-error "Point is not in a repository"))))
    (if (plist-get m :missing)
        (user-error "%s is missing from disk" (plist-get m :name))
      (dired (plist-get m :dir)))))

(defun rvb-feature-visit-repo ()
  "Open the Magit status of the repository at point."
  (interactive)
  (let ((m (or (rvb-feature--section-member)
               (user-error "Point is not in a repository"))))
    (if (plist-get m :missing)
        (user-error "%s is missing from disk" (plist-get m :name))
      (magit-status-setup-buffer (plist-get m :dir)))))

(defun rvb-feature-diff-repo ()
  "Show changes from the base of the repository at point in Magit."
  (interactive)
  (rvb-feature--show-member-diff
   (or (rvb-feature--section-member)
       (user-error "Point is not in a repository"))))

(defun rvb-feature-remove-repo (&optional feature name)
  "Remove the repository at point from this feature.
Removes its worktree and offers to delete the branch."
  (interactive)
  (let* ((feature (or feature rvb-feature--buffer-feature
                      (rvb-feature--read-name t)))
         (m (or (and name (cl-find name (rvb-feature-members feature)
                                   :key (lambda (x) (plist-get x :name)) :test #'equal))
                (rvb-feature--section-member)
                (user-error "Point is not on a repository")))
         (origin (plist-get m :origin))
         (branch (plist-get m :branch)))
    (unless (yes-or-no-p (format "Remove %s from feature %s? "
                                 (plist-get m :name) feature))
      (user-error "Aborted"))
    (unless (plist-get m :missing)
      (when (and (rvb-feature--dirty-p (plist-get m :dir))
                 (not (yes-or-no-p "Worktree has uncommitted changes; discard them? ")))
        (user-error "Aborted"))
      (rvb-feature--git! (or origin (plist-get m :dir))
                         "worktree" "remove" "--force" (plist-get m :dir)))
    (when (and origin branch (rvb-feature--branch-p origin branch)
               (y-or-n-p (format "Also delete branch %s? " branch)))
      (unless (rvb-feature--git-ok origin "branch" "-d" branch)
        (when (yes-or-no-p (format "%s is not fully merged; delete anyway? " branch))
          (rvb-feature--git! origin "branch" "-D" branch))))
    (rvb-feature--forget-member feature (plist-get m :name))
    (when (derived-mode-p 'rvb-feature-status-mode)
      (setq rvb-feature--state nil)
      (rvb-feature-refresh))
    (message "Removed %s from %s" (plist-get m :name) feature)))

(defun rvb-feature-fetch-all ()
  "Fetch every member repository, then refresh."
  (interactive)
  (let* ((feature (or rvb-feature--buffer-feature (rvb-feature--read-name t)))
         (members (cl-remove-if (lambda (m) (plist-get m :missing))
                                (rvb-feature-members feature)))
         (buf (current-buffer))
         (pending (length members)))
    (when (zerop pending) (user-error "Nothing to fetch"))
    (message "Fetching %d repositories..." pending)
    (dolist (m members)
      (let ((default-directory (plist-get m :dir)))
        (make-process
         :name "rvb-feature-fetch" :buffer nil :noquery t
         :command '("git" "fetch" "--quiet")
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (cl-decf pending)
             (when (zerop pending)
               (message "Fetched all repositories")
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (derived-mode-p 'rvb-feature-status-mode)
                     (rvb-feature-refresh))))))))))))

;;;###autoload
(defun rvb-feature-delete (feature)
  "Remove every worktree in FEATURE and delete its directory."
  (interactive (list (or rvb-feature--buffer-feature (rvb-feature--read-name t))))
  (let* ((members (rvb-feature-members feature))
         (dirty (cl-remove-if-not
                 (lambda (m) (and (not (plist-get m :missing))
                                  (rvb-feature--dirty-p (plist-get m :dir))))
                 members)))
    (unless (yes-or-no-p
             (format "Delete feature %s (%d worktree%s%s)? "
                     feature (length members) (if (= (length members) 1) "" "s")
                     (if dirty (format ", %d with uncommitted changes" (length dirty)) "")))
      (user-error "Aborted"))
    (let ((delete-branches
           (and (cl-some (lambda (m) (and (plist-get m :origin) (plist-get m :branch)))
                         members)
                (y-or-n-p "Also delete the feature branches? "))))
      (dolist (m members)
        (unless (plist-get m :missing)
          (rvb-feature--git (or (plist-get m :origin) (plist-get m :dir))
                            "worktree" "remove" "--force" (plist-get m :dir)))
        (when-let* ((origin (and delete-branches (plist-get m :origin)))
                    (branch (plist-get m :branch)))
          (unless (rvb-feature--git-ok origin "branch" "-d" branch)
            (when (y-or-n-p (format "%s in %s is not fully merged; delete anyway? "
                                    branch (plist-get m :name)))
              (rvb-feature--git origin "branch" "-D" branch))))))
    (delete-directory (rvb-feature--dir feature) t)
    (when-let* ((buf (get-buffer (format "*feature: %s*" feature))))
      (kill-buffer buf))
    (message "Deleted feature %s" feature)))


;;; project.el integration

(defun rvb-feature--project-roots ()
  "Return every feature directory, for `rvb/project-extra-roots-functions'."
  (mapcar #'rvb-feature--dir (rvb-feature--names)))

(with-eval-after-load 'rvb-projects
  (add-to-list 'rvb/project-extra-roots-functions #'rvb-feature--project-roots))


;;; Entry point

;;;###autoload (autoload 'rvb-feature-dispatch "rvb-features" nil t)
(transient-define-prefix rvb-feature-dispatch ()
  "Work on features that span several repositories.

Only commands that make sense from anywhere belong here.  Anything
that reads the section at point -- editing a description, for one --
is bound in `rvb-feature-status-mode-map' instead, where there is a
section to read."
  [["Feature"
    ("c" "Create a feature" rvb-feature-create)
    ("a" "Add a repo to a feature" rvb-feature-add-repo)
    ("s" "Status  (C-u: another feature)" rvb-feature-status)
    ("l" "List all features" rvb-feature-list)]
   ["Issue"
    ("i p" "Pull description from the issue" rvb-feature-issue-pull)
    ("i P" "Push description to the issue" rvb-feature-issue-push)]
   ["Manage"
    ("f" "Fetch all" rvb-feature-fetch-all)
    ("k" "Remove a repo" rvb-feature-remove-repo)
    ("D" "Delete feature" rvb-feature-delete)]])

(provide 'rvb-features)
;;; rvb-features.el ends here
