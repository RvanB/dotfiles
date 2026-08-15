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

(require 'autorevert)
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
(declare-function rvb/github-fetch-issue "rvb-github" (key callback))
(declare-function rvb/github-set-body "rvb-github" (key body callback))
(declare-function rvb/github-create-pr "rvb-github"
                  (dir title body base head callback))
(declare-function rvb/github-pull-request "rvb-github" (dir branch &optional refresh))
(declare-function rvb/github-pull-request-pending-p "rvb-github" (dir branch))
(declare-function rvb/github-forget-pull-request "rvb-github" (dir branch))
(declare-function rvb/github-state-string "rvb-github" (state))

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
feature reads as a single document under two top-level headings:
what the work is, and what it took.

    #+title: Single sign-on for the reports app
    #+issue: https://github.com/cdlib/zephir-reports/issues/42

    * Description
    Single sign-on across the estate.

    ** Open questions
    Which provider?

    * Implementation
    ** auth-service
    Adds the OIDC callback endpoint.

    ** web-ui
    Swaps the login form for a redirect.

Description is the feature's own writing, and the only part that has a
counterpart on GitHub: it is what `rvb-feature-issue-push' sends and
what `rvb-feature-issue-pull' replaces.  Because GitHub's headings
start at level one and Org's here start at level two, the text is
promoted on the way out and demoted on the way back in.

Implementation holds one level-two heading per member repository, and
that is where the status buffer injects each repository's branch,
commits and changed files.  Write anything you like under a
repository's heading; it belongs to that repository, and
`rvb-feature-pr-body' makes it the body of its pull request.

An optional `#+issue:' keyword links the feature to a GitHub issue or
pull request.  It is what `rvb-feature-issue-push' and
`rvb-feature-issue-pull' talk to, and the feature list heads a feature
with the issue's title and state.  See `rvb-feature-issue'.

`#+title:' is free text and drives nothing: a feature is its
directory, and that name is what the branches, the record and the
buffer names are built from.  Pulling an issue writes the issue's
title here, which is what the feature list reads.  See
`rvb-feature-title'.")

(defconst rvb-feature-description-heading "Description"
  "Top-level heading holding a feature's own description.")

(defconst rvb-feature-implementation-heading "Implementation"
  "Top-level heading holding one sub-heading per member repository.")


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

(defun rvb-feature--remote-branch-name (dir ref)
  "Return REF as the remote's own name for it, for GitHub to read.
A base is recorded as the ref it was taken from, usually a
remote-tracking one like `origin/main', which GitHub knows as `main'.
Only when it really is one, though: a local branch called
`release/2.0' keeps every part of its name."
  (if (rvb-feature--git-ok dir "show-ref" "--verify" "--quiet"
                           (concat "refs/remotes/" ref))
      (replace-regexp-in-string "\\`[^/]+/" "" ref)
    ref))

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
since checking out `origin/add-sso' means creating `add-sso'."
  (let ((locals (rvb-feature--git-lines repo "for-each-ref"
                                        "--format=%(refname:short)" "refs/heads"))
        (remotes (rvb-feature--git-lines repo "for-each-ref"
                                         "--format=%(refname:short)" "refs/remotes")))
    (delete-dups
     (append locals
             (delq nil
                   (mapcar (lambda (r)
                             ;; Only what is under a remote: git shortens
                             ;; `refs/remotes/origin/HEAD' to `origin',
                             ;; which is the default branch wearing the
                             ;; remote's name rather than a branch to
                             ;; check out.
                             (when (string-match "\\`[^/]+/\\(.+\\)\\'" r)
                               (let ((short (match-string 1 r)))
                                 (unless (equal short "HEAD") short))))
                           remotes))))))


;;; Features and their records

(defun rvb-feature-default-branch-name (feature _repo)
  "Return the default branch name for FEATURE: the feature's own name.
Every member repository gets the same branch name unless it is given
one of its own, so the feature's name is the thread running through
the worktrees, the branches and the directory alike."
  feature)

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
  "Create or restructure FEATURE's Org file as needed.  Return its path."
  (let ((file (rvb-feature--org-file feature)))
    (if (file-exists-p file)
        (rvb-feature--ensure-structure feature)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "#+title: " feature "\n\n"
                "* " rvb-feature-description-heading "\n\n"
                "* " rvb-feature-implementation-heading "\n")))
    file))

(defconst rvb-feature--top-heading-regexp "^\\* +\\(.*?\\)[ \t]*$"
  "Match a top-level heading line, capturing everything after the star.")

(defconst rvb-feature--repo-heading-regexp "^\\*\\* +\\(.*?\\)[ \t]*$"
  "Match a repository heading, capturing everything after the stars.")

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


;;; Moving around a feature's Org file
;;
;; Two top-level headings carry the structure -- Description and
;; Implementation -- and a repository is a level-two heading under the
;; second.  Everything below navigates by those, so a heading you write
;; yourself is just text no matter where you put it.

(defun rvb-feature--section-end (level)
  "Return the end of the subtree whose heading is on the current line.
LEVEL is that heading's level: the subtree runs to the next heading at
that level or above, or to the end of the buffer."
  (save-excursion
    (end-of-line)
    (if (re-search-forward (format "^\\*\\{1,%d\\} " level) nil t)
        (match-beginning 0)
      (point-max))))

(defun rvb-feature--goto-section (title)
  "Move point to the end of the top-level heading line TITLE.
Return non-nil if there is one.  Matches on the heading's text, so a
heading the status buffer has decorated still counts."
  (goto-char (point-min))
  (let (found)
    (while (and (not found)
                (re-search-forward rvb-feature--top-heading-regexp nil t))
      (when (equal title (rvb-feature--heading-text
                          (substring-no-properties (match-string 1))))
        (setq found t)))
    found))

(defun rvb-feature--section-body (title)
  "Return (START . END) bounding the body of top-level heading TITLE.
The heading line itself is not included.  Nil if there is no such
heading."
  (save-excursion
    (when (rvb-feature--goto-section title)
      (cons (min (point-max) (1+ (line-end-position)))
            (rvb-feature--section-end 1)))))

(defun rvb-feature--shift-headings (text n)
  "Return TEXT with every Org heading moved N levels down, or up if N < 0.

This is what lets the description be level two here and level one on
GitHub: an issue body starts its headings at `#', and nesting them
under Description would otherwise cost a level on every round trip.

Lines inside a block are left alone -- a `#+begin_example' may well
contain a row of asterisks that is not a heading."
  (if (or (zerop n) (null text))
      text
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((in-block nil))
        (while (not (eobp))
          (cond
           (in-block
            (when (looking-at "[ \t]*#\\+end_") (setq in-block nil)))
           ((looking-at "[ \t]*#\\+begin_") (setq in-block t))
           ((looking-at "\\(\\*+\\) ")
            (if (> n 0)
                (insert (make-string n ?*))
              ;; Never past level one: a heading has to stay a heading.
              (delete-char (min (- n) (1- (length (match-string 1))))))))
          (forward-line 1)))
      (buffer-string))))

(defun rvb-feature--restructured (members)
  "Return the current buffer rewritten under the two top-level headings.

MEMBERS names the feature's repositories.  Their headings move under
Implementation, one level down; everything else -- the text above the
first heading and any heading that does not name a repository --
becomes the Description, also one level down, since level one is now
the structure's."
  (goto-char (point-min))
  (let (keywords description implementation)
    ;; The keyword block, blank lines and all, stays at the top.
    (while (and (not (eobp)) (looking-at "^[ \t]*\\(?:#\\+.*\\)?$"))
      (when (looking-at "^[ \t]*#\\+")
        (push (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))
              keywords))
      (forward-line))
    (let ((first (save-excursion
                   (if (re-search-forward "^\\* " nil t)
                       (match-beginning 0)
                     (point-max)))))
      (let ((prose (string-trim (buffer-substring-no-properties (point) first))))
        (unless (string-empty-p prose) (push prose description)))
      (goto-char first))
    (while (re-search-forward rvb-feature--top-heading-regexp nil t)
      (let* ((title (rvb-feature--heading-text
                     (substring-no-properties (match-string 1))))
             (heading-start (line-beginning-position))
             (body-start (min (point-max) (1+ (line-end-position))))
             (end (rvb-feature--section-end 1))
             (body (string-trim (buffer-substring-no-properties body-start end)))
             (whole (string-trim (buffer-substring-no-properties
                                  heading-start end))))
        (goto-char end)
        (cond
         ;; Already-structured sections keep their contents as they are.
         ((equal title rvb-feature-description-heading)
          (unless (string-empty-p body) (push body description)))
         ((equal title rvb-feature-implementation-heading)
          (unless (string-empty-p body) (push body implementation)))
         ((member title members)
          (push (rvb-feature--shift-headings whole 1) implementation))
         (t (unless (string-empty-p whole)
              (push (rvb-feature--shift-headings whole 1) description))))))
    (concat
     (string-join
      (delq nil
            (list (and keywords (string-join (nreverse keywords) "\n"))
                  (concat "* " rvb-feature-description-heading
                          (when description
                            (concat "\n" (string-join (nreverse description)
                                                      "\n\n"))))
                  (concat "* " rvb-feature-implementation-heading
                          (when implementation
                            (concat "\n" (string-join (nreverse implementation)
                                                      "\n\n"))))))
      "\n\n")
     "\n")))

(defun rvb-feature--ensure-structure (feature)
  "Give FEATURE's Org file its Description and Implementation headings.

A file written before those existed is rewritten by
`rvb-feature--restructured', and the original kept beside it with a
tilde appended -- this moves text the user wrote, so there is a way
back."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (unless (and (rvb-feature--goto-section rvb-feature-description-heading)
                     (rvb-feature--goto-section
                      rvb-feature-implementation-heading))
          (copy-file file (concat file "~") t)
          (let ((text (rvb-feature--restructured
                       (rvb-feature--member-names feature))))
            (erase-buffer)
            (insert text)
            (write-region (point-min) (point-max) file nil 'quiet)))))
    file))

(defun rvb-feature--ensure-heading (feature name)
  "Ensure FEATURE's Org file has a repository heading for NAME."
  (let ((file (rvb-feature--ensure-org feature)))
    (with-temp-buffer
      (insert-file-contents file)
      (unless (rvb-feature--goto-heading name)
        (let ((impl (rvb-feature--section-body
                     rvb-feature-implementation-heading)))
          (goto-char (cdr impl))
          (unless (bolp) (insert "\n"))
          (unless (looking-back "\n\n" 2) (insert "\n"))
          (insert "** " name "\n")
          (write-region (point-min) (point-max) file nil 'quiet))))
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

  nil     the body of the Description heading -- the feature's own
          writing, sub-headings and all
  NAME    a repository heading under Implementation naming one of
          MEMBERS, and everything under it

A file that predates the two structural headings is read the old way,
so the feature list is right about it before it has been restructured.

MEMBERS defaults to the feature's member names.  The whole file is
fontified once and sliced up, so a refresh activates `org-mode' a
single time no matter how many repositories the feature has."
  (let ((file (rvb-feature--org-file feature))
        (members (or members (rvb-feature--member-names feature)))
        own result)
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
          (let ((bounds (or (rvb-feature--section-body
                             rvb-feature-description-heading)
                            ;; Not restructured yet: the description is
                            ;; whatever sits above the first heading.
                            (progn
                              (goto-char (point-min))
                              (while (and (not (eobp)) (looking-at "^#\\+"))
                                (forward-line))
                              (cons (point)
                                    (save-excursion
                                      (if (re-search-forward "^\\* " nil t)
                                          (match-beginning 0)
                                        (point-max))))))))
            (setq own (slice (car bounds) (cdr bounds))))
          (when-let* ((impl (rvb-feature--section-body
                             rvb-feature-implementation-heading)))
            (goto-char (car impl))
            (while (re-search-forward rvb-feature--repo-heading-regexp
                                      (cdr impl) t)
              (let* ((name (rvb-feature--heading-text
                            (substring-no-properties (match-string 1))))
                     (body-start (min (point-max) (1+ (line-end-position))))
                     (end (min (cdr impl) (rvb-feature--section-end 2))))
                (goto-char end)
                (when (member name members)
                  (push (cons name (slice body-start end)) result))))))))
    ;; The feature's own writing first, then the repositories.
    (cons (cons nil own) (nreverse result))))

(defun rvb-feature--goto-heading (name)
  "Move point past NAME's repository heading, under Implementation.
Return non-nil if there is one.  Matches on the heading's text, so a
TODO keyword or tags on it make no difference."
  (when-let* ((impl (rvb-feature--section-body
                     rvb-feature-implementation-heading)))
    (goto-char (car impl))
    (let (found)
      (while (and (not found)
                  (re-search-forward rvb-feature--repo-heading-regexp
                                     (cdr impl) t))
        (when (equal name (rvb-feature--heading-text
                           (substring-no-properties (match-string 1))))
          (setq found t)))
      found)))

(defun rvb-feature--set-description (feature name text)
  "Replace repository NAME's section in FEATURE with TEXT.

The inverse of `rvb-feature-description', and the same shape as
`rvb-feature--set-own-text' is to `rvb-feature-own-text': only that one
section is touched, so what the feature says about itself and what its
other repositories say survive a pull into this one."
  (let ((file (rvb-feature--ensure-org feature)))
    (with-temp-buffer
      (insert-file-contents file)
      (unless (rvb-feature--goto-heading name)
        (error "%s has no heading for %s" file name))
      (let ((start (min (point-max) (1+ (line-end-position))))
            (end (rvb-feature--section-end 2)))
        (delete-region start end)
        (goto-char start)
        (insert (string-trim text) "\n\n"))
      (write-region (point-min) (point-max) file nil 'quiet))
    file))

(defun rvb-feature-description (feature name)
  "Return the description text for repository NAME in FEATURE, unfontified."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (when (rvb-feature--goto-heading name)
          (let* ((start (min (point-max) (1+ (line-end-position))))
                 (end (rvb-feature--section-end 2))
                 (s (string-trim (buffer-substring-no-properties start end))))
            (unless (string-empty-p s) s)))))))

;;; The title
;;
;; Nothing here is keyed on the title: a feature is its directory, and
;; that name is what the branches, the record and the buffers are built
;; from.  The title is free text, and `rvb-feature-issue-pull' writes
;; the linked issue's title into it -- so the list can read what the
;; work is called rather than what its branch is called, with no
;; network involved.

(defconst rvb-feature--title-keyword-regexp
  (rx bol (* (any " \t")) "#+title:" (* (any " \t")) (group (* nonl)))
  "Match the `#+title:' keyword in a feature's Org file.")

(defun rvb-feature--preamble-limit ()
  "Return the end of the current buffer's keyword block."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\* " nil t) (match-beginning 0) (point-max))))

(defun rvb-feature-title (feature)
  "Return FEATURE's `#+title:', or nil if it has none of its own.

A title that repeats the directory name says nothing the name does not,
so it counts as none and the caller falls back to the linked issue."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward rvb-feature--title-keyword-regexp
                                 (rvb-feature--preamble-limit) t)
          (let ((s (string-trim (match-string 1))))
            (unless (or (string-empty-p s) (equal s feature)) s)))))))

(defun rvb-feature--set-title (feature title)
  "Set FEATURE's `#+title:' keyword to TITLE."
  (let ((file (rvb-feature--ensure-org feature)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward rvb-feature--title-keyword-regexp
                             (rvb-feature--preamble-limit) t)
          (replace-match (concat "#+title: " title) t t)
        (goto-char (point-min))
        (insert "#+title: " title "\n"))
      (write-region (point-min) (point-max) file nil 'quiet))
    file))


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

(defun rvb-feature-own-text (feature)
  "Return FEATURE's own writing, unfontified: its Description section.

This is what `rvb-feature-issue-push' sends and what
`rvb-feature-issue-pull' replaces.  The repository sections are not
part of it; they have no counterpart on the issue."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (when-let* ((bounds (rvb-feature--section-body
                             rvb-feature-description-heading)))
          (let ((s (string-trim (buffer-substring-no-properties
                                 (car bounds) (cdr bounds)))))
            (unless (string-empty-p s) s)))))))

(defun rvb-feature--set-own-text (feature text)
  "Replace FEATURE's Description with TEXT, keeping everything else.

The inverse of `rvb-feature-own-text'.  Only that one section is
touched, so the Implementation half -- which is not on GitHub and
which pulling knows nothing about -- survives a pull untouched."
  (let ((file (rvb-feature--ensure-org feature)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((bounds (or (rvb-feature--section-body
                         rvb-feature-description-heading)
                        (error "%s has no %s heading" file
                               rvb-feature-description-heading))))
        (delete-region (car bounds) (cdr bounds))
        (goto-char (car bounds))
        (insert (string-trim text) "\n\n"))
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
  "Replace FEATURE's Description section with the body of its linked issue.

Only that section is replaced -- it is exactly what
`rvb-feature-issue-push' sends, which is what makes the two inverse
rather than duplicating your notes on every round trip.  The
Implementation half is left alone; it has no counterpart on GitHub.

The body arrives as Markdown and is converted to Org by Pandoc, then
demoted a level so its headings sit under Description -- the inverse
of the promotion pushing does.

The issue's own title is written to `#+title:' whichever way the body
goes, including when you decline to replace it: the title names the
document, and GitHub is where that name is decided."
  (interactive (list (or rvb-feature--buffer-feature (rvb-feature--read-name t))))
  (let ((key (rvb-feature--issue-or-error feature))
        (current (rvb-feature-own-text feature)))
    (rvb/github-fetch-issue
     key
     (lambda (issue)
       (when issue
         (let ((body (string-trim (or (plist-get issue :body) "")))
               (title (string-trim (or (plist-get issue :title) ""))))
           (unless (string-empty-p title)
             (rvb-feature--set-title feature title))
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
             (rvb-feature--set-own-text
              feature
              (rvb-feature--shift-headings (rvb-feature--from-markdown body) 1))
             (message "Pulled %s into %s" key feature)))
           (rvb-feature--after-issue-sync feature)))))))

;;;###autoload
(defun rvb-feature-issue-push (feature)
  "Set the body of FEATURE's linked issue from its Description section.

Only that section is sent.  The Implementation half is held back --
it is per-repository and belongs in the pull requests, not on the
issue.

The text is promoted a level first, so a heading written under
Description as level two arrives on GitHub as `#', then converted from
Org to GitHub-flavoured Markdown by Pandoc.

This rewrites the issue on GitHub, where other people can see it and
where there is no undo, so it always asks first."
  (interactive (list (or rvb-feature--buffer-feature (rvb-feature--read-name t))))
  (let* ((key (rvb-feature--issue-or-error feature))
         (body (rvb-feature--to-markdown
                (rvb-feature--shift-headings
                 (or (rvb-feature-own-text feature)
                     (user-error "%s has no description to push" feature))
                 -1))))
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

That repository's own section under Implementation, and only that: the
feature's Description belongs to the linked issue, which the pull
request references rather than repeats.

The text is promoted two levels, so a heading written under the
repository -- level three here, since the repository itself is level
two -- arrives at one.  `rvb-feature-create-pr' converts it with
`rvb-feature--to-markdown', the same way pushing an issue does."
  (when-let* ((own (rvb-feature-description feature name)))
    (rvb-feature--shift-headings own -2)))

(defcustom rvb-feature-pr-issue-trailer "Implements %s"
  "Format string naming the feature's issue in a pull request body.

%s is the issue as \"owner/repo#42\", which GitHub renders as a link
from any repository -- so the pull requests of a feature that spans
several all point back at the one issue.

Deliberately a reference and not one of GitHub's closing keywords.  A
keyword, or the linked branch that the Development section is really
made of, means \"merging this closes the issue\" -- which for a feature
is wrong in all but the last repository to merge, and there is no
saying which that will be.  A feature's issue is closed when the
feature is done, by whoever decides that.

Set this to nil for no trailer at all."
  :type '(choice (const :tag "None" nil) string)
  :group 'rvb-feature)

(defun rvb-feature--pr-markdown (feature m)
  "Return the Markdown body of member M's pull request in FEATURE.
`rvb-feature-pr-body' converted, with `rvb-feature-pr-issue-trailer'
naming the feature's issue after it.

The trailer is added to the Markdown rather than to the Org, so it
reaches GitHub as the reference it is rather than as whatever Pandoc
would make of a `#' in prose."
  (let* ((body (when-let* ((org (rvb-feature-pr-body feature (plist-get m :name))))
                 (rvb-feature--to-markdown org)))
         (trailer (rvb-feature--issue-trailer feature)))
    (string-join (delq nil (list body trailer)) "\n\n")))

(defun rvb-feature--issue-trailer (feature)
  "Return the trailer naming FEATURE's issue, or nil."
  (when-let* ((rvb-feature-pr-issue-trailer)
              (issue (rvb-feature-issue feature)))
    (format rvb-feature-pr-issue-trailer issue)))

(defun rvb-feature--without-issue-trailer (markdown feature)
  "Return MARKDOWN without the trailer `rvb-feature--pr-markdown' adds.

Pulling a body back has to undo what pushing it added, or the trailer
would be written into the prose and a second one appended above it on
every round trip.  Matched against the exact line pushing would write,
rather than guessed at, so a sentence of your own that happens to
mention the issue is left alone."
  (let ((text (string-trim (or markdown "")))
        (trailer (rvb-feature--issue-trailer feature)))
    (if (and trailer (string-suffix-p trailer text))
        (string-trim (substring text 0 (- (length text) (length trailer))))
      text)))

(defun rvb-feature--pr-key (pr)
  "Return PR's \"owner/repo#number\", read from its URL.
Which is how the rest of this talks about anything on GitHub, and the
URL is already in hand from the lookup that found the pull request."
  (when-let* ((url (plist-get pr :url)))
    (when (string-match (rx "github.com/"
                            (group (+ (not (any "/")))) "/"
                            (group (+ (not (any "/")))) "/"
                            "pull/" (group (+ digit)))
                        url)
      (format "%s/%s#%s" (match-string 1 url) (match-string 2 url)
              (match-string 3 url)))))

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

BRANCH is always asked for, completing over the repository's local and
remote branches with `rvb-feature-branch-function''s name offered as
the default.  Take the default -- a single RET -- and that branch is
created; name one that already exists, locally or on the remote, and
the worktree checks it out instead, which is how work already under way
somewhere else is pulled into the feature.

BASE is what a branch being created starts from, and defaults to the
repository's default branch.  A prefix argument asks for that too."
  (interactive
   (let* ((guess (when-let* ((top (rvb-feature--toplevel)))
                   (rvb-feature--main-worktree top)))
          (feature (rvb-feature--read-name))
          (chosen (rvb-feature--read-repository guess))
          (repo (or (when-let* ((top (rvb-feature--toplevel chosen)))
                      (rvb-feature--main-worktree top))
                    (user-error "%s is not inside a Git repository"
                                (abbreviate-file-name chosen))))
          (default (funcall rvb-feature-branch-function
                            feature
                            (file-name-nondirectory
                             (directory-file-name repo)))))
     (list feature repo
           ;; The default leads the candidates as well as being the
           ;; default, so the branch about to be created is something
           ;; you can see rather than only something RET does.
           (completing-read (format-prompt "Branch" default)
                            (delete-dups
                             (cons default (rvb-feature--branch-candidates repo)))
                            nil nil nil nil default)
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
       ;; --no-track: BASE is a remote-tracking ref, and git's default
       ;; `branch.autoSetupMerge' would make it the new branch's
       ;; upstream -- so a fresh feature branch would report itself
       ;; ahead of origin/master and offer it as a push target.  The
       ;; base is recorded below and read back by the probe script;
       ;; nothing here needs an upstream to find it.
       (t      (rvb-feature--git! repo "worktree" "add" "--no-track"
                                  "-b" branch worktree base))))
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


;;; Noticing that something changed
;;
;; Neither buffer visits a file -- one is an Org file with git's answer
;; injected, the other is derived from every feature directory at once
;; -- so ordinary Auto Revert has nothing to watch.  `buffer-stale-
;; function' is its hook for exactly this case: answer "has anything
;; changed?" and Auto Revert calls `revert-buffer', which both modes
;; route to their own refresh.
;;
;; Answering means stat, not git.  A commit, checkout, merge or rebase
;; writes the worktree's index and appends to its reflog; a fetch
;; writes FETCH_HEAD; editing a description writes the Org file.  All
;; of those are caught.  Editing a tracked file writes none of them, so
;; the dirty markers still wait for a refresh you ask for -- the
;; alternative is running `git status' over every repository on a
;; five-second timer, which is the cost this avoids.

(defvar-local rvb-feature--signature nil
  "State of the files behind this buffer when it was last drawn.")

(defun rvb-feature--mtime (file)
  "Return FILE's modification time, or nil if it is not there."
  (file-attribute-modification-time (file-attributes file)))

(defun rvb-feature--gitdir (dir)
  "Return the Git directory of worktree DIR.
A linked worktree's `.git' is a file naming the real one, which is
where that worktree's own HEAD, index and reflog live."
  (let ((dot (expand-file-name ".git" dir)))
    (cond
     ((file-directory-p dot) (file-name-as-directory dot))
     ((file-readable-p dot)
      (with-temp-buffer
        (insert-file-contents dot)
        (goto-char (point-min))
        (when (looking-at "gitdir:[ \t]*\\(.*\\)$")
          (file-name-as-directory
           (expand-file-name (string-trim (match-string 1)) dir))))))))

(defun rvb-feature--worktree-signature (dir)
  "Return a value that changes when git's state in worktree DIR does.

The reflog is the reliable half: every commit, checkout, merge, reset
and rebase appends to it.  The index deliberately is not part of this
-- `git status' rewrites it whenever the working tree was touched in
the same second, which is exactly what probing a feature does, so
including it would make every refresh dirty the thing it just read and
Auto Revert would refresh forever."
  (when-let* ((gitdir (rvb-feature--gitdir dir)))
    (mapcar (lambda (f) (rvb-feature--mtime (expand-file-name f gitdir)))
            '("HEAD" "logs/HEAD" "FETCH_HEAD"))))

(defun rvb-feature--status-signature (feature members)
  "Return a value that changes when FEATURE's MEMBERS or Org file do."
  (cons (rvb-feature--mtime (rvb-feature--org-file feature))
        (mapcar (lambda (m)
                  (rvb-feature--worktree-signature (plist-get m :dir)))
                members)))

(defun rvb-feature--list-signature ()
  "Return a value that changes when any feature does."
  (cons (rvb-feature--mtime rvb-feature-directory)
        (mapcar (lambda (name)
                  (cons (rvb-feature--mtime (rvb-feature--record-file name))
                        (rvb-feature--status-signature
                         name (rvb-feature-members name))))
                (rvb-feature--names))))

(defun rvb-feature--status-stale-p (&optional _noconfirm)
  "Return non-nil if this feature's status buffer is out of date.
Never while there are unsaved edits: a redraw rereads the Org file,
and Auto Revert must not be the thing that throws away what you typed."
  (and rvb-feature--buffer-feature
       (not (buffer-modified-p))
       (not (equal rvb-feature--signature
                   (rvb-feature--status-signature
                    rvb-feature--buffer-feature
                    (rvb-feature-members rvb-feature--buffer-feature))))))

(defun rvb-feature--list-stale-p (&optional _noconfirm)
  "Return non-nil if the feature list is out of date."
  (not (equal rvb-feature--signature (rvb-feature--list-signature))))


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

(defun rvb-feature--own-commits-range (m)
  "Return the revision range holding member M's own commits.

Measured from the fork point recorded when the repository was added,
so a busy upstream does not make an untouched feature look like work
in progress.  A worktree added by hand has no record to read, and
falls back to the whole history of its branch."
  (let ((base (or (plist-get m :base-commit) (plist-get m :base))))
    (if base (concat base "..HEAD") "HEAD")))

(defun rvb-feature--summary-script (members)
  "Return a script reporting MEMBERS' last commit time and dirtiness."
  (concat
   (mapconcat
    (lambda (m)
      (let ((q (shell-quote-argument
                (directory-file-name (plist-get m :dir))))
            (range (shell-quote-argument (rvb-feature--own-commits-range m))))
        (concat "printf '%s\\t%s\\n' "
                "\"$(git -C " q " log -1 --format=%ct " range " 2>/dev/null)\" "
                "\"$(git -C " q " status --porcelain 2>/dev/null | head -c 1)\"\n")))
    members "")
   "exit 0\n"))

(defun rvb-feature--collect-summaries (features callback)
  "Summarise each of FEATURES concurrently, then call CALLBACK with the list.

One process per feature, each reporting every member's last commit
time and whether it is dirty.  Each summary is a plist with :name
:repos :dirty :time :description.

:time is when the feature was last committed to, and nil if it never
has been.  Deliberately not the Org file's modification time: that
moves when a repository is added, when the file is restructured, or
when a description is saved, none of which is work on the code."
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
              (base (list :name name
                          :repos (length members)
                          :description (rvb-feature--summary-description name))))
         (if (null members)
             (progn (aset results idx (append base (list :dirty 0 :time nil)))
                    (cl-decf pending))
           (let ((buf (generate-new-buffer " *rvb-feature-summary*")))
             (make-process
              :name "rvb-feature-summary"
              :buffer buf
              :noquery t
              :connection-type 'pipe
              :command (list shell-file-name shell-command-switch
                             (rvb-feature--summary-script members))
              :sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((out (with-current-buffer (process-buffer proc)
                               (buffer-string)))
                        (latest nil)
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
  "Return FEATURE's first description paragraph, fontified as Org.

One paragraph is enough for a list; the rest is in the status buffer.
A heading it opens with is kept: descriptions live under Description,
so their headings start at level two, and level two under the entry's
own heading is what they mean here as much as in the status buffer."
  (when-let* ((body (cdr (assoc nil (rvb-feature--org-sections feature)))))
    (let ((end (string-match "\n[ \t]*\n" body)))
      (if end (substring body 0 end) body))))

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

(defun rvb-feature--entry-heading (name refresh)
  "Return the heading text for feature NAME in the list.

What the work is called, in order of preference: its `#+title:', the
title of the issue it is linked to, its directory name.  The state of
the issue follows either of the first two.  Because
`rvb-feature-issue-pull' writes the issue's title into the file, a
feature that has been pulled reads the same with no network at all.

The heading belongs to the entry, so it carries no keymap of its own:
`RET' and a click open the feature, not the issue in a browser.  The
URL stays in `help-echo' to be read.  REFRESH is called if a pending
issue lookup lands."
  (let* ((key (rvb-feature-issue name))
         (info (and key (fboundp 'rvb/github-lookup)
                    (rvb/github-lookup key refresh)))
         (url (and key (fboundp 'rvb/github-url) (rvb/github-url key)))
         (state (and info (fboundp 'rvb/github-state-string)
                     (rvb/github-state-string (plist-get info :state))))
         (text (or (rvb-feature-title name) (plist-get info :title) name)))
    (rvb-feature--string-faces-to-font-lock
     (concat (apply #'propertize text
                    ;; Faced here rather than left to Org.  Every other
                    ;; word in this buffer is generated text carrying its
                    ;; own `font-lock-face', and an entry's heading is no
                    ;; different -- it is a level-one heading because we
                    ;; wrote it as one, so it can say so itself instead
                    ;; of depending on Org's fontification reaching it.
                    'font-lock-face 'org-level-1
                    (and url (list 'help-echo url)))
             (if (and state (not (string-empty-p state)))
                 (concat "  " (copy-sequence state))
               "")))))

(defun rvb-feature--insert-entry (s)
  "Insert summary S as an Org subtree.

A feature is headed by what the work is called rather than by its
branch name -- see `rvb-feature--entry-heading' -- since the branch
name is recoverable from the status buffer and the directory."
  (let* ((name (plist-get s :name))
         (start (point)))
    (insert "* " (rvb-feature--entry-heading name (rvb-feature--list-redraw))
            "  "
            (propertize (rvb-feature--relative-time (plist-get s :time))
                        'font-lock-face 'rvb-feature-count)
            "\n")
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
    ;; An Org keyword rather than a banner of our own, so this buffer
    ;; opens the way a feature's does.  Faced here for the same reason
    ;; the entries are: everything in this buffer is generated, and
    ;; saying so costs less than depending on Org's fontification.
    (insert (propertize "#+title:" 'font-lock-face 'org-document-info-keyword)
            " "
            (propertize "Features" 'font-lock-face 'org-document-title)
            "\n\n")
    (if (null summaries)
        (insert (rvb-feature--hint
                 "\\<rvb-feature-list-mode-map>\
Press \\[rvb-feature-dispatch] to start one.")
                "\n")
      (dolist (s (sort (copy-sequence summaries)
                       (lambda (a b)
                         (let ((ta (plist-get a :time))
                               (tb (plist-get b :time)))
                           (cond ((null ta) nil)
                                 ((null tb) t)
                                 (t (time-less-p tb ta)))))))
        (rvb-feature--insert-entry s)))
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
    ;; Recorded before collecting, so a change made while the summaries
    ;; are being gathered still counts as one Auto Revert should notice.
    (setq rvb-feature--signature (rvb-feature--list-signature))
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
  "Delete the feature at point.
Not bound to a key: deleting a feature is worth naming out loud, and
the dispatch menu's own entry prompts for which one."
  (interactive)
  (if-let* ((name (rvb-feature--entry-name)))
      (progn (rvb-feature-delete name)
             (rvb-feature-list-refresh))
    (user-error "Point is not on a feature")))

(defvar-keymap rvb-feature-list-mode-map
  :parent org-mode-map
  :doc "Keymap for `rvb-feature-list-mode'.

`RET' acts on the feature at point and `q' buries the buffer.  Nothing
here is editable, so those two letters cost nothing; everything else
lives behind `C-c C-f', the same prefix as in the status buffer, where
a bare letter would type itself instead.

Redrawing is `revert-buffer', which is what reverting a generated
buffer means and is already bound wherever you like it."
  "RET"     #'rvb-feature-list-visit
  "q"       #'quit-window
  "C-c C-f" #'rvb-feature-dispatch)

(define-derived-mode rvb-feature-list-mode org-mode "Features"
  "Org-based major mode listing every feature.

Auto Revert keeps it current: this buffer visits no file, so
`buffer-stale-function' answers for it, and a commit in any member
worktree redraws the list within `auto-revert-interval'.

\\<rvb-feature-list-mode-map>\\[rvb-feature-list-visit] opens the \
feature at point, \\[rvb-feature-dispatch] is the
command menu, and \\[revert-buffer] redraws."
  :interactive nil
  (setq buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (rvb-feature-list-refresh)))
  (setq-local buffer-stale-function #'rvb-feature--list-stale-p)
  (auto-revert-mode 1))

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

(defface rvb-feature-clean '((t :inherit default))
  "Face for the marker on a member with nothing outstanding.
Deliberately plain: nothing outstanding is the quiet case, and it is
the badges that want something doing -- ahead, behind, uncommitted,
conflicts -- that are worth colour.  It still overrides the Org heading
face it sits on, which is why it is `default' rather than no face."
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

(defface rvb-feature-unresolved '((t :inherit warning))
  "Face for the count of unanswered review conversations."
  :group 'rvb-feature)

(defface rvb-feature-missing '((t :inherit error))
  "Face for a member whose worktree is gone from disk."
  :group 'rvb-feature)

;; Adding, modifying and deleting files is what writing code is, not
;; something to be warned about.  The markers say which of the three a
;; file is, and a letter says that on its own.

(defface rvb-feature-added '((t :inherit default))
  "Face for files added relative to the base ref."
  :group 'rvb-feature)

(defface rvb-feature-modified '((t :inherit default))
  "Face for files modified relative to the base ref."
  :group 'rvb-feature)

(defface rvb-feature-deleted '((t :inherit default))
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

(defun rvb-feature--pull-request (m)
  "Return what GitHub says about member M's pull request, or nil.

The lookup is asynchronous, so the answer is nil the first time and the
heading offers to open one.  When it lands the buffer is drawn again --
drawn, not refreshed: nothing about git has changed, and re-probing
every repository because GitHub answered would be a poor trade."
  (when-let* (((fboundp 'rvb/github-pull-request))
              (branch (or (plist-get m :head) (plist-get m :branch)))
              (buf (current-buffer)))
    (rvb/github-pull-request
     (plist-get m :dir) branch
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf (rvb-feature--redraw)))))))

(defun rvb-feature--pr-lookup-pending-p (m)
  "Return non-nil while member M's pull-request lookup is in flight."
  (when-let* (((fboundp 'rvb/github-pull-request-pending-p))
              (branch (or (plist-get m :head) (plist-get m :branch))))
    (rvb/github-pull-request-pending-p (plist-get m :dir) branch)))

(defun rvb-feature--insert-pr-link (m)
  "Insert member M's pull request, or the offer to open one.
A pull request that exists is worth more than a button that would fail:
the heading names it the way GitHub does, `repo#42', and following it
opens it in a browser.  An open one also says how many review
conversations are still unresolved, which is the whole reason to look
at a pull request you have already opened."
  (let ((name (plist-get m :name))
        (pr (rvb-feature--pull-request m)))
    (if (null pr)
        (rvb-feature--insert-link
         "Create PR"
         (lambda () (rvb-feature--create-pr rvb-feature--buffer-feature m))
         (format "Open a pull request for %s" name))
      (let ((url (plist-get pr :url))
            (state (and (fboundp 'rvb/github-state-string)
                        (rvb/github-state-string (plist-get pr :state))))
            (unresolved (or (plist-get pr :unresolved) 0)))
        (rvb-feature--insert-link
         (format "%s#%s" name (plist-get pr :number))
         (lambda () (browse-url url))
         (or (plist-get pr :title) url))
        (unless (string-empty-p (or state ""))
          (insert "  " (rvb-feature--string-faces-to-font-lock
                        (copy-sequence state))))
        ;; Only while it is open: a conversation nobody resolved before
        ;; merging is history, and the heading is about what is left to
        ;; do.
        (when (and (> unresolved 0) (equal (plist-get pr :state) "open"))
          (insert "  " (propertize (format "Unresolved %d" unresolved)
                                   'font-lock-face 'rvb-feature-unresolved)))))))

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
      (insert "*** " (format "Commits (%d)" (length commits)) "\n")
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
      (insert "*** " (format "Changed files (%d)" (length changed)) "  ")
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

(defun rvb-feature--protect-sections ()
  "Protect the structural headings of the feature in this buffer.
Editing one would cut the file loose from the structure everything
else navigates by."
  (dolist (title (list rvb-feature-description-heading
                       rvb-feature-implementation-heading))
    (when (rvb-feature--goto-section title)
      (rvb-feature--protect (line-beginning-position)
                            (min (point-max) (1+ (line-end-position)))))))

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
          (unless (plist-get m :missing)
            (insert "  ")
            (rvb-feature--insert-pr-link m))
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
                 (format "Open %s in Dired" name))))))
        ;; Somewhere to write.  A repository nobody has written under
        ;; yet has its generated block starting where the protected
        ;; heading ends -- two read-only regions meeting, with no
        ;; position between them that will accept a character.  One
        ;; blank line is ordinary text, so it is saved with the rest and
        ;; only has to be added once.
        (let ((body-start (min (point-max) (1+ (line-end-position)))))
          (when (= body-start (rvb-feature--section-end 2))
            (save-excursion
              (goto-char body-start)
              (insert "\n")))))
      (when (or (plist-get m :commits) (plist-get m :changed))
        ;; The end of this repository's subtree: the next repository, or
        ;; the next top-level section.
        (goto-char (rvb-feature--section-end 2))
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
      (rvb-feature--protect-sections)
      (let ((first t))
        (dolist (m members)
          (rvb-feature--inject-repo m (not first))
          (setq first nil)))
      (when (null members)
        (goto-char (point-max))
        (let ((at (point)))
          (insert "\n"
                  (rvb-feature--hint
                   (concat "\\<rvb-feature-status-mode-map>"
                           "No repositories yet.  "
                           "\\[rvb-feature-dispatch] adds one."))
                  "\n")
          (rvb-feature--generated at (point))))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (when window
        (set-window-start window (min start (point-max)))))))

(defun rvb-feature--redraw ()
  "Draw this status buffer again from what is already known.
For a background answer landing -- a pull-request lookup, say -- where
nothing was collected and nothing needs to be: unlike
`rvb-feature-refresh', this rereads no worktree.  Does nothing while
there are unsaved edits, which a redraw would read the file over."
  (when (and (derived-mode-p 'rvb-feature-status-mode)
             rvb-feature--buffer-feature
             (not (buffer-modified-p)))
    (rvb-feature--render
     rvb-feature--buffer-feature
     (or rvb-feature--state
         (rvb-feature-members rvb-feature--buffer-feature)))))

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
  (let* ((feature (or rvb-feature--buffer-feature
                      (user-error "Not a feature status buffer")))
         (text (rvb-feature--editable-text))
         (file (rvb-feature--org-file feature)))
    ;; Deliberately not `rvb-feature--ensure-org': the buffer is the
    ;; file, structural headings and all, and restructuring what is
    ;; about to be overwritten would only make a backup of it.
    (make-directory (file-name-directory file) t)
    (write-region (concat text "\n") nil file nil 'quiet)
    (set-buffer-modified-p nil)
    (message "Saved %s" (abbreviate-file-name file))
    t))

(defun rvb-feature-refresh ()
  "Recollect and redraw this feature's status.

Asking for one by hand also forgets what GitHub said about each
repository's pull request, so that somebody else opening one shows up.
Auto Revert's refreshes do not: they come of committing, which does not
change GitHub's answer, and each one would cost a call per repository."
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
    (when (and (called-interactively-p 'interactive)
               (fboundp 'rvb/github-forget-pull-request))
      (dolist (m members)
        (when-let* ((branch (or (plist-get m :head) (plist-get m :branch))))
          (rvb/github-forget-pull-request (plist-get m :dir) branch))))
    ;; Opening a feature is where a file written before the Description
    ;; and Implementation headings existed gets restructured.
    (rvb-feature--ensure-org feature)
    ;; Every member needs a heading to hang its status on.
    (dolist (m members)
      (rvb-feature--ensure-heading feature (plist-get m :name)))
    ;; Recorded after those writes and before collecting, so writing the
    ;; Org file ourselves does not read back as somebody else's change.
    (setq rvb-feature--signature
          (rvb-feature--status-signature feature members))
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

(defun rvb-feature--protected-at-point-p ()
  "Return non-nil when point sits in protected text.
That is git's half of the buffer -- read-only, so a bare letter has
nothing to type itself into there and can carry a command instead."
  (get-text-property (point) 'read-only))

(defun rvb-feature--protected-key (command)
  "Return a binding running COMMAND, but only in protected text.
Elsewhere the `:filter' declines, and key lookup carries on to the maps
underneath -- which is what leaves the letter typing itself."
  `(menu-item "" ,command
              :filter ,(lambda (cmd)
                         (and (rvb-feature--protected-at-point-p) cmd))))

(defvar-keymap rvb-feature-status-mode-map
  :doc "Keymap for `rvb-feature-status-mode'.

The buffer is editable, so single letters type themselves.  Commands
live behind `C-c C-f'; saving and redrawing are ordinary, with
\\[save-buffer] and \\[revert-buffer].

The exceptions are on the protected, read-only text, where nothing can
be typed anyway: there `g' redraws and `p' opens a pull request for the
repository at point.  Both are `rvb-feature--protected-key' bindings,
so in the prose you actually write the letters are still letters."
  "C-c C-f" #'rvb-feature-dispatch
  "g" (rvb-feature--protected-key #'rvb-feature-refresh)
  "p" (rvb-feature--protected-key #'rvb-feature-create-pr))

(define-derived-mode rvb-feature-status-mode org-mode "Feature"
  "Major mode for a feature: its Org file, with git's answer injected.

Everything not generated is editable and saved back to the Org file
with \\[save-buffer].

Auto Revert keeps git's half current: this buffer visits no file, so
`buffer-stale-function' answers for it, and committing in a member
worktree redraws it.  Never while you have unsaved edits -- a redraw
rereads the Org file."
  :interactive nil
  (setq-local revert-buffer-function (lambda (&rest _) (rvb-feature-refresh)))
  (setq-local buffer-stale-function #'rvb-feature--status-stale-p)
  (auto-revert-mode 1)
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
    ;; In the window you called it from: a status buffer is where you
    ;; work, so it should sit where you were looking rather than take
    ;; the frame or push the code you were reading out of the way.
    (pop-to-buffer-same-window buf)))


;;; Commands in the status buffer

(defun rvb-feature--settle-edits (what &optional destructive)
  "Deal with this buffer's unsaved edits before WHAT.

Saving is offered either way.  DESTRUCTIVE says the edits are about to
be written over rather than merely left out, which is a different thing
to agree to."
  (when (and (derived-mode-p 'rvb-feature-status-mode) (buffer-modified-p))
    (if (y-or-n-p (format "Save your edits before %s? " what))
        (rvb-feature-save)
      (unless (yes-or-no-p (if destructive "Discard them? " "Go on without them? "))
        (user-error "Aborted")))))

(defun rvb-feature--read-member (feature)
  "Prompt for one of FEATURE's repositories."
  (let* ((members (or (rvb-feature-members feature)
                      (user-error "%s has no repositories" feature)))
         (names (mapcar (lambda (m) (plist-get m :name)) members))
         (default (car names)))
    (rvb-feature--member-named
     members
     (completing-read (format-prompt "Repository" default)
                      names nil t nil nil default))))

(defun rvb-feature--member-named (members name)
  "Return the member of MEMBERS called NAME."
  (cl-find name members :key (lambda (m) (plist-get m :name)) :test #'equal))

(defun rvb-feature--target-member ()
  "Return (FEATURE . MEMBER) for a command about one repository.

The repository at point when there is one -- these belong to the status
buffer first -- and otherwise the feature and repository asked for, so
that the same commands work from the dispatch menu."
  (let* ((feature (or rvb-feature--buffer-feature (rvb-feature--read-name t)))
         (m (or (and (equal feature rvb-feature--buffer-feature)
                     (rvb-feature--section-member))
                (rvb-feature--read-member feature))))
    (cons feature m)))

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
                 (while (re-search-backward rvb-feature--repo-heading-regexp nil t)
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

(defun rvb-feature--pr-push-kind (m)
  "Return what M's branch needs pushing before a pull request can exist.
`set' when GitHub has never seen the branch, t when it has commits the
remote does not, nil when there is nothing to send.  Asked of git
rather than read from the last probe, because this decides whether to
push."
  (let ((dir (plist-get m :dir)))
    (cond
     ((not (rvb-feature--git-ok dir "rev-parse" "--verify" "--quiet" "@{u}")) 'set)
     ((not (equal "0" (rvb-feature--git dir "rev-list" "--count" "@{u}..HEAD"))) t))))

(defun rvb-feature--create-pr (feature m)
  "Open a pull request on GitHub for member M of FEATURE.

The body is what you wrote under this repository's heading, converted
to Markdown, so the status buffer is what reviewers read.  The title is
the feature's `#+title:', or its name when it has none.

It ends with `rvb-feature-pr-issue-trailer' naming the feature's
`#+issue:', so a reviewer can get from any of the feature's pull
requests to the one issue -- a reference, which is all it should be:
see that variable for why nothing here asks GitHub to close anything.

That body is read from the Org file rather than from this buffer,
which is why unsaved edits are saved first.

The branch is pushed when it has to be: GitHub will not open a pull
request for a branch it has never seen, and one that is behind what
you have locally would open a pull request missing the work."
  (let* ((name (plist-get m :name))
         (dir (plist-get m :dir))
         (branch (or (plist-get m :head) (plist-get m :branch)))
         (base (rvb-feature--remote-branch-name
                dir (or (plist-get m :base) (rvb-feature--default-base dir))))
         (title (or (rvb-feature-title feature) feature))
         (buf (current-buffer))
         written push)
    (when (plist-get m :missing)
      (user-error "%s is missing from disk" name))
    (when (or (null branch) (equal branch "(detached)"))
      (user-error "%s is not on a branch" name))
    ;; Only what is already known -- the lookup behind this is
    ;; asynchronous, so gh is still the one that decides.  This is for
    ;; the heading that is showing the pull request as this is typed.
    (when-let* ((pr (rvb-feature--pull-request m)))
      (user-error "%s#%s is already open for %s"
                  name (plist-get pr :number) branch))
    (when (equal branch base)
      (user-error "%s is on %s, which is its own base" name branch))
    (rvb-feature--settle-edits "opening the pull request")
    (setq push (rvb-feature--pr-push-kind m)
          written (rvb-feature-pr-body feature name))
    ;; It is on GitHub for other people to see afterwards, so ask before
    ;; rather than undo after -- and say so when there is nothing written
    ;; under the heading, since an empty description is rarely meant.
    (unless (yes-or-no-p
             (format "Open a pull request for %s (%s -> %s)%s%s? "
                     name branch base
                     (if push ", pushing first" "")
                     (if written "" ", with no description")))
      (user-error "Aborted"))
    (when push
      (message "Pushing %s..." branch)
      (apply #'rvb-feature--git! dir "push"
             (when (eq push 'set) (list "--set-upstream" "origin" branch))))
    (rvb/github-create-pr
     dir title (rvb-feature--pr-markdown feature m) base branch
     (lambda (url)
       (when url
         ;; The URL is the one thing gh says that is worth keeping, and
         ;; pasting it under the heading is the usual next move.
         (kill-new url)
         (message "Opened %s (copied)" url)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and (derived-mode-p 'rvb-feature-status-mode)
                        (not (buffer-modified-p)))
               (rvb-feature-refresh)))))))))

(defun rvb-feature-create-pr ()
  "Open a pull request for the repository at point.
The link on each repository's heading runs this too.  See
`rvb-feature--create-pr' for what ends up in it."
  (interactive)
  (let ((target (rvb-feature--target-member)))
    (rvb-feature--create-pr (car target) (cdr target))))

(defun rvb-feature--pr-or-error (m)
  "Return member M's pull request, or explain that there is not one."
  (or (rvb-feature--pull-request m)
      (user-error "No pull request for %s%s" (plist-get m :name)
                  ;; A lookup that has not landed yet reads the same as
                  ;; none at all, and the heading is where you can see
                  ;; which of the two this is.
                  (if (rvb-feature--pr-lookup-pending-p m)
                      " yet -- still asking GitHub"
                    ""))))

;;;###autoload
(defun rvb-feature-pr-pull ()
  "Replace this repository's section with the body of its pull request.

The counterpart of `rvb-feature-pr-push', and the same bargain as
`rvb-feature-issue-pull' makes over the feature's own description: only
this repository's section is replaced, so the rest of the feature is
untouched by a pull into one of its repositories.

The body arrives as Markdown and is converted to Org by Pandoc, then
demoted two levels so its headings sit under the repository -- the
inverse of the promotion pushing does.  The trailer naming the issue is
dropped, being pushing's work rather than yours.

The body is fetched rather than read from what the heading already
knows: a description is worth pulling when somebody has edited it, and
that is exactly what a cached answer would not show."
  (interactive)
  (let* ((target (rvb-feature--target-member))
         (feature (car target))
         (m (cdr target))
         (name (plist-get m :name))
         (key (rvb-feature--pr-key (rvb-feature--pr-or-error m)))
         (current (rvb-feature-description feature name)))
    (unless key
      (user-error "Cannot tell which pull request %s's is" name))
    ;; It is the Org file this writes, and this buffer is showing it.
    (rvb-feature--settle-edits "pulling the description" t)
    (rvb/github-fetch-issue
     key
     (lambda (pr)
       (when pr
         (let ((body (rvb-feature--without-issue-trailer
                      (or (plist-get pr :body) "") feature))
               ;; Compared as Markdown, which is what pushing would
               ;; send: converting the other way and comparing Org would
               ;; call a description unchanged only if Pandoc's blank
               ;; lines happened to land where yours are.
               (mine (or (when-let* ((org (rvb-feature-pr-body feature name)))
                           (rvb-feature--to-markdown org))
                         "")))
           (cond
            ((string-empty-p body)
             (message "%s has an empty body; nothing to pull" key))
            ((equal body mine)
             (message "%s already matches %s" name key))
            ((and current
                  (not (yes-or-no-p
                        (format "Replace %s's section with the body of %s? "
                                name key))))
             (message "Kept what %s says locally" name))
            (t
             (rvb-feature--set-description
              feature name
              (rvb-feature--shift-headings (rvb-feature--from-markdown body) 2))
             (message "Pulled %s into %s" key name)))
           (rvb-feature--after-issue-sync feature)))))))

;;;###autoload
(defun rvb-feature-pr-push ()
  "Set the body of this repository's pull request from its section.

The counterpart of `rvb-feature-pr-pull'.  What is sent is exactly what
opening the pull request would have sent -- this repository's section,
promoted, converted to Markdown, and the issue trailer after it -- so
the two commands stay inverse and a description edited here catches up
with one opened days ago.

This rewrites the pull request on GitHub, where reviewers can see it
and where there is no undo, so it always asks first."
  (interactive)
  (let* ((target (rvb-feature--target-member))
         (feature (car target))
         (m (cdr target))
         (name (plist-get m :name))
         (key (rvb-feature--pr-key (rvb-feature--pr-or-error m))))
    (unless key
      (user-error "Cannot tell which pull request %s's is" name))
    (rvb-feature--settle-edits "pushing the description")
    (let ((body (rvb-feature--pr-markdown feature m)))
      (when (string-empty-p (string-trim body))
        (user-error "%s has no description to push" name))
      (unless (yes-or-no-p
               (format "Replace the body of %s on GitHub with %s's section? "
                       key name))
        (user-error "Aborted"))
      (rvb/github-set-body
       key body
       (lambda (result)
         (when result
           (rvb-feature--after-issue-sync feature)
           (message "Pushed %s's section to %s" name key)))))))

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

Only commands that make sense from anywhere belong here.  The
pull-request commands qualify: they act on the repository at point when
there is one and ask which repository when there is not.  What does not
qualify is anything with no answer to fall back on -- following a link,
say -- which is bound in `rvb-feature-status-mode-map' instead."
  [["Feature"
    ("c" "Create a feature" rvb-feature-create)
    ("a" "Add a repo to a feature" rvb-feature-add-repo)
    ("s" "Status  (C-u: another feature)" rvb-feature-status)
    ("l" "List all features" rvb-feature-list)]
   ["Issue"
    ("i p" "Pull description from the issue" rvb-feature-issue-pull)
    ("i P" "Push description to the issue" rvb-feature-issue-push)]
   ["Pull request"
    ("r c" "Create for a repository" rvb-feature-create-pr)
    ("r p" "Pull description from the PR" rvb-feature-pr-pull)
    ("r P" "Push description to the PR" rvb-feature-pr-push)]
   ["Manage"
    ("f" "Fetch all" rvb-feature-fetch-all)
    ("k" "Remove a repo" rvb-feature-remove-repo)
    ("D" "Delete feature" rvb-feature-delete)]])

(provide 'rvb-features)
;;; rvb-features.el ends here
