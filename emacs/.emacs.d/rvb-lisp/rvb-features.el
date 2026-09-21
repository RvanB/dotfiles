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
(require 'calendar)
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'parse-time)
(require 'transient)

;; Defined in rvb-projects.el, which this module does not require.
(defvar rvb/project-extra-roots-functions)

;; Keep declarations for the Org entry points used below explicit.
(declare-function org-narrow-to-subtree "org" (&optional element))

;; Magit and Org are not loaded with this file -- together they are over
;; a second and a half of startup.  Org arrives with the status buffer,
;; whose mode derives from it; Magit is loaded by both feature modes,
;; whose faces inherit its.
(declare-function magit-diff-range "magit-diff" (rev-or-range &optional args files))
(declare-function magit-diff-paths "magit-diff" (a b))
(declare-function magit-show-commit "magit-diff" (rev &optional args files module))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
(declare-function magit-list-repos "magit-repos" ())
(defvar magit-repository-directories)
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
(declare-function rvb/github-issue-due "rvb-github" (key &optional refresh))
(declare-function rvb/github-lookup "rvb-github" (key &optional refresh))
(declare-function rvb/github-url "rvb-github" (key))
(declare-function rvb/github-issue-expired-p "rvb-github" (key))
(declare-function rvb/github-expire-issue "rvb-github" (key))
(declare-function rvb/github-pull-request-expired-p "rvb-github" (dir branch))

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

(defcustom rvb-feature-list-separator " · "
  "String between the facts on a feature's metadata line.
Change it to \" | \", \" — \" or plain \"  \" to taste; it is only ever
punctuation between fields, so nothing reads it back."
  :type 'string
  :group 'rvb-feature)

(defcustom rvb-feature-fill-column 70
  "Column that prose is filled to in the feature buffers.
Applies to descriptions only."
  :type 'integer
  :group 'rvb-feature)

(defcustom rvb-feature-agent 'codex
  "Command-line coding agent a feature is assigned to.
`rvb-feature-assign-to-agent' runs it non-interactively from the
feature directory, where it can see every member repository."
  :type '(choice (const :tag "Codex CLI" codex)
                 (const :tag "Claude Code" claude))
  :group 'rvb-feature)

(defcustom rvb-feature-agent-prompt
  (concat
   "Implement the feature described in feature.org.  Treat that file as "
   "the authoritative specification.  Inspect all repository worktrees in "
   "this directory, make the required changes in the appropriate "
   "repositories, and run the relevant tests.  Read and follow any "
   "repository-local instructions.  Do not commit.  Finish with a concise "
   "summary of the changes, the verification performed, and any unresolved "
   "questions.")
  "Instructions given to the coding agent for every feature.
The feature name, working directory and path to `feature.org' are
appended automatically, so this can concentrate on how the work
should be done."
  :type 'string
  :group 'rvb-feature)

(defcustom rvb-feature-codex-executable "codex"
  "Codex CLI executable used by `rvb-feature-assign-to-agent'."
  :type 'string
  :group 'rvb-feature)

(defcustom rvb-feature-codex-arguments
  '("exec" "--approve-for-me" "--skip-git-repo-check" "--color" "never")
  "Arguments used to run Codex CLI non-interactively.
The prompt is sent on standard input.  `--approve-for-me' already
means the workspace-write sandbox; a bare \"workspace-write\" here
would be read as the prompt, with standard input appended to it.
The feature directory itself is not a Git repository, which is why
the default includes `--skip-git-repo-check'; its children are the
member worktrees."
  :type '(repeat string)
  :group 'rvb-feature)

(defcustom rvb-feature-claude-executable "claude"
  "Claude Code executable used by `rvb-feature-assign-to-agent'."
  :type 'string
  :group 'rvb-feature)

(defcustom rvb-feature-claude-arguments
  '("--print" "--permission-mode" "auto")
  "Arguments used to run Claude Code non-interactively.
The prompt is sent on standard input."
  :type '(repeat string)
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

An optional `#+due:' keyword says when the work is wanted:

    #+due: <2026-08-21 Fri>

The feature list sorts on it, ahead of the iteration the linked issue
is scheduled in on a project board -- a date written here is a
decision, and the board is only where the default comes from.  It also
means due dates work with no GitHub at all, which the board's do not.
See `rvb-feature-due'.

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
  "Return the names of all active features, sorted.
A directory whose name starts with a dot is not one -- the archive of
closed features lives in one, see `rvb-feature-archive-name'."
  (when (file-directory-p rvb-feature-directory)
    (sort (cl-remove-if-not
           (lambda (n)
             (and (not (string-prefix-p "." n))
                  (file-directory-p (expand-file-name n rvb-feature-directory))))
           (directory-files rvb-feature-directory nil directory-files-no-dot-files-regexp))
          #'string<)))

(defun rvb-feature--enclosing (&optional dir)
  "Return the feature DIR belongs to, or nil."
  (let ((root (file-name-as-directory (expand-file-name rvb-feature-directory)))
        (dir (file-name-as-directory (expand-file-name (or dir default-directory)))))
    (when (string-prefix-p root dir)
      (let ((name (car (split-string (substring dir (length root)) "/" t))))
        (unless (and name (string-prefix-p "." name)) name)))))

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


;;; When a feature is wanted by

(defconst rvb-feature--due-keyword-regexp
  (rx bol (* (any " \t")) "#+due:" (* (any " \t")) (group (+ nonl)))
  "Match the `#+due:' keyword in a feature's Org file.")

(defconst rvb-feature--date-regexp
  (rx (group (= 4 digit)) "-" (group (= 2 digit)) "-" (group (= 2 digit)))
  "Match a calendar date inside a `#+due:' value.")

(defun rvb-feature-due (feature)
  "Return the day FEATURE is wanted by, as a \"YYYY-MM-DD\" string.

Read from a `#+due:' keyword in the feature's Org file, which takes an
Org timestamp or a bare date -- all three of these mean the same day:

    #+due: <2026-08-21 Fri>
    #+due: [2026-08-21]
    #+due: 2026-08-21

The date is picked out of the value rather than the value being
parsed, so an Org timestamp keeps whatever else it carries -- a day
name, a repeater -- without any of it having to be understood here.

Nil when there is no such keyword, or nothing in it looks like a date.
This is what the feature list sorts on when it is set, in preference
to the iteration the linked issue is scheduled in: a date written down
here is a decision, and the board is only where the default comes
from.  See `rvb-feature--entry-before-p'."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward rvb-feature--due-keyword-regexp
                                 (rvb-feature--preamble-limit) t)
          (let ((value (match-string 1)))
            (when (string-match rvb-feature--date-regexp value)
              (match-string 0 value))))))))

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

;;;; Features from your issues

(declare-function rvb/github-assigned-issues "rvb-github" (callback &optional limit))

(defcustom rvb-feature-issue-name-function #'rvb-feature-default-issue-name
  "Function naming the feature made for an issue.
Called with the issue as a plist -- see `rvb/github-assigned-issues' --
and returning a name, which is also every member's branch by default,
so it wants to be short and branch-safe."
  :type 'function
  :group 'rvb-feature)

(defun rvb-feature-default-issue-name (issue)
  "Return \"42-short-title\" for ISSUE.

The number keeps it unique and findable; a few words of the title keep
it readable.  Template tags such as \"[Task]:\" are dropped first --
they are on every issue of their kind and say nothing about this one."
  (let* ((title (downcase (or (plist-get issue :title) "")))
         (title (replace-regexp-in-string
                 "\\`\\(?:[ \t]*\\[[^]]*\\][ \t]*:?\\)+" "" title))
         (words (split-string title "[^[:alnum:]]+" t))
         (slug ""))
    ;; Whole words, up to about forty characters.
    (while (and words
                (<= (+ (length slug) 1 (length (car words))) 40))
      (setq slug (if (string-empty-p slug)
                     (car words)
                   (concat slug "-" (car words)))
            words (cdr words)))
    (if (string-empty-p slug)
        (format "%s" (plist-get issue :number))
      (format "%s-%s" (plist-get issue :number) slug))))

(defun rvb-feature--linked-issues ()
  "Return the issue every existing feature is linked to, as keys."
  (delq nil (mapcar #'rvb-feature-issue (rvb-feature--names))))

(defun rvb-feature--issue-feature-name (issue)
  "Return a feature name for ISSUE that is not taken yet.
Two repositories can both have an issue 42 about much the same thing;
the second is told apart by its repository's name."
  (let ((name (funcall rvb-feature-issue-name-function issue)))
    (if (file-exists-p (rvb-feature--dir name))
        (format "%s-%s"
                (car (last (split-string (plist-get issue :repo) "/")))
                name)
      name)))

(defun rvb-feature--create-from-issue (issue)
  "Create a feature for ISSUE and return its name.

Linked with `#+issue:' and titled after it, and with the issue's body as
its Description when Pandoc is there to convert it -- the same place
`rvb-feature-issue-pull' puts it, so pulling later is a no-op rather
than a surprise.  No repositories: which ones the work touches is still
yours to decide, with `rvb-feature-add-repo'."
  (let* ((feature (rvb-feature--issue-feature-name issue))
         (body (string-trim (or (plist-get issue :body) ""))))
    (make-directory (rvb-feature--dir feature) t)
    (with-temp-file (rvb-feature--org-file feature)
      (insert "#+title: " (or (plist-get issue :title) feature) "\n"
              "#+issue: " (plist-get issue :url) "\n\n"
              "* " rvb-feature-description-heading "\n\n"
              "* " rvb-feature-implementation-heading "\n"))
    (unless (or (string-empty-p body)
                (not (executable-find rvb-feature-pandoc-executable)))
      (rvb-feature--set-own-text
       feature
       (rvb-feature--shift-headings (rvb-feature--from-markdown body) 1)))
    feature))

;;;###autoload
(defun rvb-feature-create-from-assigned-issues ()
  "Create a feature for each open issue assigned to you that has none.

An issue has one when some feature's `#+issue:' already names it,
whatever that feature is called -- so this can be run whenever, and
only ever adds what is new.  Asks before creating anything, listing
what it would create; each is made as `rvb-feature--create-from-issue'
describes, and the feature list is shown afterwards."
  (interactive)
  (unless (fboundp 'rvb/github-assigned-issues)
    (user-error "This needs rvb-github"))
  (message "Asking GitHub for your issues...")
  (rvb/github-assigned-issues
   (lambda (issues)
     (when issues
       (let* ((linked (rvb-feature--linked-issues))
              (new (cl-remove-if (lambda (i) (member (plist-get i :key) linked))
                                 issues)))
         (cond
          ((null new)
           (message "All %d of your open issues already have a feature"
                    (length issues)))
          ((y-or-n-p
            (format "Create %d feature%s for %s? "
                    (length new) (if (cdr new) "s" "")
                    (mapconcat (lambda (i) (plist-get i :key)) new ", ")))
           (let ((created (mapcar #'rvb-feature--create-from-issue new)))
             (rvb-feature-list)
             (message "Created %s" (string-join created ", "))))
          (t (message "Created nothing"))))))))

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
;; of those are caught.  Editing a tracked file writes none of them, and
;; running `git status' over every repository on a five-second timer is
;; the cost this avoids -- so the rest is caught by what does the
;; editing instead:
;;
;; - Saving a file under a feature, or Magit refreshing in one (staging,
;;   discarding, committing), refreshes that feature's views.
;; - Coming back to a view -- selecting its window, or Emacs regaining
;;   focus after work in a terminal -- refreshes it if it has not been
;;   for `rvb-feature-revisit-refresh-interval' seconds.  Nothing else
;;   sees edits made outside Emacs, an agent's among them.
;; - GitHub's answers expire (`rvb/github-cache-ttl'), and an expired
;;   one counts as staleness, since a pull request merged in a browser
;;   changes nothing on disk.

(defvar-local rvb-feature--signature nil
  "State of the files behind this buffer when it was last drawn.")

(defvar-local rvb-feature--refreshed-at nil
  "When this buffer last recollected, as a float time.
Nil when something is known to have changed since.")

;; Made buffer-local further down, with the status buffer.
(defvar rvb-feature--state)

(defcustom rvb-feature-revisit-refresh-interval 10
  "Seconds after which coming back to a feature view refreshes it.
Coming back means selecting a window showing it, or Emacs regaining
focus with it selected.  nil never refreshes on a revisit."
  :type '(choice (const :tag "Never" nil) integer)
  :group 'rvb-feature)

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

(defun rvb-feature--pr-stale-p (members)
  "Return non-nil if what GitHub said about a pull request of MEMBERS expired.
Redrawing is what asks again, the same bargain as
`rvb-feature--github-stale-p' makes for the list."
  (and (fboundp 'rvb/github-pull-request-expired-p)
       (cl-some (lambda (m)
                  (when-let* ((branch (or (plist-get m :head) (plist-get m :branch))))
                    (rvb/github-pull-request-expired-p (plist-get m :dir) branch)))
                members)))

(defun rvb-feature--status-stale-p (&optional _noconfirm)
  "Return non-nil if this feature's status buffer is out of date.
Never while there are unsaved edits: a redraw rereads the Org file,
and Auto Revert must not be the thing that throws away what you typed."
  (and rvb-feature--buffer-feature
       (not (buffer-modified-p))
       (let ((members (rvb-feature-members rvb-feature--buffer-feature)))
         (or (not (equal rvb-feature--signature
                         (rvb-feature--status-signature
                          rvb-feature--buffer-feature members)))
             (rvb-feature--pr-stale-p (or rvb-feature--state members))))))

(defun rvb-feature--github-stale-p ()
  "Return non-nil if what GitHub said about a feature's issue has expired.

Git is not the only thing that can date this list: an issue closed in
a browser changes nothing on disk, and the list is left saying a
feature is still open and sorting it as though it were.  So an answer
past `rvb/github-cache-ttl' counts as staleness too, and Auto Revert
redraws -- which is what asks GitHub again.

Only answers already in hand: a reference nobody has looked up yet is
fetched by the redraw itself, and one whose lookup is in flight is
already on its way, so neither is a reason to redraw again."
  (and (fboundp 'rvb/github-issue-expired-p)
       (cl-some (lambda (name)
                  (when-let* ((key (rvb-feature-issue name)))
                    (rvb/github-issue-expired-p key)))
                (rvb-feature--names))))

(defun rvb-feature--list-stale-p (&optional _noconfirm)
  "Return non-nil if the feature list is out of date."
  (or (not (equal rvb-feature--signature (rvb-feature--list-signature)))
      (rvb-feature--github-stale-p)))

(defun rvb-feature--refresh-quietly ()
  "Recollect the feature view in the current buffer, if that is safe.
A status buffer with unsaved edits is left alone -- a refresh would ask
about them, and nothing that happens by itself should ask anything."
  (cond ((derived-mode-p 'rvb-feature-status-mode)
         (unless (buffer-modified-p) (rvb-feature-refresh)))
        ((derived-mode-p 'rvb-feature-list-mode)
         (rvb-feature-list-refresh))))

(defun rvb-feature--refresh-on-revisit (&rest _)
  "Refresh the selected window's feature view if it has been a while.
For `window-selection-change-functions', `window-buffer-change-functions'
and `after-focus-change-function'."
  (when rvb-feature-revisit-refresh-interval
    (with-current-buffer (window-buffer (selected-window))
      (when (and (derived-mode-p 'rvb-feature-status-mode 'rvb-feature-list-mode)
                 (or (null rvb-feature--refreshed-at)
                     (> (- (float-time) rvb-feature--refreshed-at)
                        rvb-feature-revisit-refresh-interval)))
        (rvb-feature--refresh-quietly)))))

(defun rvb-feature--refresh-on-focus ()
  "Refresh the selected feature view when Emacs regains focus."
  (when (frame-focus-state)
    (rvb-feature--refresh-on-revisit)))

(add-function :after after-focus-change-function #'rvb-feature--refresh-on-focus)

(defvar rvb-feature--touch-timers (make-hash-table :test #'equal)
  "Pending refreshes, by feature, so a burst of saves costs one.")

(defun rvb-feature--refresh-views (feature)
  "Bring FEATURE's status buffer and the feature list up to date.
A view on screen is refreshed now.  One that is not is only marked out
of date, and refreshes when it is next looked at -- no sense probing
every repository for a buffer nobody is reading."
  (dolist (buffer (list (get-buffer (format "*feature: %s*" feature))
                        (get-buffer "*features*")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (get-buffer-window buffer t)
            (rvb-feature--refresh-quietly)
          (setq rvb-feature--refreshed-at nil))))))

(defun rvb-feature--touched (dir)
  "Note that something changed under DIR, refreshing its feature soon."
  (when-let* ((dir)
              (feature (rvb-feature--enclosing dir))
              ((not (gethash feature rvb-feature--touch-timers))))
    (puthash feature
             (run-with-timer
              1 nil
              (lambda ()
                (remhash feature rvb-feature--touch-timers)
                (when (file-directory-p (rvb-feature--dir feature))
                  (rvb-feature--refresh-views feature))))
             rvb-feature--touch-timers)))

(defun rvb-feature--after-save ()
  "Refresh the feature a just-saved file belongs to.
Saving a tracked file is the change the signatures cannot see."
  (when buffer-file-name
    (rvb-feature--touched (file-name-directory buffer-file-name))))

(defun rvb-feature--after-magit-refresh ()
  "Refresh the feature Magit just refreshed a repository of.
Magit refreshes after staging, unstaging, discarding and committing,
which covers the index changes the signatures leave out."
  (rvb-feature--touched default-directory))

(add-hook 'after-save-hook #'rvb-feature--after-save)
(add-hook 'magit-post-refresh-hook #'rvb-feature--after-magit-refresh)


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

(defun rvb-feature--format-date (date)
  "Return DATE, a \"YYYY-MM-DD\" string, as \"30 Sep\".
The year is added only when it is not this one: naming it every time
costs a third of the width to say what is nearly always today's year."
  (when (string-match rvb-feature--date-regexp date)
    (let ((year (string-to-number (match-string 1 date)))
          (month (string-to-number (match-string 2 date)))
          (day (string-to-number (match-string 3 date))))
      (format-time-string
       (if (= year (string-to-number (format-time-string "%Y")))
           "%-d %b" "%-d %b %Y")
       ;; Midday, so no timezone can drag the date onto its neighbour.
       (encode-time 0 0 12 day month year)))))

(defun rvb-feature--days-until (date)
  "Return whole days from today to DATE, a \"YYYY-MM-DD\" string.

Negative once DATE has passed, zero on the day itself.  Counted in
calendar days rather than elapsed hours, so \"tomorrow\" means the next
date on the calendar however late in the evening it is now."
  (when (and date (string-match rvb-feature--date-regexp date))
    (- (calendar-absolute-from-gregorian
        (list (string-to-number (match-string 2 date))
              (string-to-number (match-string 3 date))
              (string-to-number (match-string 1 date))))
       (calendar-absolute-from-gregorian (calendar-current-date)))))

(defun rvb-feature--relative-due (date)
  "Describe DATE, a \"YYYY-MM-DD\" string, relative to today.

Phrased the way `rvb-feature--relative-time' phrases the past, so a
line carrying both reads in one voice: what you want from a list is
how long you have, not which Tuesday it falls on.  Far enough out that
counting weeks stops meaning anything, the date itself says more."
  (when-let* ((days (rvb-feature--days-until date)))
    (cond ((<= days -14) (format "overdue %d weeks" (/ (- days) 7)))
          ((<= days -7)  "overdue a week")
          ((<  days -1)  (format "overdue %d days" (- days)))
          ((=  days -1)  "overdue a day")
          ((=  days 0)   "due today")
          ((=  days 1)   "due tomorrow")
          ((<  days 7)   (format "due in %d days" days))
          ((<  days 14)  "due next week")
          ((<  days 60)  (format "due in %d weeks" (/ days 7)))
          (t (format "due %s" (rvb-feature--format-date date))))))

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
                          :archived (rvb-feature--archived-p name)
                          :description (rvb-feature--card-blurb name))))
         (if (null members)
             (progn (aset results idx
                          (append base
                                  (list :dirty 0
                                        ;; An archived feature's moment is
                                        ;; when it was put away.
                                        :time (rvb-feature--archived-time name))))
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

(defun rvb-feature--feature-info (name refresh)
  "Return what is known about the work feature NAME stands for.

A plist:

  :key :url     its `#+issue:', when it is linked to one
  :title :state what GitHub says about that issue
  :due          the day it is wanted by, as \"YYYY-MM-DD\"
  :due-from     where that day came from: `keyword' or `iteration'
  :iteration    the iteration it came from, when it came from one

:due is the feature's own `#+due:' keyword when it has one, and
otherwise the last day of the iteration its issue is scheduled in.
Written down beats fetched: a date set by hand is a decision, and the
board is only where the default comes from.  A local date takes
:iteration out with it -- the day is no longer that sprint's, and
still naming the sprint would misreport where it came from.

Note that a feature needs no issue to be due: the keyword is read
whether or not there is one, which is what makes due dates work
without the `read:project' scope, or without GitHub at all.

The GitHub lookups are asynchronous and cached, so this is cheap to
call on every redraw; REFRESH is what draws the caller again when one
lands.  Called once per entry and carried on the summary, because both
the heading and the order of the list are decided by it."
  (let* ((key (rvb-feature-issue name))
         (issue (and key (fboundp 'rvb/github-lookup)
                     (rvb/github-lookup key refresh)))
         (iteration (and key (fboundp 'rvb/github-issue-due)
                         (rvb/github-issue-due key refresh)))
         (local (rvb-feature-due name)))
    (append
     (when key
       (list :key key
             :url (and (fboundp 'rvb/github-url) (rvb/github-url key))))
     (list :title (plist-get issue :title)
           :state (plist-get issue :state))
     (if local
         (list :due local :due-from 'keyword)
       (list :due (plist-get iteration :due)
             :due-from (and (plist-get iteration :due) 'iteration)
             :iteration (plist-get iteration :iteration))))))

(defun rvb-feature--state-string (state)
  "Return STATE as a word for the feature list, or nil.

Its own rendering rather than `rvb/github-state-string': that one
answers for pull requests too, where a closed one is a rejection worth
colouring as such, while a closed issue is simply work that is done."
  (pcase state
    ("open"   (propertize "open" 'font-lock-face 'rvb-feature-open))
    ("closed" (propertize "closed" 'font-lock-face 'rvb-feature-done))
    ("merged" (propertize "merged" 'font-lock-face 'rvb-feature-done))))

(defun rvb-feature--due-string (info)
  "Return when INFO's work is wanted, as faced text, or nil.

The iteration it came from follows in brackets when it came from one:
the date is the thing to act on, and the sprint is only where it came
from -- useful to recognise, not the point."
  (when-let* ((due (plist-get info :due))
              (text (rvb-feature--relative-due due)))
    (let ((days (rvb-feature--days-until due))
          (iteration (plist-get info :iteration)))
      (propertize (if iteration (format "%s (%s)" text iteration) text)
                  'font-lock-face
                  (cond ((< days 0) 'rvb-feature-overdue)
                        ((< days 2) 'rvb-feature-due-soon)
                        (t 'rvb-feature-due))))))

(defun rvb-feature--activity-string (time)
  "Return when a feature was last committed to, as faced text."
  (propertize (if time
                  (concat "committed " (rvb-feature--relative-time time))
                "no commits yet")
              'font-lock-face 'rvb-feature-count))

(defun rvb-feature--agent-info (feature)
  "Return (LABEL . PROCESS) while FEATURE's agent is running."
  (when-let* ((process (rvb-feature--agent-process feature))
              ((process-live-p process)))
    (cons (or (process-get process 'rvb-feature-agent-label) "Agent")
          process)))

(defun rvb-feature--insert-agent-status (feature &optional agent)
  "Insert FEATURE's running-agent status and a link to its output.
Return non-nil when an agent is running, nil without inserting
anything otherwise.  AGENT, when non-nil, is the value already
returned by `rvb-feature--agent-info'."
  (when-let* ((agent (or agent (rvb-feature--agent-info feature))))
    (let ((label (car agent))
          (doing (or (process-get (cdr agent) 'rvb-feature-agent-doing)
                     "implementing this feature")))
      (insert (propertize (format "%s is %s" label doing)
                          'font-lock-face 'rvb-feature-agent-running)
              (propertize rvb-feature-list-separator
                          'font-lock-face 'rvb-feature-count))
      (rvb-feature--insert-link
       "Open output"
       (lambda () (rvb-feature-show-agent feature))
       (format "Open %s's output for this feature" label)))
    t))

(defun rvb-feature--entry-closed-p (s)
  "Return non-nil if summary S's issue has been closed.

A feature with no issue, or one GitHub has not answered about yet,
counts as open: work is in progress until something says it is not,
and a lookup still in flight is no reason to bury an entry."
  (or (plist-get s :archived)
      (member (plist-get (plist-get s :info) :state) '("closed" "merged"))))

(defun rvb-feature--entry-before-p (a b)
  "Return non-nil if summary A belongs above summary B in the list.

Three questions in order.  Is it still open -- what is being worked on
comes first, and a feature whose issue is closed has nothing left
wanting attention.  When is it due -- the last day of the iteration
its issue is scheduled in, soonest first, with anything not in an
iteration after everything that is.  When was it last committed to --
most recent first, which is what separates two features due the same
week."
  (let ((closed-a (rvb-feature--entry-closed-p a))
        (closed-b (rvb-feature--entry-closed-p b))
        (due-a (plist-get (plist-get a :info) :due))
        (due-b (plist-get (plist-get b :info) :due))
        (time-a (plist-get a :time))
        (time-b (plist-get b :time)))
    (cond
     ((and closed-a (not closed-b)) nil)
     ((and closed-b (not closed-a)) t)
     ;; ISO dates, so string order is date order.
     ((not (equal due-a due-b))
      (cond ((null due-a) nil)
            ((null due-b) t)
            (t (string< due-a due-b))))
     ((null time-a) nil)
     ((null time-b) t)
     (t (time-less-p time-b time-a)))))

;;;; The timeline
;;
;; The list is drawn as a timeline: a rail down the left with each
;; feature's due date beside it, and the features hung off it as cards,
;; grouped by when they are wanted -- overdue, this week, next week,
;; then month by month, then whatever has no date, then (on request)
;; what is done.  The order within a group is `rvb-feature--entry-
;; before-p''s, so the groups are simply where that order changes.
;;
;; The look is deliberately plain -- a terminal's, not a dashboard's:
;; square corners, no bold, no banner or totals, and dates and counts
;; written short ("09-30", "3d late", "4d ago") rather than spelled out
;; between separators.
;;
;; Cards are drawn with box characters, which is why this buffer is not
;; Org: Org's font-lock hides link and emphasis markup, and a card whose
;; text is narrower on screen than in the buffer has its right border
;; out of line.  Everything is faced with `face' directly -- font-lock
;; has no rules here and never runs, so nothing takes it away.

(defcustom rvb-feature-card-width 72
  "Widest a card in the feature list is drawn, in columns.
A narrower window gets narrower cards."
  :type 'integer
  :group 'rvb-feature)

(defcustom rvb-feature-card-blurb-lines 3
  "Lines of a feature's description shown on its card."
  :type 'integer
  :group 'rvb-feature)

(defface rvb-feature-rail '((t :inherit shadow))
  "Face for the timeline rail and a quiet card's border."
  :group 'rvb-feature)

(defface rvb-feature-card-title '((t :inherit default))
  "Face for the title on a card."
  :group 'rvb-feature)

(defface rvb-feature-bucket '((t :inherit default))
  "Face for the name of a group of cards on the timeline."
  :group 'rvb-feature)

(defconst rvb-feature--gutter 5
  "Columns for the date beside the rail: \"09-30\".")

(defun rvb-feature--short-date (date)
  "Return DATE, a \"YYYY-MM-DD\" string, as \"MM-DD\".
The group a date sits in names the month, and the year when it is not
this one, so the gutter need not."
  (substring date 5))

(defun rvb-feature--short-due (info)
  "Return when INFO's work is wanted, briefly and faced, or nil.
\"3d late\", \"due today\", \"due 9d\" -- the date itself is in
the gutter beside the card.  The iteration follows when there is one."
  (when-let* ((due (plist-get info :due))
              (days (rvb-feature--days-until due)))
    (let ((iteration (plist-get info :iteration)))
      (propertize (concat (cond ((< days 0) (format "%dd late" (- days)))
                                ((= days 0) "due today")
                                (t (format "due %dd" days)))
                          (if iteration (concat "  " iteration) ""))
                  'face (cond ((< days 0) 'rvb-feature-overdue)
                              ((< days 2) 'rvb-feature-due-soon)
                              (t 'rvb-feature-due))))))

(defun rvb-feature--short-ago (time)
  "Return how long ago TIME was, briefly: \"5h ago\", \"3d ago\"."
  (let* ((secs (max 0 (floor (float-time (time-subtract nil time)))))
         (days (/ secs 86400)))
    (cond ((< secs 3600) "just now")
          ((< days 1) (format "%dh ago" (/ secs 3600)))
          ((< days 14) (format "%dd ago" days))
          ((< days 60) (format "%dw ago" (/ days 7)))
          ((< days 365) (format "%dmo ago" (/ days 30)))
          (t (format "%dy ago" (/ days 365))))))

(defun rvb-feature--as-face (s)
  "Return a copy of S with its `font-lock-face' properties moved to `face'.
The shared helpers face their text for font-lock buffers; this one has
no font-lock to honour that.  Nil stays nil, so an absent part of a
line is still absent."
  (when s
   (let ((s (copy-sequence s)) (pos 0))
    (while (< pos (length s))
      (let ((next (or (next-single-property-change pos 'font-lock-face s)
                      (length s)))
            (val (get-text-property pos 'font-lock-face s)))
        (when val
          (remove-text-properties pos next '(font-lock-face nil) s)
          (add-face-text-property pos next val t s))
        (setq pos next)))
    s)))

(defun rvb-feature--card-blurb (feature)
  "Return the opening of FEATURE's description as one line of plain text.

Prose only: headings, keywords, drawers and tables are dropped and link
markup is reduced to what it says, since a card has room for a few
sentences and none for structure.  Nil when there is nothing written."
  (when-let* ((text (rvb-feature-own-text feature)))
    (let* ((prose (cl-remove-if
                   (lambda (line)
                     (string-match-p
                      "\\`[ \t]*\\(?:\\*+ \\|#\\+\\|:[[:alnum:]_-]+:\\||\\)" line))
                   (split-string text "\n")))
           (s (mapconcat #'string-trim prose " ")))
      (setq s (replace-regexp-in-string
               "\\[\\[\\(?:[^]]*\\]\\[\\)?\\([^]]*\\)\\]\\]" "\\1" s))
      (setq s (string-trim (replace-regexp-in-string "[ \t]+" " " s)))
      (unless (string-empty-p s) s))))

(defun rvb-feature--wrap (text width max-lines)
  "Break TEXT into lines at most WIDTH wide, no more than MAX-LINES.
Text that does not fit ends in an ellipsis."
  (let ((words (split-string text " " t)) lines)
    (while (and words (< (length lines) max-lines))
      (let ((line (pop words)))
        (when (> (string-width line) width)
          (setq line (truncate-string-to-width line (1- width) nil nil "…")))
        (while (and words (<= (+ (string-width line) 1 (string-width (car words)))
                              width))
          (setq line (concat line " " (pop words))))
        (push line lines)))
    (when (and words lines (not (string-suffix-p "…" (car lines))))
      (let ((last (car lines)))
        (setcar lines (concat (if (< (string-width last) width)
                                  last
                                (truncate-string-to-width last (1- width)))
                              "…"))))
    (nreverse lines)))

(defun rvb-feature--fit (s width)
  "Return S cut down to WIDTH columns, with an ellipsis if it was cut."
  (if (> (string-width s) width)
      (truncate-string-to-width s width nil nil "…")
    s))

(defun rvb-feature--date-in (days)
  "Return the date DAYS from today, as \"YYYY-MM-DD\"."
  (pcase-let ((`(,month ,day ,year)
               (calendar-gregorian-from-absolute
                (+ (calendar-absolute-from-gregorian (calendar-current-date))
                   days))))
    (format "%04d-%02d-%02d" year month day)))

(defun rvb-feature--bucket (s)
  "Return (KEY . LABEL) for the group summary S belongs in on the timeline.

Weeks run Monday to Sunday.  Beyond next week the month is what
matters, so a group is a month; the year is named only when it is not
this one."
  (let* ((info (plist-get s :info))
         (due (plist-get info :due))
         (days (and due (rvb-feature--days-until due)))
         ;; Days from today to this Sunday: 0 on a Sunday.
         (to-sunday (mod (- 7 (string-to-number (format-time-string "%w"))) 7)))
    (cond
     ((rvb-feature--entry-closed-p s) '(done . "DONE"))
     ((null days) '(none . "NO DUE DATE"))
     ((< days 0) '(overdue . "OVERDUE"))
     ((<= days to-sunday)
      (cons 'this-week
            (format "THIS WEEK %s..%s"
                    (rvb-feature--short-date (rvb-feature--date-in (- to-sunday 6)))
                    (rvb-feature--short-date (rvb-feature--date-in to-sunday)))))
     ((<= days (+ to-sunday 7))
      (cons 'next-week
            (format "NEXT WEEK %s..%s"
                    (rvb-feature--short-date (rvb-feature--date-in (+ to-sunday 1)))
                    (rvb-feature--short-date (rvb-feature--date-in (+ to-sunday 7))))))
     (t
      (let ((month (substring due 0 7)))
        (cons (intern month)
              (upcase
               (format-time-string
                (if (string-prefix-p (format-time-string "%Y") month) "%B" "%B %Y")
                (encode-time 0 0 12 1 (string-to-number (substring due 5 7))
                             (string-to-number (substring due 0 4)))))))))))

(defun rvb-feature--card-border-face (s)
  "Return the face for summary S's card border: its urgency, at a glance."
  (let* ((due (plist-get (plist-get s :info) :due))
         (days (and due (rvb-feature--days-until due))))
    (cond ((rvb-feature--entry-closed-p s) 'rvb-feature-rail)
          ((and days (< days 0)) 'rvb-feature-overdue)
          ((and days (< days 2)) 'rvb-feature-due-soon)
          ((rvb-feature--agent-info (plist-get s :name)) 'rvb-feature-agent-running)
          (t 'rvb-feature-rail))))

(defun rvb-feature--card-meta (s)
  "Return the line under summary S's title: when, where and how lately.
Parts are set two spaces apart, like columns, rather than between dots."
  (let* ((info (plist-get s :info))
         (repos (plist-get s :repos))
         (dirty (plist-get s :dirty))
         (time (plist-get s :time))
         (dim (lambda (text) (propertize text 'face 'rvb-feature-count))))
    (string-join
     (delq nil
           (if (plist-get s :archived)
               (list (rvb-feature--as-face
                      (rvb-feature--state-string (plist-get info :state)))
                     (funcall dim (concat "archived " (rvb-feature--short-ago time))))
             (list (and (rvb-feature--entry-closed-p s)
                        (rvb-feature--as-face
                         (rvb-feature--state-string (plist-get info :state))))
                   (unless (rvb-feature--entry-closed-p s)
                     (rvb-feature--short-due info))
                   (funcall dim (pcase repos
                                  (0 "no repos")
                                  (1 "1 repo")
                                  (_ (format "%d repos" repos))))
                   (and dirty (> dirty 0)
                        (propertize (format "%d dirty" dirty) 'face 'rvb-feature-dirty))
                   (funcall dim (if time (rvb-feature--short-ago time) "no commits")))))
     "  ")))

(defun rvb-feature--insert-rail-line (gutter mark &optional rest)
  "Insert one line of the timeline: GUTTER, the rail as MARK, then REST."
  (insert (propertize (string-pad (or gutter "") rvb-feature--gutter nil t)
                      'face 'rvb-feature-count)
          " "
          (propertize mark 'face 'rvb-feature-rail)
          (or rest "")
          "\n"))

(defun rvb-feature--insert-card (s width)
  "Insert summary S as a card WIDTH columns wide, hung off the rail."
  (let* ((name (plist-get s :name))
         (info (plist-get s :info))
         (closed (rvb-feature--entry-closed-p s))
         (inner (- width 4))
         (border (rvb-feature--card-border-face s))
         (edge (lambda (text) (propertize text 'face border)))
         (row (lambda (content &optional left)
                (concat (funcall edge (or left "│ "))
                        content
                        (make-string (max 0 (- inner (string-width content))) ?\s)
                        (funcall edge " │"))))
         (key (plist-get info :key))
         (ref (and key (replace-regexp-in-string "\\`[^/]+/" "" key)))
         (title (or (rvb-feature-title name) (plist-get info :title) name))
         (due (plist-get info :due))
         (gutter (and due (not closed) (rvb-feature--short-date due)))
         (blurb (plist-get s :description))
         (agent (rvb-feature--agent-info name))
         (start (point)))
    (rvb-feature--insert-rail-line
     nil "│" (concat "  " (funcall edge (concat "┌" (make-string (- width 2) ?─) "┐"))))
    ;; The title line, hung off the rail at the card's date.
    (let* ((ref-text (if ref (concat "  " ref) ""))
           (title-text (rvb-feature--fit title (- inner (string-width ref-text))))
           (content (concat (propertize title-text 'face 'rvb-feature-card-title
                                        'rvb-feature-title-line t)
                            (make-string (max 0 (- inner (string-width title-text)
                                                   (string-width ref-text)))
                                         ?\s)
                            (propertize ref-text 'face 'rvb-feature-count
                                        'help-echo (plist-get info :url)))))
      ;; Plugged into the rail: ├──┤.
      (rvb-feature--insert-rail-line
       gutter "├"
       (concat (propertize "──" 'face 'rvb-feature-rail) (funcall row content "┤ "))))
    (rvb-feature--insert-rail-line
     nil "│" (concat "  " (funcall row (rvb-feature--fit (rvb-feature--card-meta s) inner))))
    (when blurb
      (dolist (line (rvb-feature--wrap blurb inner rvb-feature-card-blurb-lines))
        (rvb-feature--insert-rail-line nil "│" (concat "  " (funcall row line)))))
    (when agent
      (let* ((doing (or (process-get (cdr agent) 'rvb-feature-agent-doing)
                        "implementing this feature"))
             (link "[output]")
             (text (rvb-feature--fit (format "%s: %s" (car agent) doing)
                                     (- inner (string-width link) 2))))
        (rvb-feature--insert-rail-line
         nil "│"
         (concat "  " (funcall row (concat (propertize text 'face 'rvb-feature-agent-running)
                                           "  "
                                           (propertize link 'rvb-feature-agent-link t)))))))
    (rvb-feature--insert-rail-line
     nil "│" (concat "  " (funcall edge (concat "└" (make-string (- width 2) ?─) "┘"))))
    ;; The output link is a button, which is an overlay, made once the
    ;; text it sits on is in the buffer.
    (save-excursion
      (goto-char start)
      (when-let* ((match (text-property-search-forward 'rvb-feature-agent-link t t)))
        (rvb-feature--make-link (prop-match-beginning match) (prop-match-end match)
                                (lambda () (rvb-feature-show-agent name))
                                "Open the agent's output for this feature")))
    (when closed
      (let ((dim (make-overlay start (point))))
        (overlay-put dim 'face 'rvb-feature-closed)
        (overlay-put dim 'rvb-feature-layout t)))
    (add-text-properties start (point) (list 'rvb-feature-entry name))))

(defun rvb-feature--insert-bucket (label count width &optional first)
  "Insert the heading of a group of COUNT cards called LABEL.
FIRST is the top of the timeline, where the rail starts rather than
running on from above."
  (let* ((text (format " %s (%d) " label count))
         (rule (max 2 (- (+ width 1) (string-width text)))))
    (unless first
      (rvb-feature--insert-rail-line nil "│"))
    (rvb-feature--insert-rail-line
     nil "├"
     (concat (propertize "─" 'face 'rvb-feature-rail)
             (propertize text 'face (if (string-prefix-p "OVERDUE" label)
                                        '(rvb-feature-overdue rvb-feature-bucket)
                                      'rvb-feature-bucket))
             (propertize (make-string rule ?─) 'face 'rvb-feature-rail)))))

(defun rvb-feature--card-width ()
  "Return how wide cards are drawn, for the window showing the list."
  (let ((window (get-buffer-window (current-buffer) t)))
    (max 30 (min rvb-feature-card-width
                 (- (if window (window-body-width window) 80)
                    rvb-feature--gutter 5)))))

(defvar-local rvb-feature--list-show-closed nil
  "Whether the feature list shows closed features.
See `rvb-feature--entry-closed-p' for what closed means.")

(defvar-local rvb-feature--list-width nil
  "The card width the list was last drawn at.")

;; Made buffer-local further down, with the rest of the list's state.
(defvar rvb-feature--list-state)

(defun rvb-feature--render-list (summaries)
  "Draw SUMMARIES as cards on a timeline, grouped by when they are due.

Each summary is given its `rvb-feature--feature-info' as :info first,
since the card, its group and the order are all read from it.  The
GitHub half of that is asynchronous, so the first draw of a session
places features by what is written down locally and draws again as
GitHub answers.

Closed features are left out unless `rvb-feature--list-show-closed'
says otherwise, and are then drawn dimmed, last, under Done."
  (let* ((inhibit-read-only t)
         (entry (rvb-feature--entry-name))
         (redraw (rvb-feature--list-redraw))
         (width (rvb-feature--card-width))
         (sorted (sort (mapcar
                        (lambda (s)
                          (append (list :info
                                        (if (plist-get s :archived)
                                            (rvb-feature--archived-info
                                             (plist-get s :name))
                                          (rvb-feature--feature-info
                                           (plist-get s :name) redraw)))
                                  s))
                        summaries)
                       #'rvb-feature--entry-before-p))
         (closed (+ (cl-count-if #'rvb-feature--entry-closed-p sorted)
                    ;; Archived features are only collected when shown.
                    (if rvb-feature--list-show-closed
                        0
                      (length (rvb-feature--archived-names)))))
         (shown (if rvb-feature--list-show-closed
                    sorted
                  (cl-remove-if #'rvb-feature--entry-closed-p sorted))))
    (setq rvb-feature--list-width width)
    ;; The buttons and the dimming are overlays, and outlive the text
    ;; under them.
    (remove-overlays (point-min) (point-max) 'rvb-feature-button t)
    (remove-overlays (point-min) (point-max) 'rvb-feature-layout t)
    (erase-buffer)
    (if (null summaries)
        (insert " " (rvb-feature--as-face
                     (rvb-feature--hint
                      "\\<rvb-feature-list-mode-map>\
Press \\[rvb-feature-dispatch] to start one, or \
\\[rvb-feature-create-from-assigned-issues] for one per issue assigned to you.")))
      ;; Consecutive summaries in the same group share its heading; the
      ;; order already keeps each group together.
      (let ((first t))
       (while shown
        (let* ((bucket (rvb-feature--bucket (car shown)))
               (group (cl-loop for s in shown
                               while (equal (rvb-feature--bucket s) bucket)
                               collect s)))
          (rvb-feature--insert-bucket (cdr bucket) (length group) width first)
          (setq first nil)
          (dolist (s group)
            (rvb-feature--insert-rail-line nil "│")
            (rvb-feature--insert-card s width))
          (setq shown (nthcdr (length group) shown)))))
      (when (> closed 0)
        (insert "\n"
                (rvb-feature--as-face
                 (rvb-feature--hint
                  (concat "\\<rvb-feature-list-mode-map>"
                          (if rvb-feature--list-show-closed
                              "[\\[rvb-feature-list-toggle-closed]] hide closed"
                            (format "[\\[rvb-feature-list-toggle-closed]] show %d closed"
                                    closed)))))
                "\n")))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    ;; Back to the card point was on, or the first one.
    (let ((match (save-excursion
                   (cl-loop for m = (text-property-search-forward
                                     'rvb-feature-title-line t t)
                            while m
                            when (or (null entry)
                                     (equal (get-text-property (prop-match-beginning m)
                                                               'rvb-feature-entry)
                                            entry))
                            return m))))
      (when match (goto-char (prop-match-beginning match))))))

(defun rvb-feature-list-next (&optional n)
  "Move to the next card, or the Nth."
  (interactive "p")
  (dotimes (_ (abs (or n 1)))
    (let ((match (if (< (or n 1) 0)
                     (progn (beginning-of-line)
                            (text-property-search-backward 'rvb-feature-title-line t t))
                   (end-of-line)
                   (text-property-search-forward 'rvb-feature-title-line t t))))
      (if match
          (goto-char (prop-match-beginning match))
        (user-error "No more features")))))

(defun rvb-feature-list-previous (&optional n)
  "Move to the previous card, or the Nth."
  (interactive "p")
  (rvb-feature-list-next (- (or n 1))))

(defun rvb-feature--list-resized (&rest _)
  "Redraw the list when its window's width changes how wide cards are."
  (when (and rvb-feature--list-state
             (not (equal rvb-feature--list-width (rvb-feature--card-width))))
    (rvb-feature--render-list rvb-feature--list-state)))

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
  "Recollect and redraw the feature list.
Asked for by hand, GitHub is asked again about every feature's issue
too, rather than waiting for `rvb/github-cache-ttl'."
  (interactive)
  (let ((buf (current-buffer))
        (gen (cl-incf rvb-feature--list-generation)))
    (when (and (called-interactively-p 'interactive)
               (fboundp 'rvb/github-expire-issue))
      (dolist (name (rvb-feature--names))
        (when-let* ((key (rvb-feature-issue name)))
          (rvb/github-expire-issue key))))
    ;; Recorded before collecting, so a change made while the summaries
    ;; are being gathered still counts as one Auto Revert should notice.
    (setq rvb-feature--signature (rvb-feature--list-signature)
          rvb-feature--refreshed-at (float-time))
    (rvb-feature--collect-summaries
     (append (rvb-feature--names)
             (and rvb-feature--list-show-closed (rvb-feature--archived-names)))
     (lambda (summaries)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (= gen rvb-feature--list-generation)
             (setq rvb-feature--list-state summaries)
             (rvb-feature--render-list summaries))))))))

(defun rvb-feature-list-toggle-closed ()
  "Show or hide closed and archived features in the list."
  (interactive)
  (setq rvb-feature--list-show-closed (not rvb-feature--list-show-closed))
  ;; Recollected rather than redrawn: archived features are only
  ;; collected while they are shown.
  (rvb-feature-list-refresh)
  (message (if rvb-feature--list-show-closed
               "Showing closed features"
             "Hiding closed features")))

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
  :parent special-mode-map
  :doc "Keymap for `rvb-feature-list-mode'.

`RET' acts on the feature at point, `g' refreshes, `+' adds a feature
for each issue assigned to you that has none, `c' shows or hides
closed features and `q' buries the buffer.  Nothing here is editable,
so those letters cost nothing; everything else lives behind `C-c C-f',
the same prefix as in the status buffer, where a bare letter would
type itself instead.

`g' also asks GitHub again about every issue, the way `g' in a status
buffer does about its pull requests.  `revert-buffer' refreshes too,
without that."
  "RET"     #'rvb-feature-list-visit
  "g"       #'rvb-feature-list-refresh
  "n"       #'rvb-feature-list-next
  "p"       #'rvb-feature-list-previous
  "+"       #'rvb-feature-create-from-assigned-issues
  "X"       #'rvb-feature-archive-closed
  "c"       #'rvb-feature-list-toggle-closed
  "q"       #'quit-window
  "C-c C-f" #'rvb-feature-dispatch)

(define-derived-mode rvb-feature-list-mode special-mode "Features"
  "Major mode showing every feature as a card on a timeline.

Auto Revert keeps it current: this buffer visits no file, so
`buffer-stale-function' answers for it, and a commit in any member
worktree redraws the list within `auto-revert-interval'.

\\<rvb-feature-list-mode-map>\\[rvb-feature-list-visit] opens the \
feature at point, \\[rvb-feature-dispatch] is the
command menu, and \\[revert-buffer] redraws."
  :interactive nil
  ;; Its faces inherit Magit's; see the note at the top of the file.
  (require 'magit)
  (setq buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (rvb-feature-list-refresh)))
  (setq-local buffer-stale-function #'rvb-feature--list-stale-p)
  (add-hook 'window-selection-change-functions #'rvb-feature--refresh-on-revisit nil t)
  (add-hook 'window-buffer-change-functions #'rvb-feature--refresh-on-revisit nil t)
  (add-hook 'window-size-change-functions #'rvb-feature--list-resized nil t)
  ;; Cards are drawn to a width; wrapping would break every border.
  (setq truncate-lines t)
  (buffer-disable-undo)
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
  "Show every feature, the ones still open and due soonest first.
See `rvb-feature--entry-before-p' for the order in full.
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

(defface rvb-feature-closed '((t :inherit shadow))
  "Face merged over a closed feature's entry in the list.
Only the colour is taken, so the heading keeps its size."
  :group 'rvb-feature)

(defface rvb-feature-branch '((t :inherit magit-branch-local))
  "Face for a member's checked-out branch."
  :group 'rvb-feature)

(defface rvb-feature-count '((t :inherit magit-dimmed))
  "Face for neutral counts and separators."
  :group 'rvb-feature)

(defface rvb-feature-open '((t :inherit rvb-feature-count))
  "Face for a feature whose issue is still open.
Dim on purpose: nearly everything in the list is open, and a colour
every entry wears is one that tells you nothing."
  :group 'rvb-feature)

(defface rvb-feature-done '((t :inherit success))
  "Face for a feature whose issue is closed or merged.
Deliberately not `error' the way a closed *pull request* is: a closed
issue is work that is finished, not work that was rejected."
  :group 'rvb-feature)

(defface rvb-feature-due '((t :inherit rvb-feature-count))
  "Face for a due date with time still on it."
  :group 'rvb-feature)

(defface rvb-feature-due-soon '((t :inherit warning))
  "Face for work due today or tomorrow."
  :group 'rvb-feature)

(defface rvb-feature-overdue '((t :inherit error))
  "Face for work whose due date has passed."
  :group 'rvb-feature)

(defface rvb-feature-agent-running '((t :inherit success))
  "Face for the status of an agent implementing a feature."
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

(defun rvb-feature--inject-agent-status (feature)
  "Put FEATURE's running-agent status in the Org keyword block."
  (when-let* ((agent (rvb-feature--agent-info feature)))
    (save-excursion
      (goto-char (point-min))
      ;; Keep it immediately below the title when there is one.  With
      ;; no title it becomes the first line of the same keyword block.
      (if (re-search-forward rvb-feature--title-keyword-regexp
                             (rvb-feature--preamble-limit) t)
          (forward-line 1)
        (goto-char (point-min)))
      (let ((start (point)))
        (insert (propertize "#+agent:" 'font-lock-face
                            'org-document-info-keyword)
                " ")
        (rvb-feature--insert-agent-status feature agent)
        (insert "\n")
        (rvb-feature--generated start (point)
                                'rvb-feature-agent-status t)))))

(defun rvb-feature--update-agent-status ()
  "Update only the running-agent line in this feature status buffer.

Unlike a full redraw, this preserves unsaved edits.  That matters when
the user chose to assign the on-disk specification without first
saving the prose currently being edited."
  (let ((inhibit-read-only t)
        (modified (buffer-modified-p))
        (pos (point-min)))
    (save-excursion
      (while (setq pos (text-property-any
                        pos (point-max) 'rvb-feature-agent-status t))
        (let ((end (or (next-single-property-change
                        pos 'rvb-feature-agent-status nil (point-max))
                       (point-max))))
          (remove-overlays pos end 'rvb-feature-button t)
          (delete-region pos end)))
      (rvb-feature--inject-agent-status rvb-feature--buffer-feature))
    (set-buffer-modified-p modified)))

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
                                   'font-lock-face 'rvb-feature-unresolved)
                  "  ")
          (rvb-feature--insert-link
           "Fix"
           (lambda () (rvb-feature--agent-fix-review rvb-feature--buffer-feature m))
           (format "Have the agent address %s's review comments" name)))))))

(defun rvb-feature--generated-suffix (start end)
  "Mark START..END as generated text appended to a line you still own.

Like `rvb-feature--generated', but deliberately not front-sticky.  The
`#+issue:' keyword is yours to edit -- it is how a feature is linked to
an issue in the first place -- and a front-sticky region butting
against the end of it would mean the last character of the URL could
not be corrected."
  (add-text-properties start end
                       (list 'read-only t
                             'rear-nonsticky t
                             'rvb-feature-generated t)))

(defun rvb-feature--issue-annotation (feature)
  "Return what GitHub says about FEATURE's issue, ready to display.

Whether it is still open, and the iteration it is due in.  Nil when
the feature is linked to no issue, or while nothing has come back yet.

The lookups are asynchronous, like the pull-request ones, so this
answers nil the first time and the buffer is drawn again when they
land -- drawn, not refreshed: no worktree needs rereading because
GitHub answered."
  (when-let* ((buf (current-buffer))
              ((rvb-feature-issue feature)))
    (let* ((redraw (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf (rvb-feature--redraw)))))
           (info (rvb-feature--feature-info feature redraw))
           (parts (delq nil
                        (list (rvb-feature--state-string (plist-get info :state))
                              ;; Only a date that came off the board.  A
                              ;; `#+due:' keyword is already on screen a
                              ;; line away -- this buffer is the file --
                              ;; and echoing it back would read as a
                              ;; second, disagreeing date.
                              (and (eq (plist-get info :due-from) 'iteration)
                                   (rvb-feature--due-string info))))))
      (when parts
        (mapconcat #'identity parts rvb-feature-list-separator)))))

(defun rvb-feature--inject-issue (feature)
  "Annotate FEATURE's `#+issue:' line with what GitHub says about it.

On that line rather than beside the title, because it is what the
annotation is about: the title is free text of the feature's own, and
whether the work is still open and when it is due are the issue's."
  (when-let* ((text (rvb-feature--issue-annotation feature)))
    (save-excursion
      (goto-char (point-min))
      ;; Unbounded, like the search `rvb-feature-issue' reads the
      ;; keyword with, so the annotation lands on the line it read.
      (when (re-search-forward rvb-feature--issue-keyword-regexp nil t)
        (end-of-line)
        (let ((start (point)))
          (insert "  " text)
          (rvb-feature--generated-suffix start (point)))))))

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
      (rvb-feature--inject-agent-status feature)
      (rvb-feature--inject-issue feature)
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
          (rvb-feature--status-signature feature members)
          rvb-feature--refreshed-at (float-time))
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
  (require 'magit)
  (setq-local revert-buffer-function (lambda (&rest _) (rvb-feature-refresh)))
  (setq-local buffer-stale-function #'rvb-feature--status-stale-p)
  (add-hook 'window-selection-change-functions #'rvb-feature--refresh-on-revisit nil t)
  (add-hook 'window-buffer-change-functions #'rvb-feature--refresh-on-revisit nil t)
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

(defun rvb-feature--agent-label (&optional agent)
  "Return the display name of AGENT, by default `rvb-feature-agent'."
  (let ((agent (or agent rvb-feature-agent)))
    (pcase agent
      ('codex "Codex")
      ('claude "Claude Code")
      (_ (symbol-name agent)))))

(defun rvb-feature--agent-command (&optional agent continue)
  "Return the command list for AGENT, by default `rvb-feature-agent'.

With CONTINUE, the command carries on the agent's most recent session
in the directory it is run from rather than starting a new one.  Both
agents scope that to the working directory, and every feature has a
directory of its own, so \"most recent here\" is this feature's
conversation -- nothing needs recording to find it again.  The prompt
arrives on standard input either way."
  (let ((agent (or agent rvb-feature-agent)))
    (pcase agent
      ('codex
       (append (list rvb-feature-codex-executable)
               rvb-feature-codex-arguments
               ;; `-' is Codex's way of saying the prompt is on stdin.
               (and continue '("resume" "--last" "-"))))
      ('claude
       (append (list rvb-feature-claude-executable)
               rvb-feature-claude-arguments
               (and continue '("--continue"))))
      (_ (user-error "Unknown feature agent: %s" agent)))))

(defun rvb-feature--agent-context (feature)
  "Return the lines telling an agent where FEATURE is."
  (let ((dir (file-name-as-directory (rvb-feature--dir feature))))
    (concat "\n\nFeature: " feature
            "\nSpecification: " (rvb-feature--org-file feature)
            "\nWorking directory: " dir "\n")))

(defun rvb-feature--agent-prompt (feature)
  "Return the prompt assigning FEATURE to the configured agent."
  (concat (string-trim-right rvb-feature-agent-prompt)
          (rvb-feature--agent-context feature)))

(defun rvb-feature--last-agent (feature)
  "Return the agent that last worked on FEATURE, or nil if none has.
Kept in the record because a conversation can only be continued by
the agent that had it, whatever `rvb-feature-agent' says today."
  (plist-get (rvb-feature--read-record feature) :agent))

(defun rvb-feature--set-last-agent (feature agent)
  "Record AGENT as the one that last worked on FEATURE."
  (unless (eq agent (rvb-feature--last-agent feature))
    (rvb-feature--write-record
     feature
     (plist-put (or (rvb-feature--read-record feature)
                    (list :version 1 :name feature))
                :agent agent))))

(defun rvb-feature--agent-buffer-name (feature)
  "Return the output buffer name used by FEATURE's coding agent."
  (format "*feature agent: %s*" feature))

(defun rvb-feature--agent-buffer (feature)
  "Return the output buffer used by FEATURE's coding agent."
  (get-buffer-create (rvb-feature--agent-buffer-name feature)))

(defun rvb-feature--agent-process (feature)
  "Return FEATURE's agent process, or nil when it has no output buffer."
  (when-let* ((buffer (get-buffer (rvb-feature--agent-buffer-name feature))))
    (get-buffer-process buffer)))

(defun rvb-feature-show-agent (feature)
  "Open FEATURE's coding-agent output buffer."
  (interactive
   (list (or rvb-feature--buffer-feature
             (and (derived-mode-p 'rvb-feature-list-mode)
                  (rvb-feature--entry-name))
             (rvb-feature--enclosing)
             (rvb-feature--read-name t))))
  (if-let* ((buffer (get-buffer (rvb-feature--agent-buffer-name feature))))
      (pop-to-buffer buffer)
    (user-error "No agent output for %s" feature)))

(defun rvb-feature--redraw-agent-views (feature &optional refresh-status)
  "Show FEATURE's current agent state in every open feature view.

When REFRESH-STATUS is non-nil, also re-probe an unmodified status
buffer because the agent may have changed its member worktrees."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond
       ((and (derived-mode-p 'rvb-feature-status-mode)
             (equal rvb-feature--buffer-feature feature))
        (if (and refresh-status (not (buffer-modified-p)))
            (rvb-feature-refresh)
          (rvb-feature--update-agent-status)))
       ((and (derived-mode-p 'rvb-feature-list-mode)
             rvb-feature--list-state)
        (rvb-feature--render-list rvb-feature--list-state))))))

(defun rvb-feature--agent-sentinel (process event)
  "Finish an agent PROCESS after terminal EVENT."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'rvb-feature-finished)))
    (process-put process 'rvb-feature-finished t)
    (let* ((feature (process-get process 'rvb-feature))
           (label (process-get process 'rvb-feature-agent-label))
           (success (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process))))
           (summary (if success
                        (format "%s finished %s" label feature)
                      (format "%s stopped for %s: %s"
                              label feature (string-trim event)))))
      (when-let* ((buffer (process-buffer process))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n" summary "\n"))))
      ;; The agent may have changed any number of member repositories.
      ;; Re-probe every open status view when it is safe, and remove the
      ;; running marker from all views immediately either way.
      (rvb-feature--redraw-agent-views feature t)
      (message "%s" summary))))

;;;###autoload
(defun rvb-feature-assign-to-agent (feature)
  "Assign FEATURE to `rvb-feature-agent' in a headless process.

The agent runs from the feature directory, so all of the feature's
member worktrees are in scope.  Its instructions arrive on standard
input, its output goes to a dedicated buffer, and the feature status
is refreshed when it exits.

This starts a new session.  `rvb-feature-agent-ask' carries on the
last one, which is how to ask for a correction afterwards."
  (interactive
   (list (or rvb-feature--buffer-feature
             (rvb-feature--enclosing)
             (rvb-feature--read-name t))))
  (unless (rvb-feature-members feature)
    (user-error "%s has no repositories to work in" feature))
  (rvb-feature--run-agent feature (rvb-feature--agent-prompt feature)))

(defun rvb-feature--run-agent (feature prompt &optional continue doing)
  "Run an agent on FEATURE with PROMPT, headless, from its directory.

CONTINUE carries on the last session, with the agent that had it --
see `rvb-feature--agent-command' -- and appends to its output buffer
rather than starting it over, so the conversation reads top to bottom.
Without it, a new session is started with `rvb-feature-agent'.

DOING says what the agent is up to, for the status line; it reads
after the agent's name, as in \"Codex is DOING\"."
  (let* ((agent (or (and continue (rvb-feature--last-agent feature))
                    rvb-feature-agent))
         (label (rvb-feature--agent-label agent))
         (doing (or doing "implementing this feature"))
         (status-buffer (get-buffer (format "*feature: %s*" feature)))
         (buffer (rvb-feature--agent-buffer feature))
         (old-process (get-buffer-process buffer))
         (dir (file-name-as-directory (rvb-feature--dir feature)))
         (command (rvb-feature--agent-command agent continue))
         (program (executable-find (car command)))
         process)
    (when (process-live-p old-process)
      (display-buffer buffer)
      (user-error "%s is already working on %s"
                  (process-get old-process 'rvb-feature-agent-label) feature))
    (unless program
      (user-error "Cannot find executable: %s" (car command)))
    ;; The status buffer is the editing surface for feature.org, even
    ;; when this command was invoked from the global dispatch menu.
    (when (buffer-live-p status-buffer)
      (with-current-buffer status-buffer
        (rvb-feature--settle-edits (format "handing %s to an agent" feature))))
    (rvb-feature--ensure-org feature)
    (setq command (cons program (cdr command)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (if continue
            (progn (goto-char (point-max))
                   (insert "\n" (make-string 70 ?-) "\n\n"))
          (erase-buffer))
        (insert (format "%s is %s\nDirectory: %s\nCommand: %s\n\n"
                        label doing dir
                        (mapconcat #'shell-quote-argument command " ")))
        (when continue
          (insert "Asked:\n" (string-trim prompt) "\n\n")))
      (unless (derived-mode-p 'special-mode) (special-mode))
      (setq default-directory dir))
    (let ((default-directory dir))
      (setq process
            (make-process
             :name (format "rvb-feature-agent-%s" feature)
             :buffer buffer
             :command command
             :connection-type 'pipe
             :coding 'utf-8-unix
             :sentinel #'rvb-feature--agent-sentinel)))
    (process-put process 'rvb-feature feature)
    (process-put process 'rvb-feature-agent-label label)
    (process-put process 'rvb-feature-agent-doing doing)
    (rvb-feature--set-last-agent feature agent)
    (process-send-string process prompt)
    (process-send-eof process)
    (rvb-feature--redraw-agent-views feature)
    (display-buffer buffer)
    (message "%s is %s" label doing)))

;;;###autoload
(defun rvb-feature-agent-ask (feature prompt)
  "Send PROMPT to the agent that last worked on FEATURE -- a correction, say.

The last session is continued rather than a new one started, so the
agent still knows what it did and why.  A feature no agent has worked
on yet gets a new session, told where the feature is."
  (interactive
   (let ((feature (or rvb-feature--buffer-feature
                      (and (derived-mode-p 'rvb-feature-list-mode)
                           (rvb-feature--entry-name))
                      (rvb-feature--enclosing)
                      (rvb-feature--read-name t))))
     (list feature
           (read-string
            (format "Tell %s: "
                    (rvb-feature--agent-label
                     (rvb-feature--last-agent feature)))))))
  (when (string-empty-p (string-trim prompt))
    (user-error "Nothing to say"))
  (if (rvb-feature--last-agent feature)
      (rvb-feature--run-agent feature prompt t "working on your correction")
    (rvb-feature--run-agent feature
                            (concat (string-trim-right prompt)
                                    (rvb-feature--agent-context feature)))))

(defcustom rvb-feature-agent-review-prompt
  "Review comments are waiting on %1$s, the pull request for the %2$s/ \
worktree (branch %3$s).

Read its unresolved review threads -- `gh api graphql` with the pull \
request's reviewThreads gives each thread's comments and whether it \
isResolved -- and make the changes they ask for in %2$s/.  Where a \
comment is a question, or you disagree with it, change nothing and say \
why instead.

Do not commit or push, and do not reply to or resolve anything on \
GitHub: finish with each thread and what you did about it, so the \
changes can be checked before reviewers see them."
  "Prompt asking the agent to address a pull request's review comments.
A `format' string: %1$s is the pull request as \"owner/repo#42\", %2$s
the repository's directory in the feature and %3$s its branch.

Committing, pushing and answering reviewers are left out on purpose,
the same as in `rvb-feature-agent-prompt': a fix is worth reading
before anyone else sees it."
  :type 'string
  :group 'rvb-feature)

(defun rvb-feature--agent-fix-review (feature m)
  "Have an agent address the review comments on member M's pull request.
The last session is continued when there is one, since the agent that
wrote the code is the one best placed to answer comments on it."
  (let* ((pr (rvb-feature--pr-or-error m))
         (key (or (rvb-feature--pr-key pr)
                  (user-error "Cannot tell which pull request %s's is"
                              (plist-get m :name))))
         (prompt (format rvb-feature-agent-review-prompt key (plist-get m :name)
                         (or (plist-get m :head) (plist-get m :branch))))
         (doing (format "addressing review comments on %s" key)))
    (unless (equal (plist-get pr :state) "open")
      (user-error "%s is %s" key (plist-get pr :state)))
    (if (rvb-feature--last-agent feature)
        (rvb-feature--run-agent feature prompt t doing)
      (rvb-feature--run-agent
       feature (concat prompt (rvb-feature--agent-context feature)) nil doing))))

;;;###autoload
(defun rvb-feature-agent-fix-review ()
  "Have an agent address the review comments on this repository's PR.
The `Fix' link beside a heading's unresolved count runs this too.  See
`rvb-feature-agent-review-prompt' for what it is asked."
  (interactive)
  (let ((target (rvb-feature--target-member)))
    (rvb-feature--agent-fix-review (car target) (cdr target))))

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


;;; Archiving closed features
;;
;; A feature whose issue has closed has nothing left to do in its
;; worktrees.  Archiving removes them -- and the branches, where that
;; loses nothing -- rewrites the Implementation section as the pull
;; requests that were merged, and moves what is left, the Org file and
;; the record, under `rvb-feature-archive-name'.  The feature list shows
;; archived features under Done, with closed ones, when asked to.
;;
;; Nothing here runs by itself.  Worktrees are where uncommitted and
;; unpushed work lives, and GitHub saying an issue is closed is no
;; reason to risk it: every archive is asked for and confirmed, and a
;; feature with work that exists nowhere else is left alone and said why.

(declare-function rvb/github-fetch-pull-request "rvb-github" (dir branch callback))
(declare-function rvb/github-fetch-issue "rvb-github" (key callback))

(defconst rvb-feature-archive-name ".archive"
  "Directory under `rvb-feature-directory' holding archived features.
Dotted, so `rvb-feature--names' does not take it for a feature.")

(defun rvb-feature--archive-dir ()
  "Return the directory archived features are moved to."
  (file-name-as-directory
   (expand-file-name rvb-feature-archive-name rvb-feature-directory)))

(defun rvb-feature--archived-p (feature)
  "Return non-nil if FEATURE names an archived feature.
Archived features are named by their path under `rvb-feature-directory',
\".archive/NAME\", which is what lets `rvb-feature--dir' and everything
built on it find their files without being told about the archive."
  (string-prefix-p (concat rvb-feature-archive-name "/") feature))

(defun rvb-feature--archived-names ()
  "Return the names of all archived features."
  (let ((dir (rvb-feature--archive-dir)))
    (when (file-directory-p dir)
      (mapcar (lambda (n) (concat rvb-feature-archive-name "/" n))
              (sort (cl-remove-if-not
                     (lambda (n) (file-directory-p (expand-file-name n dir)))
                     (directory-files dir nil directory-files-no-dot-files-regexp))
                    #'string<)))))

(defun rvb-feature--archived-time (feature)
  "Return when FEATURE was archived, or nil if it has not been."
  (when-let* (((rvb-feature--archived-p feature))
              (stamp (plist-get (rvb-feature--read-record feature) :archived)))
    (ignore-errors (parse-iso8601-time-string stamp))))

(defun rvb-feature--archived-info (feature)
  "Return `rvb-feature--feature-info' for archived FEATURE, without GitHub.
It was archived because its issue closed, and the archive only grows:
asking GitHub about every issue in it on every redraw would cost more
than it could ever tell."
  (let ((key (rvb-feature-issue feature)))
    (list :key key
          :url (and key (fboundp 'rvb/github-url) (rvb/github-url key))
          :state "closed")))

(defun rvb-feature--map-async (items fn callback)
  "Call FN on each of ITEMS, then CALLBACK with their results, in order.
FN is called with an item and a function to hand its result to, which
it may call at once or later."
  (if (null items)
      (funcall callback nil)
    (let* ((results (make-vector (length items) nil))
           (pending (length items)))
      (cl-loop for item in items for i from 0 do
               (let ((i i))
                 (funcall fn item
                          (lambda (result)
                            (aset results i result)
                            (when (zerop (cl-decf pending))
                              (funcall callback (append results nil))))))))))

(defun rvb-feature--unpushed-count (dir)
  "Return how many commits at HEAD in DIR are on no remote."
  (string-to-number
   (or (rvb-feature--git dir "rev-list" "--count" "HEAD" "--not" "--remotes")
       "0")))

(defun rvb-feature--archive-plan (feature callback)
  "Work out what archiving FEATURE would do, then call CALLBACK with it.

A plist: :feature; :members, each with the :pr GitHub has for its
branch; :merged, the pull requests that were merged; and :problems,
the reasons it must not be archived yet, as strings.

A problem is anything archiving would lose: uncommitted changes, or
commits on no remote whose pull request was not merged -- once a pull
request is merged the work is on GitHub, even if a squash left the
branch's own commits behind.  A running agent is one too."
  (let ((members (rvb-feature-members feature))
        (gh (and (fboundp 'rvb/github-fetch-pull-request)
                 (boundp 'rvb/github-executable)
                 (executable-find rvb/github-executable))))
    (rvb-feature--map-async
     members
     (lambda (m done)
       (let ((branch (plist-get m :branch)))
         (if (or (not gh) (plist-get m :missing) (null branch))
             (funcall done m)
           (rvb/github-fetch-pull-request
            (plist-get m :dir) branch
            (lambda (pr) (funcall done (append (list :pr pr) m)))))))
     (lambda (members)
       (let (problems merged)
         (dolist (m members)
           (let* ((name (plist-get m :name))
                  (dir (plist-get m :dir))
                  (pr (plist-get m :pr))
                  (pr-merged (equal (plist-get pr :state) "merged")))
             (when pr-merged (push pr merged))
             (unless (plist-get m :missing)
               (when (rvb-feature--dirty-p dir)
                 (push (format "%s has uncommitted changes" name) problems))
               (let ((unpushed (rvb-feature--unpushed-count dir)))
                 (when (and (> unpushed 0) (not pr-merged))
                   (push (format "%s has %d commit%s on no remote"
                                 name unpushed (if (= unpushed 1) "" "s"))
                         problems))))))
         (when (rvb-feature--agent-info feature)
           (push "an agent is working on it" problems))
         (funcall callback
                  (list :feature feature
                        :members members
                        :merged (nreverse merged)
                        :problems (nreverse problems))))))))

(defun rvb-feature--write-merged-implementation (feature merged)
  "Replace FEATURE's Implementation section with the MERGED pull requests.

Bare URLs, one to a line: rvb-github.el shows each as the pull
request's reference, title and state, and they read as themselves
anywhere else.  What was written under each repository went to its
pull request as the description, so it is on GitHub still."
  (let ((file (rvb-feature--org-file feature)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (if-let* ((bounds (rvb-feature--section-body
                           rvb-feature-implementation-heading)))
            (progn (delete-region (car bounds) (cdr bounds))
                   (goto-char (car bounds)))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "\n* " rvb-feature-implementation-heading "\n"))
        (insert (if merged
                    (concat (mapconcat (lambda (pr) (concat "- " (plist-get pr :url)))
                                       merged "\n")
                            "\n")
                  "No pull requests were merged.\n"))
        (unless (eobp) (insert "\n"))
        (write-region (point-min) (point-max) file nil 'quiet)))))

(defun rvb-feature--archive-now (plan)
  "Carry out PLAN from `rvb-feature--archive-plan'.  Return its report.

Worktrees go first, and if any will not, nothing else happens: a
feature is either archived or left as it was.  A branch is deleted when
its pull request was merged, or when git agrees it is merged; any other
is kept, and the report names it."
  (let* ((feature (plist-get plan :feature))
         (status (get-buffer (format "*feature: %s*" feature)))
         failed kept)
    (when (buffer-live-p status)
      (with-current-buffer status
        (rvb-feature--settle-edits (format "archiving %s" feature))))
    (dolist (m (plist-get plan :members))
      (unless (plist-get m :missing)
        (condition-case err
            (rvb-feature--git! (or (plist-get m :origin) (plist-get m :dir))
                               "worktree" "remove" "--force" (plist-get m :dir))
          (error (push (format "%s: %s" (plist-get m :name)
                               (error-message-string err))
                       failed)))))
    (if failed
        (list :feature feature :failed (nreverse failed))
      (dolist (m (plist-get plan :members))
        (let ((origin (plist-get m :origin))
              (branch (plist-get m :branch)))
          (when origin
            (rvb-feature--git origin "worktree" "prune"))
          (when (and origin branch (rvb-feature--branch-p origin branch))
            (unless (if (equal (plist-get (plist-get m :pr) :state) "merged")
                        (rvb-feature--git-ok origin "branch" "-D" branch)
                      (rvb-feature--git-ok origin "branch" "-d" branch))
              (push (format "%s in %s" branch (plist-get m :name)) kept)))))
      (rvb-feature--write-merged-implementation feature (plist-get plan :merged))
      (rvb-feature--write-record
       feature
       (plist-put (plist-put (or (rvb-feature--read-record feature)
                                 (list :version 1 :name feature))
                             :members nil)
                  :archived (format-time-string "%FT%T%z")))
      (make-directory (rvb-feature--archive-dir) t)
      (let* ((base (expand-file-name feature (rvb-feature--archive-dir)))
             (dest base)
             (n 1))
        (while (file-exists-p dest)
          (setq dest (format "%s-%d" base (cl-incf n))))
        (rename-file (directory-file-name (rvb-feature--dir feature)) dest))
      (dolist (buffer (list status (get-buffer (rvb-feature--agent-buffer-name feature))))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (list :feature feature :kept (nreverse kept)
            :merged (length (plist-get plan :merged))))))

(defun rvb-feature--archive-report (reports)
  "Say what archiving produced REPORTS, and redraw the list."
  (let ((done (cl-remove-if (lambda (r) (plist-get r :failed)) reports))
        (failed (cl-remove-if-not (lambda (r) (plist-get r :failed)) reports))
        (kept (mapcan (lambda (r) (copy-sequence (plist-get r :kept))) reports)))
    (when-let* ((list (get-buffer "*features*")))
      (with-current-buffer list (rvb-feature-list-refresh)))
    (message "%s"
             (string-join
              (delq nil
                    (list (and done
                               (format "Archived %s"
                                       (mapconcat (lambda (r) (plist-get r :feature))
                                                  done ", ")))
                          (and kept (format "kept unmerged branch%s %s"
                                            (if (cdr kept) "es" "")
                                            (string-join kept ", ")))
                          (and failed
                               (format "could not remove worktrees for %s"
                                       (mapconcat
                                        (lambda (r)
                                          (format "%s (%s)" (plist-get r :feature)
                                                  (string-join (plist-get r :failed) "; ")))
                                        failed ", ")))))
              "; "))))

(defun rvb-feature--plan-summary (plan)
  "Describe what archiving PLAN keeps, for a prompt."
  (let ((merged (length (plist-get plan :merged))))
    (format "%s: %s" (plist-get plan :feature)
            (if (zerop merged) "no merged PRs"
              (format "%d merged PR%s" merged (if (= merged 1) "" "s"))))))

;;;###autoload
(defun rvb-feature-archive (feature)
  "Archive FEATURE: remove its worktrees and move its notes to the archive.

What happens is described under \"Archiving closed features\" in this
file.  Meant for a feature whose issue has closed, and says so when it
has not; a feature with uncommitted or unpushed work is refused."
  (interactive
   (list (or rvb-feature--buffer-feature
             (and (derived-mode-p 'rvb-feature-list-mode) (rvb-feature--entry-name))
             (rvb-feature--read-name t))))
  (when (rvb-feature--archived-p feature)
    (user-error "%s is already archived" feature))
  (message "Asking GitHub about %s's pull requests..." feature)
  (rvb-feature--archive-plan
   feature
   (lambda (plan)
     (let* ((state (plist-get (rvb-feature--issue-info-cached feature) :state))
            (open (not (member state '("closed" "merged"))))
            (problems (plist-get plan :problems)))
       (cond
        (problems
         (message "Not archiving %s: %s" feature (string-join problems "; ")))
        ((yes-or-no-p (format "Archive %s, removing its worktrees%s? "
                              (rvb-feature--plan-summary plan)
                              (if open
                                  (if (plist-get (rvb-feature--issue-info-cached feature) :state)
                                      " -- its issue is still OPEN"
                                    " -- it has no closed issue")
                                "")))
         (rvb-feature--archive-report (list (rvb-feature--archive-now plan))))
        (t (message "Kept %s" feature)))))))

(defun rvb-feature--issue-info-cached (feature)
  "Return what is already known about FEATURE's issue, asking nothing."
  (when-let* ((key (rvb-feature-issue feature))
              ((fboundp 'rvb/github-lookup)))
    (rvb/github-lookup key nil)))

;;;###autoload
(defun rvb-feature-archive-closed ()
  "Archive every feature whose issue is closed, after asking once.

GitHub is asked afresh about each feature's issue and pull requests;
the prompt names what would be archived and what would be skipped, and
why.  See `rvb-feature-archive' for what archiving one does."
  (interactive)
  (unless (fboundp 'rvb/github-fetch-issue)
    (user-error "This needs rvb-github"))
  (let ((linked (delq nil (mapcar (lambda (name)
                                    (when-let* ((key (rvb-feature-issue name)))
                                      (cons name key)))
                                  (rvb-feature--names)))))
    (message "Asking GitHub which features are closed...")
    (rvb-feature--map-async
     linked
     (lambda (pair done)
       (rvb/github-fetch-issue
        (cdr pair)
        (lambda (issue)
          (funcall done (and issue (equal (plist-get issue :state) "closed")
                             (car pair))))))
     (lambda (closed)
       (setq closed (delq nil closed))
       (if (null closed)
           (message "No closed features to archive")
         (message "Asking GitHub about their pull requests...")
         (rvb-feature--map-async
          closed #'rvb-feature--archive-plan
          (lambda (plans)
            (let ((ready (cl-remove-if (lambda (p) (plist-get p :problems)) plans))
                  (blocked (cl-remove-if-not (lambda (p) (plist-get p :problems)) plans)))
              (cond
               ((null ready)
                (message "Nothing can be archived: %s"
                         (mapconcat (lambda (p)
                                      (format "%s (%s)" (plist-get p :feature)
                                              (string-join (plist-get p :problems) "; ")))
                                    blocked ", ")))
               ((yes-or-no-p
                 (format "Archive %d closed feature%s, removing their worktrees (%s)%s? "
                         (length ready) (if (cdr ready) "s" "")
                         (mapconcat #'rvb-feature--plan-summary ready ", ")
                         (if blocked
                             (format "; skipping %s"
                                     (mapconcat
                                      (lambda (p)
                                        (format "%s (%s)" (plist-get p :feature)
                                                (string-join (plist-get p :problems) "; ")))
                                      blocked ", "))
                           "")))
                (rvb-feature--archive-report
                 (mapcar #'rvb-feature--archive-now ready)))
               (t (message "Archived nothing")))))))))))


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
    ("C" "Create from my assigned issues" rvb-feature-create-from-assigned-issues)
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
   ["Agent"
    ("A" "Assign to agent" rvb-feature-assign-to-agent)
    ("m" "Message it  (a correction)" rvb-feature-agent-ask)
    ("R" "Fix a PR's review comments" rvb-feature-agent-fix-review)
    ("o" "Show its output" rvb-feature-show-agent)]
   ["Manage"
    ("f" "Fetch all" rvb-feature-fetch-all)
    ("k" "Remove a repo" rvb-feature-remove-repo)
    ("x" "Archive a feature" rvb-feature-archive)
    ("X" "Archive closed features" rvb-feature-archive-closed)
    ("D" "Delete feature" rvb-feature-delete)]])

(provide 'rvb-features)
;;; rvb-features.el ends here
