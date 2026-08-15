;;; rvb-projects.el --- Manual, multi-repo projects  -*- lexical-binding: t; -*-

;; Projects are directories I set by hand, not repositories auto-detected
;; by version control.  A manual project may contain any number of Git
;; repositories.  `project.el' handles detection (so the whole directory
;; is the project root); file/regexp search is delegated to consult's
;; async commands rooted at that directory, so they stay fast and cross
;; every repo underneath.

(require 'project)
(require 'cl-lib)

;; Defined in rvb-tools.el, which loads after this file.  A value saved
;; in `custom-file' is applied when the defcustom below is evaluated,
;; which is before that -- so the sync is skipped here and rvb-tools.el
;; runs it once on load to catch up.
(declare-function rvb/git-sync-repository-directories "rvb-tools" ())

(defun rvb/project--set-directories (symbol value)
  "Set SYMBOL to VALUE and resync anything derived from it."
  (set-default symbol value)
  (when (fboundp 'rvb/git-sync-repository-directories)
    (rvb/git-sync-repository-directories)))

(defcustom rvb/project-directories nil
  "Directories I work in, each containing Git repositories.

These serve two purposes from one list, because they were always the
same list:

  - Each is a project root.  Unlike the default VC detection, being
    inside one makes the whole directory the current project, so
    finding files and grepping cross every repository underneath.

  - `magit-repository-directories' is derived from them, together with
    `rvb/git-repository-depth', so `magit-list-repositories' and the
    repository prompts see everything below them.

Add to this with `rvb/project-add-directory', or edit it with
`\\[customize-variable]'; either way the paths are saved to
`custom-file' rather than written into a config file."
  :type '(repeat directory)
  :set #'rvb/project--set-directories
  :group 'project)

;; Defined in magit-repos.el, loaded later than this file.
(declare-function magit-list-repos "magit-repos" ())

(defun rvb/project--known-repositories ()
  "Return the repositories Magit knows about, as abbreviated names.
Driven by `rvb/project-directories'.  Empty when Magit is not loaded
or nothing is configured, in which case callers fall back to reading a
directory."
  (and (fboundp 'magit-list-repos)
       (mapcar (lambda (dir)
                 (abbreviate-file-name (directory-file-name dir)))
               (magit-list-repos))))

(defun rvb/project--normalize-1 (dir)
  "Return DIR as an abbreviated directory file-name, for comparison."
  (abbreviate-file-name (directory-file-name (expand-file-name dir))))

(defun rvb/project-add-directory (directory)
  "Add DIRECTORY to `rvb/project-directories' and save it.

Since that one list also drives `magit-repository-directories', this
is how a new working directory becomes both a project and a place
Magit lists repositories from.

Completes over the repositories Magit already knows about, but any
directory can be typed -- and usually is, because these entries are
normally directories *containing* repositories rather than
repositories themselves."
  (interactive
   (list (let ((candidates (rvb/project--known-repositories)))
           (if candidates
               (completing-read (format-prompt "Add project directory"
                                               (abbreviate-file-name
                                                default-directory))
                                candidates nil nil nil nil
                                (abbreviate-file-name default-directory))
             (read-directory-name "Add project directory: ")))))
  (let ((dir (rvb/project--normalize-1 directory)))
    (unless (file-directory-p (expand-file-name dir))
      (user-error "%s is not a directory" dir))
    (if (member dir (mapcar #'rvb/project--normalize-1 rvb/project-directories))
        (message "%s is already a project directory" dir)
      (customize-save-variable 'rvb/project-directories
                               (append rvb/project-directories (list dir)))
      (message "Added project directory %s" dir))))

(defun rvb/project-remove-directory (directory)
  "Remove DIRECTORY from `rvb/project-directories' and save."
  (interactive
   (list (completing-read "Remove project directory: "
                          (mapcar #'rvb/project--normalize-1
                                  rvb/project-directories)
                          nil t)))
  (let ((dir (rvb/project--normalize-1 directory)))
    (customize-save-variable
     'rvb/project-directories
     (cl-remove-if (lambda (d) (equal dir (rvb/project--normalize-1 d)))
                   rvb/project-directories))
    (message "Removed project directory %s" dir)))

(defvar rvb/project-extra-roots-functions nil
  "Functions returning additional project roots.
Each is called with no arguments and returns a list of directories.
Lets other modules contribute roots that come and go -- rvb-features.el
registers each feature directory this way.")

(defun rvb/project--normalize (dirs)
  "Return DIRS as absolute, slash-terminated paths."
  (mapcar (lambda (d) (file-name-as-directory (expand-file-name d))) dirs))

(defun rvb/project--roots ()
  "Return every directory that counts as a project root.

Used for *detection* -- `rvb/project-try' matches a buffer against
these -- so it includes the transient roots contributed by
`rvb/project-extra-roots-functions'.  That is deliberately wider than
what `rvb/project-prompt' offers for switching: a feature directory has
to be detectable, so that `project-find-file' inside one is scoped to
that feature, without also cluttering the list of projects you switch
between."
  (rvb/project--normalize
   (append rvb/project-directories
           (mapcan #'funcall rvb/project-extra-roots-functions))))

(defun rvb/project-try (dir)
  "Return the project for DIR.  Suitable for `project-find-functions'.

For LSP queries (eglot binds `eglot-lsp-context') return the enclosing
VCS repository, so a language server's workspace is a single repo
rather than the whole manual project -- otherwise it would try to
index every repo underneath and grind to a halt.  For everything else
return the manual project when DIR lives under a known root."
  (if (bound-and-true-p eglot-lsp-context)
      (project-try-vc dir)
    (let ((dir (file-name-as-directory (expand-file-name dir))))
      (seq-some (lambda (root)
                  (when (string-prefix-p root dir)
                    (cons 'rvb root)))
                (rvb/project--roots)))))

(cl-defmethod project-root ((project (head rvb)))
  (cdr project))

(cl-defmethod project-name ((project (head rvb)))
  (file-name-nondirectory (directory-file-name (project-root project))))

;; Only manual projects are detected -- no VC fallback.
(setq project-find-functions (list #'rvb/project-try))

;; List files across every repo in the project with a single `rg --files'
;; (gitignore-aware, fast).  This is the backend for `project-find-file',
;; `project-find-regexp', etc., and completion (orderless) filters the
;; static list natively.
(cl-defmethod project-files ((project (head rvb)) &optional dirs)
  (cl-loop for dir in (or dirs (list (project-root project)))
           nconc (let ((default-directory dir))
                   (mapcar (lambda (f) (expand-file-name f dir))
                           (process-lines-ignore-status
                            ;; "!.git" (no trailing slash) so this also skips
                            ;; the .git *file* that marks a worktree.
                            "rg" "--files" "--hidden" "--glob" "!.git")))))

;; Drive `project-switch-project' from my manual directories via the
;; public `project-prompter' customization point (Emacs 30+).  This
;; bypasses `project--list' / `project-list-file' entirely.
(defun rvb/project-prompt ()
  "Prompt for one of `rvb/project-directories'.

Only the manual directories, not the whole of `rvb/project--roots'.
Roots contributed by `rvb/project-extra-roots-functions' are things
with an entry point of their own -- a feature is opened with
`rvb-feature-status' -- and they would otherwise both crowd this list
and reach `project-switch-commands', which offers `magit-project-status'
for directories that are not repositories."
  (completing-read "Project: " (rvb/project--normalize rvb/project-directories)
                   nil t))
(setq project-prompter #'rvb/project-prompt)

;;; The buffer list, grouped by project
;;
;; `ibuffer' groups by whatever `ibuffer-filter-groups' says, which is
;; ordinarily written by hand.  The projects worth grouping by are the
;; ones that happen to be open, and they change through the day -- a
;; feature is a project only while it exists -- so the groups are
;; generated from the buffers themselves every time the list is drawn.

;; Defined in ibuf-ext.el, which arrives with Ibuffer rather than with
;; this file.
(defvar ibuffer-filter-groups)

(defvar rvb/ibuffer--projects (make-hash-table :test #'eq)
  "Which project each buffer belonged to when the groups were last built.

A snapshot rather than a lookup: `ibuffer' evaluates a group's
predicate once per buffer *per group*, and answering each of those from
`project-current' would walk the roots hundreds of times to draw one
list.")

(defun rvb/ibuffer-buffer-project ()
  "Return the name of the current buffer's project, from the snapshot."
  (gethash (current-buffer) rvb/ibuffer--projects))

(defun rvb/ibuffer--project-name (buffer)
  "Return the name of BUFFER's project, or nil if it is in none.

Asked of `project-current', so a buffer is in a project here exactly
when `project-find-file' from it would search that project -- features
included, since `rvb/project-try' detects those too."
  (with-current-buffer buffer
    (when-let* ((project (project-current)))
      (project-name project))))

(defun rvb/ibuffer-project-groups ()
  "Return `ibuffer-filter-groups' for the projects that are open now.

Also refreshes the snapshot the groups' predicates read.  Buffers whose
names begin with a space are Emacs' own and are never listed; leaving
them out keeps a project out of the groups when nothing of it is
actually on show."
  (clrhash rvb/ibuffer--projects)
  (let (names)
    (dolist (buffer (buffer-list))
      (unless (string-prefix-p " " (buffer-name buffer))
        (when-let* ((name (rvb/ibuffer--project-name buffer)))
          (puthash buffer name rvb/ibuffer--projects)
          (cl-pushnew name names :test #'equal))))
    (mapcar (lambda (name)
              (list name (cons 'predicate
                               `(equal (rvb/ibuffer-buffer-project) ,name))))
            (sort names #'string<))))

(defun rvb/ibuffer-set-project-groups (&rest _)
  "Regroup this Ibuffer by project.
Everything outside a project is left to Ibuffer's own default group,
at the bottom."
  (when (derived-mode-p 'ibuffer-mode)
    (setq ibuffer-filter-groups (rvb/ibuffer-project-groups))))

;; Before every redraw rather than on `ibuffer-hook', which runs only
;; when the `ibuffer' command is called and only after the list has been
;; drawn: groups set there would show up one refresh late, and never at
;; all for the `g' that a new project's first buffer calls for.
(advice-add 'ibuffer-update :before #'rvb/ibuffer-set-project-groups)

;; Grouping lives in ibuf-ext, which plain `ibuffer' does not load --
;; it requires it only when the command is passed groups as an argument.
;; Without this, `ibuffer-filter-groups' is a variable nothing reads and
;; every buffer quietly lands in the default group.
(with-eval-after-load 'ibuffer (require 'ibuf-ext))

(provide 'rvb-projects)
;;; rvb-projects.el ends here
