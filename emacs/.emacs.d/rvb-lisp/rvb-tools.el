;;; graphviz
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;;; Magit
(use-package magit
  :ensure t)

;;; Where my Git repositories live.
;;
;; One list, `rvb/project-directories' (see rvb-projects.el), drives
;; both project detection and Magit's repository list -- they were
;; always the same directories, and keeping two customizations in step
;; by hand is a chore that only ever gets forgotten.
;;
;; `magit-repository-directories' cannot be that list directly: every
;; entry is a (DIRECTORY . DEPTH) cons, so it would mean writing the
;; same depth on every line.  It is derived instead.

;; Defined in rvb-projects.el, which loads before this file.
(defvar rvb/project-directories)
;; Declared ahead of the option below, which calls it from its `:set'.
(defvar rvb/git-repository-depth)

(defun rvb/git-sync-repository-directories ()
  "Rebuild `magit-repository-directories' from `rvb/project-directories'.
Called automatically when either that option or the depth below is
customized; call it by hand after setting one with `setq', which
bypasses `:set'.

Both are read defensively.  A value saved in `custom-file' is applied
the moment its defcustom is evaluated, which calls this through
`:set' -- and at that moment the other one may not have been evaluated
yet, so reading it directly is a void-variable error during startup."
  (interactive)
  (setq magit-repository-directories
        (mapcar (lambda (dir)
                  (cons (directory-file-name (expand-file-name dir))
                        (if (boundp 'rvb/git-repository-depth)
                            rvb/git-repository-depth
                          1)))
                (and (boundp 'rvb/project-directories)
                     rvb/project-directories)))
  (when (called-interactively-p 'interactive)
    (message "%d repository %s" (length magit-repository-directories)
             (if (= 1 (length magit-repository-directories))
                 "directory" "directories"))))

(defun rvb/git--set-and-sync (symbol value)
  "Set SYMBOL to VALUE, then resync `magit-repository-directories'."
  (set-default symbol value)
  (rvb/git-sync-repository-directories))

(defcustom rvb/git-repository-depth 1
  "How far below `rvb/project-directories' to look for repositories.
1, the default, means the immediate children are the repositories.  0
means each entry is itself a repository and nothing under it is
scanned."
  :type 'integer
  :set #'rvb/git--set-and-sync
  :group 'magit-essentials)

;; `rvb/project-directories' is evaluated in rvb-projects.el, before
;; this file, so its `:set' could not sync then.  Catch up.
(rvb/git-sync-repository-directories)

;; Magit stops at the first repository it finds.  `magit-list-repos-1'
;; is a `cond' whose first branch returns DIRECTORY and never descends,
;; on the assumption that anything nested below a repository is a
;; submodule and so already accounted for.
;;
;; That does not describe ~/zephir-meta, which is a repository that
;; *contains* independent clones -- no .gitmodules, ten separate
;; repositories checked out inside it.  Magit lists the parent and
;; hides all ten.  There is no option for this, so replace the
;; traversal with one that lists a repository and still looks below it.
(declare-function magit-list-repos-1 "magit-repos" (directory depth))

(defun rvb/git-list-repos-1 (directory depth)
  "List repositories at DIRECTORY and, up to DEPTH, below it.
Replaces `magit-list-repos-1', which treats being a repository and
containing repositories as mutually exclusive.  Recurses through
`magit-list-repos-1' so this applies at every level."
  (append
   (and (file-readable-p (expand-file-name ".git" directory))
        (list (file-name-as-directory directory)))
   (and (> depth 0)
        (file-accessible-directory-p directory)
        (mapcan (lambda (child)
                  (and (file-directory-p child)
                       (magit-list-repos-1 child (1- depth))))
                (directory-files directory t
                                 directory-files-no-dot-files-regexp t)))))

(advice-add 'magit-list-repos-1 :override #'rvb/git-list-repos-1)

;; What `magit-list-repositories' shows.  The default leads with
;; "Version", which is `git describe' output and so is blank or
;; meaningless in anything that is not tag-versioned.  Branch is the
;; useful answer to "what state is this repository in", with a flag
;; column for uncommitted work: N untracked, U unstaged, S staged.
(setq magit-repolist-columns
      '(("Name"   25 magit-repolist-column-ident ())
        ("Branch" 30 magit-repolist-column-branch ())
        ("Flag"    4 magit-repolist-column-flags ((:right-align t)))
        ("B<U"     3 magit-repolist-column-unpulled-from-upstream
         ((:right-align t) (:sort <)))
        ("B>U"     3 magit-repolist-column-unpushed-to-upstream
         ((:right-align t) (:sort <)))
        ("Path"   99 magit-repolist-column-path ())))

;;; Debugger
(use-package dap-mode
  :ensure t)

(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

(defun rvb/dired-mouse-mark (event)
  "Toggle mark at mouse click."
  (interactive "e")
  (mouse-set-point event)
  (save-excursion
    (beginning-of-line)
    (if (eq (char-after) dired-marker-char)
        (dired-unmark 1)
      (dired-mark 1))))
(define-key dired-mode-map [mouse-3] 'rvb/dired-mouse-mark)

(provide 'rvb-tools)
