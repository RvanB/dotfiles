;;; Disable lock files
(setq create-lockfiles nil)

(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil)

;; Garbage collection
(setq gc-cons-threshold 100000000)

;; Read more from a process at once
(setq read-process-output-max (* 1024 1024))

;;; Indentation
;; use spaces instead of tabs
(setq indent-tabs-mode nil)

;;; Only bring up warnings at error level
(setq warning-minimum-level :error)

;;; Disable bell
(setq ring-bell-function 'ignore)

;; Calculate line number width
(setq display-line-numbers-width-start t)

;;; Don't show splash screen
(setq inhibit-startup-message t)

;;; Save history of minibuffer
(setq history-length 25)
(savehist-mode 1)

;;; Don't use my nice config for the custom variables
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;; Auto revert EVERYTHING
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)

;;; Mac OS X Settings
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Misc settings
(setq make-backup-files nil)
(setq vc-follow-symlinks t)

;;; Select newly split windows
(defun rvb/select-newly-split-window (window)
  "Select WINDOW returned by a split command and return it."
  (select-window window)
  window)

(advice-add 'split-window-right :filter-return #'rvb/select-newly-split-window)
(advice-add 'split-window-below :filter-return #'rvb/select-newly-split-window)

;;; Search settings
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

;;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Project switch commands
(setq project-switch-commands
      '((project-find-file "Find file" ?f)
        (project-find-regexp "Find regexp" ?r)
        (project-eshell "Eshell" ?e)
        (magit-project-status "Magit" ?m)
        (project-any-command "Other" ?o)))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'reload-init-file)

;; Modes
(add-to-list 'auto-mode-alist '("\\.yaml.j2\\'" . yaml-mode))

;; `rvb/project-directories' is set with `M-x customize-variable' or
;; `M-x rvb/project-add-directory', so the paths live in `custom-file'
;; rather than here.

;;; Start at the feature dashboard rather than the splash screen.
;;; A function value must return a buffer, which is why this is
;;; `rvb-feature-list-buffer' and not the interactive `rvb-feature-list'.
;;; Setting this also inhibits the startup screen, and `emacsclient'
;;; with no target file obeys it too.
;;; rvb-features.el loads after this file; the function is not called
;;; until startup finishes, so the forward reference is fine.
(declare-function rvb-feature-list-buffer "rvb-features" ())

(provide 'rvb-settings)
