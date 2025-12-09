;;; Disable lock files
(setq create-lockfiles nil)

(setq tab-always-indent 'complete)


;;; Indentation
;; use spaces instead of tabs
(setq indent-tabs-mode nil)

;;; Only bring up warnings at error level
(setq warning-minimum-level :error)

;;; Disable bell
(setq ring-bell-function 'ignore)

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

(provide 'rvb-settings)
