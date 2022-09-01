;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


(package-refresh-contents t)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always ensure packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(use-package rjsx-mode)

(use-package conda
  :defer t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-tabnine :ensure t)
(add-to-list 'company-backends #'company-tabnine)
;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Keybindings
(use-package general
  :config
  (general-create-definer rvb/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rvb/leader-keys
    "k"  'kill-buffer
    "f"  'helm-projectile-find-file
    "s"  'helm-ag
  ))

(setq evil-want-keybinding nil)

(global-visual-line-mode t)
(global-hl-line-mode t)

;; Disable toolbar
(tool-bar-mode -1)

;; load evil
(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config ;; tweak evil after loading it
  (evil-mode 1))

;; load evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; (use-package ivy
;;   :diminish
;;   :init
;;   :config
;;   (ivy-mode 1))

(use-package projectile
  :diminish
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package helm
  :init
    (require 'helm-config)
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t)
  :config 
    (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
    (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
    (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
    (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
    (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
    (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
    (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
    (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
    (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
  :ensure t)

(use-package helm-projectile)

(use-package helm-ag)

;; Change theme
(use-package modus-themes)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(load-theme 'modus-operandi t)

;; Change font
(set-face-font 'default "Monaco 16" nil)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Display line numbers
(global-display-line-numbers-mode 1)

;; Disable backup files
(setq make-backup-files nil)

;; Follow symlinks
(setq vc-follow-symlinks t)

(scroll-bar-mode -1)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" default))
 '(helm-minibuffer-history-key "M-p")
 '(inhibit-startup-screen t)
 '(neo-window-fixed-size nil)
 '(package-selected-packages
   '(helm-projectile helm-ag fzf affe rjsx-mode company-anaconda anaconda-mode conda vterm markdown-mode lsp-pyright yaml-mode use-package undo-fu queue pythonic projectile magit helm general evil-collection company-tabnine)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
