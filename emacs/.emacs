;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(package-refresh-contents t)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always ensure packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

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
    "j"  'counsel-switch-buffer
    "k"  'kill-buffer
    "f"  'counsel-fzf
  ))

(setq evil-want-keybinding nil)

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

(use-package ivy
  :diminish
  :init
  :config
  (ivy-mode 1))

(use-package projectile
  :diminish
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;; Change theme
(use-package modus-themes)
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(load-theme 'modus-operandi t)

;; Change font
(set-face-font 'default "Menlo 16" nil)

;; Disable bell
(setq ring-bell-function 'ignore)


;; FZF binary
(use-package counsel)
(setq counsel-fzf-cmd (concat (concat (getenv "HOME") "/.fzf/bin/fzf") " -f \"%s\""))

;; Display line numbers
(global-display-line-numbers-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dad40020beea412623b04507a4c185079bff4dcea20a93d8f8451acb6afc8358" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(company-anaconda anaconda-mode conda vterm markdown-mode lsp-pyright yaml-mode use-package undo-fu queue pythonic projectile magit helm general evil-collection counsel company-tabnine)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
