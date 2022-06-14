;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents t))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always ensure packages
(setq use-package-always-ensure t)

;; Tabnine completion
(use-package company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

(use-package golden-ratio-scroll-screen
  :init
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(global-hl-line-mode 1)

;; load evil
(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-C-i-jump nil)
  :config ;; tweak evil after loading it

  ;; Use golden ratio scrolling
  (define-key evil-normal-state-map (kbd "C-u") 'golden-ratio-scroll-screen-down)
  (define-key evil-visual-state-map (kbd "C-u") 'golden-ratio-scroll-screen-down)
  (define-key evil-normal-state-map (kbd "C-d") 'golden-ratio-scroll-screen-up)
  (define-key evil-visual-state-map (kbd "C-d") 'golden-ratio-scroll-screen-up)

  (evil-mode 1)
  (global-undo-tree-mode 1))

;; Elpy (Python stuff)
(setenv "WORKON_HOME" "~/envs")
(use-package elpy
  :config
  (defalias 'workon 'pyvenv-workon)
  (elpy-enable))
  
(use-package ivy
  :diminish
  :init
  (global-set-key (kbd "C-c C-f") 'counsel-fzf)
  :config
  (ivy-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;; Change theme
(load-theme 'modus-vivendi)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Disable scroll bar
(scroll-bar-mode -1)

;; FZF binary
(setq counsel-fzf-cmd (concat (concat (getenv "HOME") "/.fzf/bin/fzf") " -f \"%s\""))

;; Display line numbers
(global-display-line-numbers-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-tabnine projectile elpy counsel use-package undo-tree magit jupyter ivy helm evil))
 '(warning-suppress-types
   '(((python python-shell-completion-native-turn-on-maybe))
     ((python python-shell-completion-native-turn-on-maybe)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

