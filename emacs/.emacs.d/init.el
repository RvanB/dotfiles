;; ---------- SETUP ----------

;; Add to load-path
(add-to-list 'load-path
	     (concat user-emacs-directory
		     (convert-standard-filename "lisp/")))
;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents t)

;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; ---------- PREFERENCES ----------

;; Make org agenda take up the whole window
(setq org-agenda-window-setup 'current-window)

;; Start up Emacs in Org Agenda with serif font
;; (setq initial-buffer-choice #'(lambda () (progn (org-agenda nil "n") (buffer-set-serif))))

;;(setq initial-buffer-choice #'(lambda () (org-agenda nil "n")))

;; Replace Buffer List with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scrolling behavior
(setq scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Golden ratio scrolling
(use-package golden-ratio-scroll-screen
  :ensure t
  :init
  (setq golden-ratio-scroll-highlight-flag nil)
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)
  )

;; Disable bell
(setq ring-bell-function 'ignore)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Disable backup files
(setq make-backup-files nil)

;; Default to horizontal split
(setq split-width-threshold 1 )

;; Searching
;; Add lazy count to isearch
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
;; Treat space as non-greedy wildcard in normal isearch
(setq search-whitespace-regexp ".*?")

;; Use shell environment variables
(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Better C-a
(defun back-to-indentation-or-beginning ()
  "Move point to the first non-whitespace character on the line."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(global-set-key [remap move-beginning-of-line]
		'back-to-indentation-or-beginning)

;; ---------- APPEARANCE ----------

;; Font
(set-face-attribute 'default nil :font "Hack 14")

;; Use variable width font faces in current buffer
(defun buffer-set-serif ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Palatino" :height 180))
  "Change cursor type to line"
  (setq cursor-type 'bar)
  "Don't highlight the current line"
  (hl-line-mode -1)
  " Disable line numbers"
  (display-line-numbers-mode -1)
  (buffer-face-mode))

(defun buffer-set-sans-serif ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Hack"))
  "Change cursor type to box"
  (setq cursor-type 'box)
  "Highlight the current line"
  (hl-line-mode 1)
  (buffer-face-mode))

;; Count lines in buffer for line number width
(setq display-line-numbers-width-start 1)

;; Highlight current line
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Show line numbers in programming buffers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Hide toolbar
(tool-bar-mode -1)

;; Prot's themes
(use-package standard-themes :ensure t)
(use-package ef-themes :ensure t)

;; Hide scroll bar
(scroll-bar-mode -1)

;; Org indentation
(setq org-adapt-indentation t)

;; Org bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Load theme
(load-theme 'modus-operandi t)

;; ---------- UTILITIES ----------

;; Keycast
(use-package keycast :ensure t)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; ;; Company
(use-package company
  :ensure t)
  
(add-hook 'after-init-hook 'global-company-mode)

;; Copilot
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(use-package copilot
  :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory)))
(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)


;; multi-vterm
(use-package multi-vterm
  :ensure t
  :bind ("C-s-t" . multi-vterm))

;; vterm
(use-package vterm
  :ensure t)

;; Tree sitter
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Magit
(use-package magit
  :ensure t
  :init
  ;; Show magit in fullscreen
  (setq magit-display-buffer-function 'display-buffer))

;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
  

;; ace-jump-mode
;; Cool jumping
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c C-SPC" . ace-jump-mode))

;; LSP mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (python-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; LSP pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))


;; npy
;; Python stuff
(use-package f :ensure t)
(use-package s :ensure t)
(require 'nalist)
(require 'gpc)
(require 'npy)
(npy-initialize)


;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; (add-hook 'python-mode-hook 'eglot-ensure)

;; Make python flymake use ruff
;; (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))

;; vertico
;; Nicer completion UI
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; orderless
;; Partial completions
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; docker
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; diminish
(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  (diminish 'abbrev-mode)
  (diminish 'which-key-mode)
  (diminish 'company-mode)
  (diminish 'copilot-mode)
  (diminish 'lsp-mode)
  (diminish 'tree-sitter-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("be73fbde027b9df15a98a044bcfff4d46906b653cb6eef0d98ebccb7f8425dc9" "2459d6e7e96aefaed9cebaf7fde590f64e76c96f48632d8310cfea5d10ec2bb1" "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb" "e871f44a640f98523876f77dccdbf0e20747ca7e111f9f147fe23c9d5f4937c1" "50bb891011dfe0c30cd463c65e898523788d4ac4e6df141eed75030a33da1135" "1b8df5c4f3364ebfbe9c0d3d859f6c31ab652ba518612ec27b12e462ce677731" "4ae2387bb3bcfb3419d88f586b41c1fef3ff8620b80d06d53f98ec30df469407" "eb0f822891b90a730f3331959311439f01bb39da3cdf998b5693ecec877858d0" "2cc1ac47eed7ac51d79d1aaf6218d52ec84d9c6eb8a448f221f592bddfe51550" "82b43e48862ecc7e3af29838ed843227e331b187865828dc4915021c5a74baa1" "e6b0ec96166bb3bb2843d83e56c0292308aab10ee5b79fb921d16ad2dbea5d5f" "38457f8afb329ce87e1a41d31e155acb4dcdf5ee6a1ea703d401f2042747a69f" "4f6dc03105f64cd7e5a3f555ea7c6bac7d9447141473ef9ff3c23b63858066da" default))
 '(package-selected-packages
   '(flatui-theme zenburn-theme yaml-mode which-key vertico use-package undo-fu tree-sitter-langs standard-themes spacious-padding rainbow-delimiters org-bullets orderless multi-vterm marginalia magit lsp-pyright keycast golden-ratio-scroll-screen flycheck expand-region exec-path-from-shell evil elpy ef-themes editorconfig docker diminish ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
