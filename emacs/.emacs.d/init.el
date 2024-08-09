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
;; (setq split-width-threshold 160)

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

;; Spacious padding mode
(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode 1))

;; Bind Command-P to project-switch-project
(global-set-key (kbd "s-p") 'project-switch-project)

;; Project ruff
(defun ruff-check-project ()
  ;; get project root with (when-let ((project (project-current))) (project-root project))
  (interactive)
  ;; Run "NO_COLOR=1 ruff check -q <project root>" and display in a buffer in compilation mode
  (let ((output-buffer (get-buffer-create "*ruff-check*")))
	(with-current-buffer output-buffer
	  (erase-buffer)
	  (insert (shell-command-to-string (format "NO_COLOR=1 ruff check -q %s" (when-let ((project (project-current))) (project-root project)))))
	  (compilation-mode)
	  (local-set-key "q" (lambda () (interactive) (quit-window t))))
	(display-buffer output-buffer)))
(global-set-key (kbd "C-c r") 'ruff-check-project)

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

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "C-g") 'copilot-clear-overlay)
(global-set-key (kbd "C-c c") 'copilot-complete)


;; multi-vterm
;; (use-package multi-vterm
;;   :ensure t
;;   :bind ("C-s-t" . multi-vterm))

;; vterm
;; (use-package vterm
;;   :ensure t)

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
  :hook (html-mode . lsp)
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

(use-package poetry
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(project-switch-commands
   '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-find-dir "Find directory" nil)
     (magit-project-status "Git Status" 109)
     (project-eshell "Eshell" nil)))
 '(project-switch-use-entire-map t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#ffffff")))
 '(header-line ((t :box (:line-width 4 :color "#f2f2f2" :style nil))))
 '(header-line-highlight ((t :box (:color "#000000"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#ffffff")))
 '(mode-line ((t :box (:line-width 6 :color "#c8c8c8" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#c8c8c8" :style nil))))
 '(mode-line-highlight ((t :box (:color "#000000"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#e6e6e6" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#c2c2c2" :style nil))))
 '(window-divider ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider-first-pixel ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider-last-pixel ((t :background "#ffffff" :foreground "#ffffff"))))
