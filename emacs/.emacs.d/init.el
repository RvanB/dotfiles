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

(windmove-default-keybindings)

(winner-mode 1)

;; Make org agenda take up the whole window
(setq org-agenda-window-setup 'current-window)

;; Start up Emacs in Org Agenda with serif font
;; (setq initial-buffer-choice #'(lambda () (progn (org-agenda nil "n") (buffer-set-serif))))

;;(setq initial-buffer-choice #'(lambda () (org-agenda nil "n")))
;; Splitting behavior
(setq display-buffer-reuse-frames t)

;; Use ibuffer-vc
(use-package ibuffer-vc :ensure t)

;; Set up ibuffer saved filter groups
(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Dired" (mode . dired-mode))
               ("Org" (mode . org-mode))
	       ("Shell" (or
			 (mode . shell-mode)
			 (mode . vterm-mode)))
               ("Programming" (or
			       (mode . c-ts-mode)
			       (mode . perl-mode)
			       (mode . python-ts-mode)
			       (mode . bash-ts-mode)
			       (mode . emacs-lisp-mode)))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

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

(set-face-attribute 'default nil :font "Iosevka Comfy Motion 16")

;; Use italics for comments
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;; Use bold for keywords
;; (set-face-attribute 'font-lock-keyword-face nil :weight 'bold)

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
;; (ef-themes-load-random)
;;(load-theme 'ef-cyprus t)
(standard-themes-load-light)
;; (load-theme 'modus-vivendi t)

;; ---------- UTILITIES ----------



;; VLF (view large files)
(use-package vlf
  :ensure t
  :config
  (custom-set-variables '(vlf-tune-enabled nil)))

;; Spacious padding mode
;; (use-package spacious-padding
;;   :ensure t
;;   :config
;;   (spacious-padding-mode 1))

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
(define-key copilot-completion-map (kbd "C-g") 'copilot-clear-overlay)
;; (global-set-key (kbd "C-c c") 'copilot-complete)

;; Hook copilot-mode into programming buffers
(add-hook 'prog-mode-hook #'copilot-mode)

;; vterm
(use-package vterm
  :ensure t)

;; multi-vterm
(use-package multi-vterm
  :ensure t
  :bind ("C-s-t" . multi-vterm))

;; Tree sitter
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (global-treesit-auto-mode))

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
  :hook (python-ts-mode . lsp)
  :hook (bash-ts-mode . lsp)
  :hook (html-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; ;; LSP pyright
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-pyright)
;; 			 (lsp))))

;; npy
;; Python stuff
(use-package f :ensure t)
(use-package s :ensure t)
(require 'nalist)
(require 'gpc)
(require 'npy)
(npy-initialize)

;; Consult
(use-package consult
  :ensure t
  :bind (
	 ("C-x b" . consult-buffer)
	 ))

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

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
 '(custom-safe-themes
   '("dc96af3e6aaa9c96aa83d1a73a28a6d1dab58e376df1e51980b4fa9b256e9d7f" "f019002925408f081e767c515e4fb4b1d7f1462228d6cd32ff66f06a43671527" "97283a649cf1ffd7be84dde08b45a41faa2a77c34a4832d3884c7f7bba53f3f5" "0013cec68d42e640266e700c09ea4eb55e18668f72da7a5b92f0c22b80581204" "4f03e70554a58349740973c69e73aefd8ce761a77b22a9dc52a19e708532084a" "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79" "aa04c854054e8d43245bd67ca619a7bede9171e2a2efb1b2c26caf1d031497eb" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "77f1e155387d355fbbb3b382a28da41cc709b2a1cc71e7ede03ee5c1859468d2" "c171012778b7cf795ac215b91e1ecab8e3946738d03095397a790ed41e0a3386" "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" default))
 '(package-selected-packages
   '(embark-consult embark csv-mode bash-language-server vlf treesit-auto multi-vterm poetry diminish expand-region orderless marginalia vertico consult lsp-pyright lsp-mode ace-jump-mode which-key magit tree-sitter-langs tree-sitter editorconfig dash s company rainbow-delimiters keycast ef-themes standard-themes org-bullets ibuffer-vc golden-ratio-scroll-screen exec-path-from-shell))
 '(project-switch-commands
   '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-find-dir "Find directory" nil)
     (magit-project-status "Git Status" 109)
     (project-eshell "Eshell" nil)))
 '(project-switch-use-entire-map t)
 '(vlf-tune-enabled nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
