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

;; Start up Emacs in Org Agenda
(setq initial-buffer-choice #'(lambda () (org-agenda nil "n")))

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
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
	   (beginning-of-line)))
(global-set-key [remap move-beginning-of-line]
		'back-to-indentation-or-beginning)

;; ---------- APPEARANCE ----------


;; Font
(set-face-attribute 'default nil :font "Iosevka 16")


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
(load-theme 'standard-light t)

;; ---------- UTILITIES ----------

;; Keycast
(use-package keycast :ensure t)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Company
(use-package company
  :ensure t
  :diminish company-mode)
(add-hook 'after-init-hook 'global-company-mode)

;; Copilot
(use-package s :ensure t)
(use-package dash :ensure t)
(use-package editorconfig :ensure t)
(use-package copilot
  :load-path (lambda () (expand-file-name "copilot.el" user-emacs-directory))
  :diminish copilot-mode)
(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)


;; multi-vterm
(use-package multi-vterm
  :ensure t
  :bind ("C-c t" . multi-vterm))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("d8bcb88ef0a3259a38d6deba78e569c0750ebfede82ad3e6da16573419fef48c" "2459d6e7e96aefaed9cebaf7fde590f64e76c96f48632d8310cfea5d10ec2bb1" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" "e6b0ec96166bb3bb2843d83e56c0292308aab10ee5b79fb921d16ad2dbea5d5f" "45e409674661674c12070af5f8ef71741599eeb9fccd84557f1b822509f3b100" "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb" "2cc1ac47eed7ac51d79d1aaf6218d52ec84d9c6eb8a448f221f592bddfe51550" "8390abb2cc504d44f0c9dfdaf79d4e943f0328a933e20ceec74c74d17d65834f" "78be54fed89551db18cbeb645ab807c99181555a51405aaba366b56d913b6040" "260ed5a03b9ed35b1ab1eb51cb06887870221490e0c5c91940dd2203b48ce60f" "31804a8ea314e76b68f8b1c454212c3d9710c4294b8cfbaa008dd338c8d91773" "7f34e5ab75ec580aff579b3b0f40379d280f8441e424b7a04322524ed7f348b6" "eb0f822891b90a730f3331959311439f01bb39da3cdf998b5693ecec877858d0" "6cff32351bcb1726accf9dcf9c400367971eaa8bb1d163409b78ea9c9a6ae8d0" "1b8df5c4f3364ebfbe9c0d3d859f6c31ab652ba518612ec27b12e462ce677731" "0340489fa0ccbfa05661bc5c8c19ee0ff95ab1d727e4cc28089b282d30df8fc8" "a0997c8cd72b848c675e66531265b68845cfdb222b32762ac8773c1dc957d10a" "17bd04719213ed7482ce37d8207f3618f55a81babe56484851ea5951ced383ef" "88267200889975d801f6c667128301af0bc183f3450c4b86138bfb23e8a78fb1" "c171012778b7cf795ac215b91e1ecab8e3946738d03095397a790ed41e0a3386" "8122fb61548fe36171d9cf24cdb9b5f24d053b626d4cda739c3815e080623209" "f5661fd54b1e60a4ae373850447efc4158c23b1c7c9d65aa1295a606278da0f8" "50bb891011dfe0c30cd463c65e898523788d4ac4e6df141eed75030a33da1135" "d0f3adfe292c9d633930e35c3458cda77796073bb25af852689f999bbb3d9398" "4f6dc03105f64cd7e5a3f555ea7c6bac7d9447141473ef9ff3c23b63858066da" "184d32f815d68d6aadf7dde7d4f7b8d8059fdd177630e501a4a52989a568d785" default))
 '(org-agenda-files '("/Users/rvanbron/.todo"))
 '(package-selected-packages
   '(yaml-mode mixed-pitch zenburn-theme rainbow-delimiters keycast copilot editorconfig company multi-vterm which-key vterm vertico use-package tree-sitter-langs standard-themes spacious-padding org-bullets orderless neotree magit golden-ratio-scroll-screen f expand-region exec-path-from-shell evil ef-themes docker default-text-scale centered-window ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
