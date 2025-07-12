
;;; Set up package archives 
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))


(unless package-archive-contents
  (package-refresh-contents t))

(setq package-install-upgrade-built-in t)

;;; Disable lock files
(setq create-lockfiles nil)

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
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;; Winner mode
(winner-mode)

;;; Mac OS X Settings
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;; Misc settings
(setq make-backup-files nil)
(setq vc-follow-symlinks t)

;;; Search settings
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

;;; Show line numbers in programming buffers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; Make it count lines for correct line number width
(setq display-line-numbers-width-start t)

;;; Terminals
(use-package eat
  :ensure t)

(global-set-key (kbd "C-c t") 'eat)
(global-set-key (kbd "C-c p") 'eat-project)

;;; Appearance
  
(fringe-mode 0)

(defun rvb/update-ns-appearance (appearance)
  "Update the ns-appearance frame parameter based on the appearance"
  (set-frame-parameter nil 'ns-appearance appearance)
  (set-frame-parameter nil 'ns-transparent-titlebar t))

(defun rvb/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi t))
    ('dark (load-theme 'modus-vivendi t))))

(defun rvb/modus-toggle-update-ns-appearance (&rest _)
  "Update ns-appearance after toggling Modus theme."
  (let ((appearance (cond
                     ((member 'modus-operandi custom-enabled-themes) 'light)
                     ((member 'modus-vivendi custom-enabled-themes) 'dark))))
    (when appearance
      (rvb/update-ns-appearance appearance))))

(advice-add 'modus-themes-toggle :after #'rvb/modus-toggle-update-ns-appearance)

(global-set-key (kbd "<f5>") #'modus-themes-toggle)

;; (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; Spacious padding
(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode))

;; ef themes
(setq ef-themes-italic-constructs t
      ef-themes-bold-constructs t
      ef-themes-italic-comments t)
(use-package ef-themes :ensure t)

;; Modus themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)

;; Standard themes
(setq standard-themes-italic-constructs t
      standard-themes-bold-constructs t
      standard-themes-italic-comments t
      standard-themes-prompts '(bold))

(use-package standard-themes :ensure t)

;; Apply theme
(rvb/apply-theme 'dark)
(rvb/update-ns-appearance 'dark)

;; (use-package spacious-padding
;;   :ensure t
;;   :config
;;   (spacious-padding-mode))

;; (use-package autothemer
;;   :ensure t)
;; (require 'autothemer)

;;; Disable menu bar
;; (menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)



;;; Set the font
(set-face-attribute 'default nil :font "Aporetic Sans Mono 16")
(set-face-attribute 'variable-pitch nil :font "Aporetic Sans Mono 16")


(use-package eldoc-box
  :ensure t
  ;; :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :config
  (defun my-eldoc-box-update-faces ()
    "Update eldoc-box faces based on the current theme."
    (set-face-attribute 'eldoc-box-border nil
                        :background (frame-parameter nil 'foreground-color))
    (set-face-attribute 'eldoc-box-body nil
                        :font "Aporetic Sans Mono 14"
                        :background (frame-parameter nil 'background-color)))
  (my-eldoc-box-update-faces)
  (advice-add 'load-theme :after (lambda (&rest _) (my-eldoc-box-update-faces))))

(defun eglot-open-link ()
  (interactive)
  (if-let* ((url (get-text-property (point) 'help-echo)))
      (browse-url url)
    (user-error "No URL at point")))

(define-advice eldoc-display-in-buffer (:after (&rest _) update-keymap)
  (with-current-buffer eldoc--doc-buffer
    (keymap-local-set "RET" #'eglot-open-link)
    ))

(setq tab-always-indent 'complete)

;;; Detect indentation
(use-package dtrt-indent
  :ensure t)

(dtrt-indent-global-mode t)

;;; Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;;; Enable Vertico.
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  (vertico-multiform-mode))

;; (use-package vertico-posframe
;;   :ensure t
;;   :init
;;   (setq vertico-posframe-parameters
;;       '((left-fringe . 10)
;;         (right-fringe . 10)))
;;   :config
;;   (vertico-posframe-mode))


;; (use-package nova
;;   :vc (:url "https://github.com/thisisran/nova"
;;             :rev :newest
;;             :branch "main")
;;   :init
;;   (defface orderless-match-face-0 nil nil)
;;   :config
;;   (nova-vertico-mode)
;;   (nova-corfu-mode)
;;   (nova-corfu-popupinfo-mode 1)
;;   )


;; (setq tab-bar-format
;;       '(tab-bar-format-tabs
;;         tab-bar-separator
;;         tab-bar-format-align-right))
;; (tab-bar-mode)

;; (use-package otpp
;;   :ensure t
;;   :after project
;;   :init
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; Tree sitter
;; (use-package tree-sitter
;;   :ensure t)

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '(
	  (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (dot        . ("https://github.com/rydesun/tree-sitter-dot"))
          (doxygen    . ("https://github.com/tree-sitter-grammars/tree-sitter-doxygen"))
          (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (gitcommit  . ("https://github.com/gbprod/tree-sitter-gitcommit"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (gosum      . ("https://github.com/amaanq/tree-sitter-go-sum"))
          (gowork     . ("https://github.com/omertuc/tree-sitter-go-work"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (http       . ("https://github.com/rest-nvim/tree-sitter-http"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua        . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
          (make       . ("https://github.com/tree-sitter-grammars/tree-sitter-make"))
          (markdown   . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown"))
          (proto      . ("https://github.com/treywood/tree-sitter-proto"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql        . ("https://github.com/derekstride/tree-sitter-sql"))
          (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (vue        . ("https://github.com/tree-sitter-grammars/tree-sitter-vue")))))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; PDFs
(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-view-use-scaling nil))

;;; Magit
(use-package magit
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  (prog-mode . eglot-ensure))

;;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Text editing improvements
;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   (global-undo-tree-mode))

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(use-package undo-fu
  :ensure t)

;; Download Evil
;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-undo-system 'undo-fu)
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   :config

;;   (evil-mode 1))

(use-package keycast
  :ensure t)





;; God mode
(use-package god-mode
  :ensure t
  :init
  
  (setq god-mode-enable-function-key-translation nil)
  :config
  (add-to-list 'god-exempt-major-modes 'eat-mode)
  (god-mode)
  
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
    
  (global-set-key (kbd "C-g") #'rvb/keyboard-quit-and-god-mode)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-window-below)
  (global-set-key (kbd "C-x C-3") #'split-window-right)
  (global-set-key (kbd "C-x C-0") #'delete-window)
  (global-set-key (kbd "C-x C-o") #'other-window)

  (global-set-key (kbd "C-c C-s i") #'surround-insert)
  (global-set-key (kbd "C-c C-s d") #'surround-delete)
  (global-set-key (kbd "C-c C-s c") #'surround-change)
  
  
  (global-set-key (kbd "C-x C-g") #'magit))

(setq god-mode-cursor-type 'box)
(setq normal-mode-cursor-type 'bar)

(defun rvb/set-cursor-according-to-mode ()
  (cond
   (god-local-mode
    (setq cursor-type god-mode-cursor-type))
   (t
    (setq cursor-type normal-mode-cursor-type))))

(defun rvb/keyboard-quit-and-god-mode ()
  (interactive)
  (god-mode-all 1)
  (keyboard-quit))

(add-hook 'post-command-hook #'rvb/set-cursor-according-to-mode)

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; Prevent undo tree files from polluting your git repo
;; (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo")))

(advice-add 'keyboard-quit :before
            (lambda ()
              (isearch-done)))

(defvar-local rvb/isearch-narrowed nil)
(defvar-local rvb/isearch-wrapped nil)

(defun rvb/isearch-visible-region ()
  "Narrow buffer to visible region and start isearch. Auto-widens on exit."
  (interactive)
  (let ((start (window-start))
        (end (save-excursion
               (goto-char (window-end nil t))
               (point))))
    (narrow-to-region start end)
    (setq rvb/isearch-narrowed t)
    (setq rvb/isearch-wrapped nil)
    (add-hook 'isearch-update-post-hook #'rvb/auto-wrap-isearch nil t)
    (call-interactively #'isearch-forward)))

(defun rvb/widen-after-isearch ()
  "Widen the buffer if it was narrowed by rvb/isearch-visible-region."
  (when rvb/isearch-narrowed
    (setq rvb/isearch-narrowed nil)
    (setq rvb/isearch-wrapped nil)
    (remove-hook 'isearch-update-post-hook #'rvb/auto-wrap-isearch t)
    (widen)))

(defun rvb/auto-wrap-isearch ()
  "Automatically wrap Isearch when no match is found in narrowed region."
  (when (and isearch-forward
             isearch-string
             (not isearch-success)
             (not rvb/isearch-wrapped))  ;; prevent infinite looping
    (setq rvb/isearch-wrapped t)
    (goto-char (point-min))
    (isearch-repeat-forward)))

(add-hook 'isearch-mode-end-hook #'rvb/widen-after-isearch)

(global-set-key (kbd "C-c C-i") #'rvb/isearch-visible-region)


;; (add-hook 'isearch-mode-end-hook (lambda ()
;;                                    (when (buffer-narrowed-p)
;;                                      (widen))))
;; (defvar-local rvb/isearch-narrowed nil)

;; (defun rvb/isearch-visible-region ()
;;   "Narrow buffer to visible region and start isearch. Auto-widens on exit."
;;   (interactive)
;;   (let ((start (window-start))
;;         (end (save-excursion
;;                (goto-char (window-end nil t))
;;                (point))))
;;     (narrow-to-region start end)
;;     (setq rvb/isearch-narrowed t)
;;     (call-interactively #'isearch-forward)))

;; (defun rvb/widen-after-isearch ()
;;   "Widen the buffer if it was narrowed by rvb/isearch-visible-region."
;;   (when rvb/isearch-narrowed
;;     (setq rvb/isearch-narrowed nil)
;;     (widen)))

;; (add-hook 'isearch-mode-end-hook #'rvb/widen-after-isearch)

;; (global-set-key (kbd "C-c C-i") #'rvb/isearch-visible-region)


(use-package expand-region
  :ensure t
:bind
  ("C-=" . er/expand-region)
  :config
  (setq expand-region-smart-cursor t)
  ;; Prioritize symbol over word
  (setq expand-region-exclude-text-mode-expansions nil)
  (setq er/try-expand-list
        '(er/mark-symbol
          er/mark-method-call
          er/mark-inside-pairs
          er/mark-outside-pairs
          er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-comment
          er/mark-defun
          er/mark-url
          er/mark-email
          er/mark-text-paragraph
          er/mark-text-sentence
          er/mark-paragraph
          er/mark-page)))

(use-package smart-hungry-delete
  :ensure t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	 ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	 ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

;;; Remap M-f and M-b to move by symbol instead of word
;; (global-set-key (kbd "M-f") #'forward-symbol)
;; (global-set-key (kbd "M-b") (lambda () (interactive) (forward-symbol -1)))

;;; Surround
(use-package surround
  :ensure t
) 


;;; Delete selection on type
(delete-selection-mode 1)

;;; Python stuff

;; This is causing problems with syntax highlighting on python-ts-mode startup.
;;; PET - Python Executable Tracker
;; (use-package pet
;;   :ensure t
;;   :config
;;   (add-hook 'python-ts-mode-hook
;;             (lambda ()
;;               (setq-local python-shell-interpreter (pet-executable-find "python"))
;;               (setq-local python-shell-virtualenv-root (pet-virtualenv-root)))))


;; Flymake ruff
;; (use-package flymake-ruff
;;   :ensure t
;;   :hook (eglot-managed-mode . flymake-ruff-load))

;; This function isn't working for some reason, provided by the flymake-ruff git repo
;; (defun rvb/filter-eglot-diagnostics (diags)
;;     "Drop Pyright 'variable not accessed' notes from DIAGS."
;;     (list (seq-remove (lambda (d)
;;                         (and (eq (flymake-diagnostic-type d) 'eglot-note)
;;                              (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
;;                              (s-ends-with? "is not accessed" (flymake-diagnostic-text d))))
;;                       (car diags))))

;; (advice-add 'eglot--report-to-flymake :filter-args #'rvb/filter-eglot-diagnostics)

;;; Pyright configuration
(defun rvb/pyright-config ()
  "Create a JSON configuration file for Python using a specified package manager to find the venv."
  (interactive)
  (let* ((directory (read-directory-name "Choose directory: "))
         (package-manager (completing-read "Choose package manager: " '("pipenv" "poetry" "uv")))
         (venv-path nil)
         (venv nil))
    (cond
     ((string-equal package-manager "pipenv")
      (with-temp-buffer
        (cd directory)
        (let ((full-path (shell-command-to-string "pipenv --venv")))
  	(setq venv-path (file-name-directory (directory-file-name (string-trim full-path))))
        (setq venv (file-name-nondirectory (directory-file-name (string-trim full-path)))))))
     ((string-equal package-manager "poetry")
      (with-temp-buffer
        (cd directory)
        (let ((full-path (shell-command-to-string "poetry run poetry env info --path 2> /dev/null")))
          (setq venv-path (file-name-directory (directory-file-name (string-trim full-path))))
          (setq venv (file-name-nondirectory (directory-file-name (string-trim full-path)))))))
     ((string-equal package-manager "uv")
      (with-temp-buffer
        (cd directory)
        (setq venv-path (file-name-directory (expand-file-name ".venv" directory)))
	(setq venv ".venv"))))
    (setq venv-path (string-trim venv-path))  ; Trim whitespace
    (let ((json-content
           (json-encode `((venvPath . ,venv-path)
                          (venv . ,venv)
                          (exclude . ["**/__pycache__/**/*"
                                      "**/*.pyc"
                                      "**/*.pyo"])
                          (reportMissingImports . t)
                          (reportMissingTypeStubs . t)
                          (reportUnusedCallResult . :json-false)
                          (typeCheckingMode . "basic")))))
      (let ((file-path (expand-file-name "pyrightconfig.json" directory)))
        (with-temp-file file-path
          (insert json-content))
        (message "Configuration file saved to %s" file-path)))))

(defun rvb/ruff-check-project ()
  ;; get project root with (when-let ((project (project-current))) (project-root project))
  (interactive)
  ;; Run "NO_COLOR=1 ruff check -q <project root>" and display in a buffer in compilation mode
  (let ((output-buffer (get-buffer-create "*ruff-check*")))
	(with-current-buffer output-buffer
	  (erase-buffer)
	  (insert (shell-command-to-string (format "NO_COLOR=1 ruff check --output-format=concise -q %s" (when-let ((project (project-current))) (project-root project)))))
	  (compilation-mode)
	  (local-set-key "q" (lambda () (interactive) (quit-window t))))
	(display-buffer output-buffer)))
(global-set-key (kbd "C-c r") 'rvb/ruff-check-project)

;; MARC Mode

(load (expand-file-name (concat user-emacs-directory "marc-mode.el")))
(require 'marc-mode)



(require 'eglot)
(add-to-list 'eglot-server-programs '(marc-mode . ("marc-lsp-server")))
(add-hook 'marc-mode-hook 'eglot-ensure)

;;; Window management
;;; Enable keybindings for window switching
(windmove-default-keybindings)

(defun rvb/kill-buffer-and-close-window ()
  "Kill the current buffer and close the window."
  (interactive)
  (let ((buffer (current-buffer)))
    (kill-buffer buffer)
    (when (one-window-p)
      (delete-window))
    (when (and (not (one-window-p))
               (not (window-live-p (get-buffer-window buffer))))
      (delete-window))))

;;; Killing a window closes the buffer
(global-set-key (kbd "s-k") 'rvb/kill-buffer-and-close-window)

;;; Window traversal
(defun rvb/other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

;;; In-Buffer Movement
(use-package ultra-scroll
  :pin "manual"
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
	    :rev :newest
	    :branch "main")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;;; Disable changing text scale with the mouse
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(defun rvb/back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(global-set-key [remap move-beginning-of-line] 'rvb/back-to-indentation-or-beginning)
(global-set-key [remap org-beginning-of-line] 'rvb/back-to-indentation-or-beginning)

;;; i-search changes
;; https://emacs.stackexchange.com/questions/53004/improving-isearch/53006#53006
(defun rvb/isearch-repeat-forward+ ()
  (interactive)
  (unless isearch-forward
    (when isearch-other-end
      (goto-char isearch-other-end)))
  (isearch-repeat-forward)
  (unless isearch-success
    (isearch-repeat-forward)))

(defun rvb/isearch-repeat-backward+ ()
  (interactive)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward)
  (unless isearch-success
    (isearch-repeat-backward)))

(keymap-set isearch-mode-map "C-s" 'rvb/isearch-repeat-forward+)
(keymap-set isearch-mode-map "C-r" 'rvb/isearch-repeat-backward+)

;;; Org mode
(setq org-directory "~/orgfiles/")
(setq org-agenda-files (list org-directory))

;;; gptel
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'o3-mini
	gptel-backend (gptel-make-gh-copilot "Copilot"))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :bind
  (("C-c g m" . gptel-menu)))

(defcustom aider-extra-args ""
  "Extra command-line arguments passed to aider when starting a session."
  :type 'string
  :group 'aider)


;; (defun aider--get-models ()
;;   "Return a list of available models by running lsllms command in the shell."
;;   (let ((output (shell-command-to-string "lsllms")))
;;     (split-string output "\n" t)))  ; t removes empty strings

;; (defun aider--choose-model ()
;;   "Prompt the user to choose a model using completing-read from lsllms output."
;;   (completing-read "Select aider model: " (aider--get-models)))

;; (defun aider-start-session ()
;;   "Start an aider session in a comint buffer with the chosen model and extra args.
;; Uses lsllms to list available models and then launches the aider process."
;;   (interactive)
;;   (let* ((model (aider--choose-model))
;;          (full-model (if (string-prefix-p "openai/" model)
;;                          model
;;                        (concat "openai/" model)))
;;          (cmd (string-join (list "aider" "--model" full-model aider-extra-args) " "))
;;          (buffer-name "*aider-session*"))
;;     (with-current-buffer (get-buffer-create buffer-name)
;;       (comint-mode)
;;       (erase-buffer))
;;     (make-comint-in-buffer "aider" buffer-name "zsh" nil "-i" "-c" cmd)
;;     (pop-to-buffer buffer-name)
;;     (message "Started aider session with command:\n%s" cmd)))

;;; GitHub Copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))


(use-package aider
  :ensure t
  :config
  (exec-path-from-shell-copy-env "OPENAI_API_BASE")
  (exec-path-from-shell-copy-env "OPENAI_API_KEY")
  ;; (setq aider-args '("--model" "openai/claude-3.7-sonnet" "--no-auto-accept-architect" "--no-auto-commits"))
  (global-set-key (kbd "C-c e") 'aider-transient-menu))

;;; Custom key bindings
(global-set-key (kbd "s-p") 'project-switch-project)

(put 'narrow-to-page 'disabled nil)

(add-hook 'emacs-startup-hook #'eat)
