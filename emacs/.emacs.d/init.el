;; ========== Set up package archives ==========
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents t))

; Disable bell
(setq ring-bell-function 'ignore)

;; Don't show splash screen
(setq inhibit-startup-message t)

;; Save history of minibuffer
(setq history-length 25)
(savehist-mode 1)

;; Don't use my nice config for the custom variables
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Auto revert EVERYTHING
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Mac OS X Settings
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Misc settings
(setq make-backup-files nil)
(setq vc-follow-symlinks t)

;; Search settings
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

;; Show line numbers in programming buffers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Make it count lines for correct line number width
(setq display-line-numbers-width-start t)

;; ========== Theming ==========
(use-package standard-themes
  :ensure t
  :config
  (load-theme 'standard-light t))

;; ========== Completions ==========

(use-package icomplete
  :bind (:map icomplete-minibuffer-map
	      ("RET" . icomplete-force-complete-and-exit))
  :hook
  (after-init . (lambda ()
		  (fido-mode 1)
		  (icomplete-mode 1)))
  :config
  ;; (setq icomplete-in-buffer t)
  (setq tab-always-indent 'complete)
  (setq icomplete-show-matches-on-no-input t))
  ;; (advice-add 'completion-at-point
  ;; 	      :after #'minibuffer-hide-completions))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; ========== Tree sitter ==========
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; ========== Magit ==========
(use-package magit
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  (prog-mode . eglot-ensure))

;; ========== ChatGPT ==========
(use-package gptel
  :ensure t
  :bind
  (("C-c g s" . gptel-send)))

;; ========== Expand region ==========
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; ========== Python stuff ==========
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
        (setq venv-path (shell-command-to-string "uv env")))))
    (setq venv-path (string-trim venv-path))  ; Trim whitespace
    (let ((json-content
           (json-encode `((venvPath . ,venv-path)
                          (venv . ,venv)
                          (exclude . ["**/__pycache__/**/*"
                                      "**/*.pyc"
                                      "**/*.pyo"])
                          (reportMissingImports . t)
                          (reportMissingTypeStubs . t)
                          (typeCheckingMode . "basic")))))
      (let ((file-path (expand-file-name "pyrightconfig.json" directory)))
        (with-temp-file file-path
          (insert json-content))
        (message "Configuration file saved to %s" file-path)))))

;; ========== MRK Mode ==========
;; Create a major mode for .mrk files called MRK
(define-derived-mode mrk-mode text-mode "MRK"
  "Major mode for editing .mrk files."
  ;; Define the syntax highlighting rules
  (font-lock-add-keywords
   nil
   '(("^=[0-9A-Z][0-9A-Z][0-9A-Z]" . font-lock-keyword-face) ;; Tags
     ("$[a-z0-9]" . font-lock-variable-name-face))) ;; Subfields

  (setq font-lock-defaults '(nil)))

(add-to-list 'auto-mode-alist '("\\.mrk\\'" . mrk-mode))

(require 'eglot)
(add-to-list 'eglot-server-programs '(mrk-mode . ("/Users/rvanbron/test-lsp/.venv/bin/python" "/Users/rvanbron/test-lsp/test.py")))
(add-hook 'mrk-mode-hook 'eglot-ensure)
