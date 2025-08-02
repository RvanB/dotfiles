(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '(
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
  ;; :custom
  ;; (treesit-auto-install 'prompt)
  :config
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
	    :rev :newest
	    :branch :main)
  :after eglot
  :config
  (if (executable-find "emacs-lsp-booster")
      'eglot-booster-mode))

(use-package consult-eglot
  :ensure t)

(use-package consult-eglot-embark
  :ensure t)

(exec-path-from-shell-copy-env "PERL5LIB")

(use-package eglot
  :ensure t
  :init
  :hook
  ;; In prog-mode, ,run eglot ensure and eglot-inlay-hints-mode
  (prog-mode . eglot-ensure))

(use-package eglot-java
  :ensure t
  :after eglot)


;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (prog-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))  ; or lsp-deferred

;; (use-package lsp-java
;;   :ensure t)

;;; Python stuff

;; NOTE TO SELF: This is causing problems with syntax highlighting on python-ts-mode startup.
;;; PET - Python Executable Tracker
(use-package pet
  :ensure t
  :config)

;; Set VIRTUAL_ENV environment variable to the venv defined in pyright.json
;; If pyrightconfig.json exists in the project root, get "venvPath" and "venv" from the json object and concatenate them into a variable
;; Set VIRTUAL_ENV to this variable
(defun rvb/set-venv ()
  (interactive)
  (let* ((config-file (expand-file-name "pyrightconfig.json" (project-root (project-current))))
         (json-data (json-read-file config-file))
         (venv-path (cdr (assoc 'venvPath json-data)))
         (venv (cdr (assoc 'venv json-data))))
    (when (and venv-path venv)
      (setenv "VIRTUAL_ENV" (expand-file-name venv venv-path))
      (message "VIRTUAL_ENV set to: %s" (getenv "VIRTUAL_ENV")))))

(advice-add 'pet-mode :before #'rvb/set-venv)
    
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

;; MARC Mode
(use-package marc-mode
  :pin "manual"
  :vc (:url "https://github.com/rvanb/marc-mode.el"
            :rev :newest
            :branch "main"))

(require 'eglot)
(add-to-list 'eglot-server-programs '(marc-mode . ("marc-lsp-server")))
(add-hook 'marc-mode-hook 'eglot-ensure)

(provide 'rvb-langs)
