(use-package treesit
  :ensure nil
  :init
  ;; Rust places function calls, methods, operators, properties, and variables
  ;; in tree-sitter's most detailed font-lock tier.
  (setq treesit-font-lock-level 4)
  :config
  ;; Select the tree-sitter mode before mode hooks (not by changing modes in
  ;; an already Eglot-managed buffer), so the server receives one didOpen.
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
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

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
	    :rev :newest
	    :branch :main)
  :after eglot
  :config
  (if (executable-find "emacs-lsp-booster")
      'eglot-booster-mode))

;; Jump to eglot symbols with consult
(use-package consult-eglot
  :ensure t)
;; Added embark support
(use-package consult-eglot-embark
  :ensure t)

(exec-path-from-shell-copy-env "PERL5LIB")

(defun rvb/eglot-disable-inlay-hints ()
  "Keep Eglot inlay hints disabled by default in the current buffer."
  (eglot-inlay-hints-mode -1))

(defun rvb/eglot-ensure-non-python ()
  "Start Eglot in programming modes other than Python.

Python starts Eglot later from `python-base-mode-hook', after PET has
configured the buffer-local environment."
  (unless (derived-mode-p 'python-base-mode)
    (eglot-ensure)))

(use-package eglot
  :ensure t
  :custom
  (eldoc-idle-delay 0.05)
  (eglot-send-changes-idle-time 0.05)
  :init
  :hook
  ((prog-mode . rvb/eglot-ensure-non-python)
   (eglot-managed-mode . rvb/eglot-disable-inlay-hints)))

;;; Go
(use-package go-mode
  :ensure t)

;;; Java
(use-package eglot-java
  :ensure t
  :after eglot)

;; Rust
(use-package rustic
  :ensure t
  :init
  ;; Let Rustic use the richer built-in rust-ts-mode fontification.  In
  ;; particular, legacy rust-mode does not identify ordinary function calls.
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rustic-lsp-client 'eglot))

(exec-path-from-shell-copy-env "JAVA_HOME")

;;; Python
;;; PET - Python Executable Tracker
(defun rvb/pet-export-virtual-env ()
  "Export PET's virtualenv for Eshell and new subprocesses.

`VIRTUAL_ENV' is global to the Emacs process, so the most recently
initialized Python project supplies its value."
  (when-let ((virtualenv (pet-virtualenv-root)))
    (setenv "VIRTUAL_ENV" (directory-file-name virtualenv))))

(defun rvb/pet-configure-python-shell ()
  "Use the current virtualenv's IPython for inferior Python when available."
  (when-let ((ipython (pet-executable-find "ipython")))
    (setq-local python-shell-interpreter ipython
                python-shell-interpreter-args
                "-i --simple-prompt --no-color-info")))

(defun rvb/pyright-config ()
  "Create or update this project's Pyright configuration from PET.

Preserve existing settings, update the virtualenv location, and add
the usual defaults only when they are absent.  Return non-nil when the
configuration file changed."
  (interactive)
  (condition-case err
      (when-let* ((root (pet-project-root))
                  (virtualenv (pet-virtualenv-root)))
        (let* ((file (expand-file-name "pyrightconfig.json" root))
               (config (if (file-exists-p file)
                           (json-read-file file)
                         nil))
               (virtualenv (directory-file-name virtualenv))
               (defaults
                '((exclude . ["**/__pycache__/**/*" "**/*.pyc" "**/*.pyo"])
                  (reportMissingImports . t)
                  (typeCheckingMode . "basic")))
               (quiet-diagnostics
                '(reportAny
                  reportExplicitAny
                  reportMissingParameterType
                  reportUnknownArgumentType
                  reportUnknownLambdaType
                  reportUnknownMemberType
                  reportUnknownParameterType
                  reportUnknownVariableType
                  reportMissingTypeStubs
                  reportUnusedCallResult)))
          (setf (alist-get 'venvPath config)
                (file-name-directory virtualenv)
                (alist-get 'venv config)
                (file-name-nondirectory virtualenv))
          ;; Do not require a fully annotated codebase or complain when type
          ;; inference reaches Any/Unknown.  Concrete type errors and missing
          ;; imports remain enabled.
          (dolist (diagnostic quiet-diagnostics)
            (setf (alist-get diagnostic config) :json-false))
          (dolist (setting defaults)
            (unless (assq (car setting) config)
              (push setting config)))
          (let ((content
                 (with-temp-buffer
                   (insert (json-encode config))
                   (json-pretty-print-buffer)
                   (insert "\n")
                   (buffer-string))))
            (unless (and (file-exists-p file)
                         (string= content
                                  (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string))))
              (write-region content nil file nil 'silent)
              (message "Updated %s from PET" file)
              t))))
    (error
     (message "Could not update pyrightconfig.json: %s"
              (error-message-string err))
     nil)))

(use-package pet
  :ensure t
  :custom
  ;; Project configuration and virtualenvs live at (or above) the source
  ;; directory.  Avoid PET's recursive fallback, which otherwise walks large
  ;; directories such as .venv when `fd' is unavailable.
  (pet-find-file-functions '(pet-locate-dominating-file))
  (pet-search-globally nil)
  :init
  ;; PET must configure the buffer before Eglot chooses and starts a server.
  (add-hook 'python-base-mode-hook #'pet-mode -10)
  (add-hook 'python-base-mode-hook #'eglot-ensure 10)
  (add-hook 'pet-after-buffer-local-vars-setup
            #'rvb/pet-export-virtual-env)
  (add-hook 'pet-after-buffer-local-vars-setup
            #'rvb/pet-configure-python-shell)
  (add-hook 'pet-after-buffer-local-vars-setup #'rvb/pyright-config))

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

;;; MARC
(use-package marc-mode
  :pin "manual"
  :vc (:url "https://github.com/rvanb/marc-mode.el"
            :rev :newest
            :branch "main"))

(require 'eglot)
;; (add-to-list 'eglot-server-programs
;;              '(python-mode
;;                . ("lspx"
;;                   "--lsp" "ruff server"
;;                   "--lsp" "basedpyright-langserver --stdio")))

(add-to-list 'eglot-server-programs '(marc-mode . ("marc-lsp-server")))
(add-hook 'marc-mode-hook 'eglot-ensure)

;;; Perl
(add-to-list 'eglot-server-programs '(perl-mode . ("pls")))

;;; Ruff formatting for Python
(use-package ruff-format
  :ensure t)

;;; CSV and TSV files
(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :mode ("\\.tsv\\'" . csv-mode)
  :hook ((csv-mode . csv-guess-set-separator)
         (csv-mode . csv-align-mode))
  :bind (:map csv-mode-map
         ("C->" . csv-increase-column-width)
         ("C-<" . csv-decrease-column-width)))

(provide 'rvb-langs)
