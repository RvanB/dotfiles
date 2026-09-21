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
(defvar rvb/pet--exported-virtual-env nil
  "The `VIRTUAL_ENV' value `rvb/pet-export-virtual-env' last set.")

(defun rvb/pet-export-virtual-env ()
  "Export PET's virtualenv for Eshell and new subprocesses.

`VIRTUAL_ENV' is global to the Emacs process, so the most recently
initialized Python project supplies its value.  PET must not read it
back -- see `rvb/pet-ignore-exported-virtual-env'."
  (when-let ((virtualenv (pet-virtualenv-root)))
    (setq rvb/pet--exported-virtual-env (directory-file-name virtualenv))
    (setenv "VIRTUAL_ENV" rvb/pet--exported-virtual-env)))

(defun rvb/pet-ignore-exported-virtual-env (fn &rest args)
  "Call FN with ARGS, hiding the `VIRTUAL_ENV' this config exported.

`pet-virtualenv-root' trusts `VIRTUAL_ENV' before anything it finds in
the project.  Exported from the last project opened, that would hand
its virtualenv to every project opened after it -- the next worktree in
a feature, say.  A value this config did not set, such as one activated
before Emacs started, is still honoured."
  (let ((process-environment
         (if (and rvb/pet--exported-virtual-env
                  (equal (getenv "VIRTUAL_ENV") rvb/pet--exported-virtual-env))
             ;; A bare name in `process-environment' unsets the variable.
             (cons "VIRTUAL_ENV" process-environment)
           process-environment)))
    (apply fn args)))

(defun rvb/pet-configure-python-shell ()
  "Use the current virtualenv's IPython for inferior Python when available."
  (when-let ((ipython (pet-executable-find "ipython")))
    (setq-local python-shell-interpreter ipython
                python-shell-interpreter-args
                "-i --simple-prompt --no-color-info")))

;;; Pyright (basedpyright)
;;
;; Two layers, because basedpyright reads its settings from one place or
;; the other, never both:
;;
;; - With a config file at the workspace root (pyrightconfig.json, or a
;;   [tool.pyright]/[tool.basedpyright] table in pyproject.toml), that
;;   file is the whole story and LSP analysis settings are ignored.
;;   `rvb/pyright-config' writes one when a project has neither.
;; - Without one, the LSP settings below apply.  Otherwise basedpyright
;;   falls back to its own default, "recommended" -- close to strict,
;;   and the source of reportAny noise everywhere.
;;
;; Both layers get their defaults from the two variables below, so they
;; cannot drift apart.

(defvar eglot-lsp-context)

(defvar rvb/pyright-type-checking-mode "standard"
  "Type checking mode for projects that do not choose their own.
Pyright's own default; basedpyright's is the much stricter
\"recommended\".")

(defvar rvb/pyright-quiet-rules
  '(reportAny
    reportExplicitAny
    reportMissingParameterType
    reportMissingTypeStubs
    reportUnknownArgumentType
    reportUnknownLambdaType
    reportUnknownMemberType
    reportUnknownParameterType
    reportUnknownVariableType
    reportUnusedCallResult)
  "Diagnostics turned off by default.
Ones about missing annotations and inference reaching Any/Unknown,
which flag most of an unannotated codebase.  Concrete type errors and
missing imports stay on.")

;; Merged with PET's `python.pythonPath', which it adds to this itself
;; (see `pet-eglot--workspace-configuration-plist-advice').
(setq-default eglot-workspace-configuration
              `(:basedpyright
                (:analysis
                 (:typeCheckingMode ,rvb/pyright-type-checking-mode
                  :diagnosticSeverityOverrides
                  ,(mapcan (lambda (rule)
                             (list (intern (format ":%s" rule)) "none"))
                           rvb/pyright-quiet-rules)))))

(defun rvb/python-project-root ()
  "Return the repository a Python buffer belongs to, or nil.

What Eglot uses as the language server's workspace root, and so what
PET and the Pyright config must use too.  It is not what
`project-current' says in general: `rvb/project-try' answers with the
manual project -- a parent of several repositories, or a feature
directory -- and only narrows to the enclosing repository when
`eglot-lsp-context' is bound, which is what Eglot does.  So bind it and
ask the same question.  In a feature, that is the member worktree.

Nil outside version control."
  (when-let* ((project (let ((eglot-lsp-context t))
                         (project-current))))
    (expand-file-name (project-root project))))

(defun rvb/pyright--pyproject-configures-p (root)
  "Return non-nil if ROOT's pyproject.toml configures Pyright itself."
  (let ((pyproject (expand-file-name "pyproject.toml" root)))
    (and (file-readable-p pyproject)
         (with-temp-buffer
           (insert-file-contents pyproject)
           (re-search-forward
            "^[ \t]*\\[tool\\.\\(?:based\\)?pyright[].]" nil t)))))

(defun rvb/pyright-config ()
  "Create or update this project's pyrightconfig.json.

Written at the root Eglot gives the language server -- see
`rvb/python-project-root' -- with the virtualenv PET found.  Nothing
is written outside version control, where the LSP defaults apply.  Settings already
in the file are kept; `rvb/pyright-type-checking-mode', the quiet rules
in `rvb/pyright-quiet-rules' and a few excludes are only added when the
file does not say otherwise.

Nothing is created when pyproject.toml already configures Pyright: a
pyrightconfig.json would silently take precedence over it.

Return non-nil when the file changed.  Called interactively, a running
server is told to reread its configuration."
  (interactive)
  (condition-case err
      (when-let* ((root (or (rvb/python-project-root)
                            (and (called-interactively-p 'any)
                                 (user-error "Not in a version-controlled project")))))
        (let ((file (expand-file-name "pyrightconfig.json" root)))
          (if (and (not (file-exists-p file))
                   (rvb/pyright--pyproject-configures-p root))
              (progn
                (when (called-interactively-p 'any)
                  (message "%spyproject.toml configures Pyright; leaving it be"
                           (abbreviate-file-name root)))
                nil)
            (let* ((config (and (file-exists-p file) (json-read-file file)))
                   (virtualenv (and (fboundp 'pet-virtualenv-root)
                                    (pet-virtualenv-root)))
                   (defaults
                    (append
                     ;; Setting exclude replaces Pyright's default list
                     ;; rather than adding to it, so that list is repeated.
                     '((exclude . ["**/node_modules" "**/__pycache__" "**/.*"])
                       (reportMissingImports . "error"))
                     `((typeCheckingMode . ,rvb/pyright-type-checking-mode))
                     (mapcar (lambda (rule) (cons rule :json-false))
                             rvb/pyright-quiet-rules))))
              (when virtualenv
                (let ((virtualenv (directory-file-name virtualenv)))
                  (setf (alist-get 'venvPath config)
                        (file-name-directory virtualenv)
                        (alist-get 'venv config)
                        (file-name-nondirectory virtualenv))))
              (dolist (setting defaults)
                (unless (assq (car setting) config)
                  (setq config (append config (list setting)))))
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
                  (message "Updated %s" (abbreviate-file-name file))
                  (when-let* (((called-interactively-p 'any))
                              ((fboundp 'eglot-current-server))
                              (server (eglot-current-server)))
                    (eglot-signal-didChangeConfiguration server))
                  t))))))
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
  (add-hook 'pet-after-buffer-local-vars-setup #'rvb/pyright-config)
  :config
  ;; PET keys its cache -- virtualenv, config files -- by this root, so
  ;; two repositories sharing a manual project, as a feature's worktrees
  ;; do, would otherwise share one virtualenv.  Outside version control
  ;; PET's own answer (the manual project, or none) still applies.
  (advice-add 'pet-project-root :around
              (lambda (fn &rest args)
                (or (rvb/python-project-root) (apply fn args)))
              '((name . rvb/python-project-root)))
  (advice-add 'pet-virtualenv-root :around
              #'rvb/pet-ignore-exported-virtual-env))

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
