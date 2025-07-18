
;;; gptel
(use-package gptel
  :ensure t
  :config
  ;; (setq gptel-model 'o3-mini
  ;;       gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-model 'qwen2.5-coder:7b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(qwen2.5-coder:7b)))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :bind
  (("C-c g m" . gptel-menu)))

(defcustom aider-extra-args ""
  "Extra command-line arguments passed to aider when starting a session."
  :type 'string
  :group 'aider)

;;; GitHub Copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package codeium
  :vc (:url "https://github.com/Exafunction/codeium.el"
            :rev :newest
            :branch "main")
    :init
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; Set codeium/metadata/api_key from environment variable
    (exec-path-from-shell-copy-env "CODEIUM_API_KEY")
    (setq codeium/metadata/api_key (getenv "CODEIUM_API_KEY"))

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el"
            :rev :newest
            :branch "main")
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  :init
  (setq claude-code-terminal-backend 'vterm)
  :config
  (claude-code-mode))

(use-package aider
  :ensure t
  :config
  (exec-path-from-shell-copy-env "OPENAI_API_BASE")
  (exec-path-from-shell-copy-env "OPENAI_API_KEY")
  ;; (setq aider-args '("--model" "openai/claude-3.7-sonnet" "--no-auto-accept-architect" "--no-auto-commits"))
  (global-set-key (kbd "C-c e") 'aider-transient-menu))

(provide 'rvb-ai)
