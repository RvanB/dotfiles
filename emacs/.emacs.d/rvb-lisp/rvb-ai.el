
(defcustom ollama-ip "localhost:11434"
  "IP to connect to ollama"
  :type 'string)

;;; gptel
(use-package gptel
  :ensure t
  :config
  ;; (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (setq gptel-model 'qwen3.5:9b
        gptel-include-reasoning t
        gptel-backend (gptel-make-ollama "Ollama"
                        :host ollama-ip
                        :stream t
                        :models '(qwen3.5:9b)))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :bind
  (("C-c g m" . gptel-menu)))

(use-package gptel-magit
  :ensure t
  :after gptel
  :hook (magit-mode . gptel-magit-install))

(require 'project)

;;; GitHub Copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")

  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  )

(use-package agent-shell
  :ensure t)

;; ;;; Claude Code IDE
;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
;;   :config
;;   (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'rvb-ai)
