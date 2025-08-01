
;;; gptel
(use-package gptel
  :ensure t
  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  ;; (setq gptel-model 'qwen2.5-coder:7b
  ;;       gptel-backend (gptel-make-ollama "Ollama"
  ;;                       :host "localhost:11434"
  ;;                       :stream t
  ;;                       :models '(qwen2.5-coder:7b)))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :bind
  (("C-c g m" . gptel-menu)))

;;; GitHub Copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")

  :config
  ;; (add-hook 'prog-mode-hook 'copilot-mode)
  )


(provide 'rvb-ai)
