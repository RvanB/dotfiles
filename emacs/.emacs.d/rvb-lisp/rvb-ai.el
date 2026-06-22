
(defcustom ollama-ip "localhost:11434"
  "IP to connect to ollama"
  :type 'string)

;;; gptel
;; (use-package gptel
;;   :ensure t
;;   :config
;;   ;; (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
;;   (setq gptel-model 'qwen3.5:9b
;;         gptel-include-reasoning t
;;         gptel-backend (gptel-make-ollama "Ollama"
;;                         :host ollama-ip
;;                         :stream t
;;                         :models '(qwen3.5:9b)))
;;   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;;   (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
;;   :bind
;;   (("C-c g m" . gptel-menu)))

;; (use-package gptel-magit
;;   :ensure t
;;   :after gptel
;;   :hook (magit-mode . gptel-magit-install))

(require 'project)

;;; GitHub Copilot
;; (use-package copilot
;;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;;             :rev :newest
;;             :branch "main")

;;   :config
;;   (add-hook 'prog-mode-hook 'copilot-mode)
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;;   )

(use-package agent-shell
  :ensure t)

(defun rvb/run-apple-intelligence-shortcut (shortcut-name)
  "Run macOS Shortcut SHORTCUT-NAME.

Assumes the Shortcut reads from the clipboard and copies its result
back to the clipboard."
  (unless (executable-find "shortcuts")
    (user-error "Could not find the macOS shortcuts command"))

  (let ((buf (get-buffer-create "*apple-intelligence-shortcut*")))
    (with-current-buffer buf
      (erase-buffer))

    (let ((status
           (call-process "shortcuts" nil buf nil
                         "run" shortcut-name)))
      (unless (zerop status)
        (user-error "Shortcut failed: %s"
                    (with-current-buffer buf
                      (string-trim (buffer-string))))))))

(defun rvb/apple-intelligence-region (shortcut-name)
  "Run Apple Intelligence Shortcut SHORTCUT-NAME on the active region.

The Shortcut should read the clipboard and copy its result back to
the clipboard."
  (interactive "sShortcut name: ")
  (unless (use-region-p)
    (user-error "No region selected"))

  (let ((beg (region-beginning))
        (end (region-end)))
    ;; Copy Emacs region to macOS clipboard.
    (call-process-region beg end "pbcopy")

    ;; Run the named Shortcut.
    (rvb/run-apple-intelligence-shortcut shortcut-name)

    ;; Replace region with transformed clipboard contents.
    (let ((result
           (with-temp-buffer
             (call-process "pbpaste" nil t)
             (buffer-string))))
      (delete-region beg end)
      (insert result))))

(defun rvb/ai-rewrite ()
  "Rewrite the active region using Apple Intelligence."
  (interactive)
  (rvb/apple-intelligence-region "rewrite"))

(defun rvb/ai-to-list ()
  "Summarize the active region using Apple Intelligence."
  (interactive)
  (rvb/apple-intelligence-region "tolist"))

;; ;;; Claude Code IDE
;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
;;   :config
;;   (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'rvb-ai)
