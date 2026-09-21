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

(require 'project)

(defun rvb/copilot-mode-soon ()
  "Turn on `copilot-mode' in this buffer once Emacs is next idle.

Not straight away: the first buffer to turn it on starts Copilot's
server, most of a second, and a file should open without waiting for
that.  Not at all in *scratch*, whose Lisp Interaction mode is a
programming mode -- which cost every startup that same second."
  (unless (derived-mode-p 'lisp-interaction-mode)
    (run-with-idle-timer
     0.5 nil
     (lambda (buffer)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (copilot-mode 1))))
     (current-buffer))))

(use-package copilot
  :ensure t
  :hook (prog-mode . rvb/copilot-mode-soon)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)))

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

(provide 'rvb-ai)
