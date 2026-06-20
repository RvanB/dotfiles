;;; PDFs
(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-view-use-scaling nil))

;;; graphviz
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;;; Magit
(use-package magit
  :ensure t)

;;; Debugger
(use-package dap-mode
  :ensure t)

(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

(defun rvb/dired-mouse-mark (event)
  "Toggle mark at mouse click."
  (interactive "e")
  (mouse-set-point event)
  (save-excursion
    (beginning-of-line)
    (if (eq (char-after) dired-marker-char)
        (dired-unmark 1)
      (dired-mark 1))))
(define-key dired-mode-map [mouse-3] 'rvb/dired-mouse-mark)

(defun rvb/notes-scratch ()
  "Open a temporary notes scratch buffer in `enriched-mode'.

Enriched mode stores formatting (faces, justification) as text
properties, so `font-lock-mode' is turned off to keep it from clobbering
that formatting."
  (interactive)
  (switch-to-buffer (get-buffer-create "*notes*"))
  (unless (derived-mode-p 'text-mode)
    (text-mode))
  (enriched-mode 1)
  (mixed-pitch-mode)
  (font-lock-mode -1)
  (electric-quote-local-mode)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1))

(provide 'rvb-tools)
