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

(provide 'rvb-tools)
