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

(provide 'rvb-tools)
