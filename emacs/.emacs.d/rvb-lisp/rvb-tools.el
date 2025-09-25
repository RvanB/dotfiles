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

;; Docker
(use-package docker
  :ensure t)

;;; Magit
(use-package magit
  :ensure t)

;;; Which key
(use-package which-key
  :ensure t
  :diminish 'which-key-mode
  :config
  (which-key-mode))

;;; Rainbow parentheses
(use-package rainbow-mode
  :ensure t
  :diminish 'rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package keycast
  :ensure t)

;;; RSS and Nano feeds
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("https://simonwillison.net/atom/everything/")))

;;; Debugger
(use-package dap-mode
  :ensure t)

(provide 'rvb-tools)
