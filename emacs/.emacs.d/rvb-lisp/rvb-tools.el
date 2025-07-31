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

;;; Which key
(use-package which-key
  :ensure t
  :diminish 'which-key-mode
  :config
  (which-key-mode))

(use-package rainbow-mode
  :ensure t
  :diminish 'rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package keycast
  :ensure t)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("https://simonwillison.net/atom/everything/"))
  :config
  (global-set-key (kbd "C-c w") 'elfeed))

(use-package dap-mode
  :ensure t)

(provide 'rvb-tools)
