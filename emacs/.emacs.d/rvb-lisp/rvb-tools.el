(require 'rvb-ai)

(require 'rvb-terminals)

;;; PDFs
(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-view-use-scaling nil))

;;; Magit
(use-package magit
  :ensure t)

;;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package keycast
  :ensure t)

(provide 'rvb-tools)
