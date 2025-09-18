;;; Detect indentation
(use-package dtrt-indent
  :ensure t
  :diminish 'dtrt-indent-mode)

(use-package adaptive-wrap
  :ensure t
  :config
  (setq adaptive-wrap-extra-indent 2) ; Adjust this value for desired extra indentation
  (adaptive-wrap-prefix-mode 1)
  :hook (prog-mode . adaptive-wrap-prefix-mode))

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode 1) ; Enable visual line mode globally


(dtrt-indent-global-mode 1)

(use-package expand-region
  :ensure t
  :config
  (setq expand-region-smart-cursor t)
  ;; Prioritize symbol over word
  (setq expand-region-exclude-text-mode-expansions nil)
  (setq er/try-expand-list
        '(er/mark-word
          er/mark-symbol
          er/mark-method-call
          er/mark-inside-pairs
          er/mark-outside-pairs
          er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-comment
          er/mark-defun
          er/mark-url
          er/mark-email
          er/mark-text-paragraph
          er/mark-text-sentence
          er/mark-paragraph
          er/mark-page)))

(use-package smart-hungry-delete
  :ensure t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	 ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	 ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package meow-tree-sitter
  :ensure t
  :config
  (meow-tree-sitter-register-defaults))

;;; Surround
(use-package surround
  :ensure t)

;;; Delete selection on type
(delete-selection-mode 1)

(use-package undo-fu
  :ensure t)

(provide 'rvb-editing)
