;;; Detect indentation
(use-package dtrt-indent
  :ensure t)

(dtrt-indent-global-mode t)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  :config
  (setq expand-region-smart-cursor t)
  ;; Prioritize symbol over word
  (setq expand-region-exclude-text-mode-expansions nil)
  (setq er/try-expand-list
        '(er/mark-symbol
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

;;; Surround
(use-package surround
  :ensure t)

;;; Delete selection on type
(delete-selection-mode 1)


(use-package undo-fu
  :ensure t
  :config
  ;; Disable all current undo/redo keybinds
  (global-unset-key (kbd "s-z"))
  (global-unset-key (kbd "C-/"))
  (global-unset-key (kbd "C-?"))
  (global-unset-key (kbd "C-M-_"))

  (global-set-key (kbd "C-/") 'undo-fu-only-undo)
  (global-set-key (kbd "C-?") 'undo-fu-only-redo))


(provide 'rvb-editing)
