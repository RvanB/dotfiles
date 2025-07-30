;;; Detect indentation
(use-package dtrt-indent
  :ensure t
  :diminish 'dtrt-indent-mode)

(dtrt-indent-global-mode t)

;;; Snippets
(use-package yasnippet
  :ensure t
  :diminish 'yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package consult-yasnippet
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
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



;; (use-package meow
;;   :ensure t
;;   :init
;;   (defun meow-setup ()
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   (meow-motion-define-key
;;    '("j" . meow-next)
;;    '("k" . meow-prev)
;;    '("<escape>" . ignore))
;;   (meow-leader-define-key
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet))
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("p" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("<escape>" . ignore)))
;;   :config
;;   (require 'meow)
;;   (meow-setup)
;;   (meow-global-mode 1))

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
