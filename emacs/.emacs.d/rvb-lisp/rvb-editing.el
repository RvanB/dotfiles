;;; Detect indentation
(use-package dtrt-indent
  :ensure t
  :init
  (setq dtrt-indent-verbosity 0)
  :config
  (dtrt-indent-global-mode 1))


;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (global-visual-line-mode 1) ; Enable visual line mode globally

;; Better BackWard Word
(use-package bbww
  :ensure t
  :diminish 'bbww-mode
  :config
  (bbww-mode 1)
  (bbww-init-global-bindings))

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

;;; Surround
(use-package surround
  :ensure t)

;;; Delete selection on type
(delete-selection-mode 1)

;;; Auto horizontal scroll after fill-paragraph
(defun rvb/scroll-to-left-after-fill ()
  "Scroll to the beginning of the line after filling paragraph."
  (when (and (boundp 'truncate-lines) truncate-lines)
    (set-window-hscroll (selected-window) 0)))

(advice-add 'fill-paragraph :after #'rvb/scroll-to-left-after-fill)

(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

(use-package evil
  :ensure t
  :init
  ;; Must be set before evil loads
  (setq evil-want-C-u-scroll nil)  ; Keep C-u as universal argument prefix
  (setq evil-want-C-d-scroll nil)  ; We'll bind C-d to our custom scroll
  (setq evil-undo-system 'undo-fu) ; Use undo-fu instead of built-in undo
  (setq evil-want-integration t)
  (setq evil-want-keybinding t)
  :config
  ;; (evil-mode 1)

  ;; Use custom scrolling commands
  (define-key evil-normal-state-map (kbd "C-u") 'kb/pixel-scroll-down)
  (define-key evil-normal-state-map (kbd "C-d") 'kb/pixel-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'kb/pixel-scroll-down)
  (define-key evil-visual-state-map (kbd "C-d") 'kb/pixel-scroll-up)

  ;; Keep some Emacs bindings in insert state
  (define-key evil-insert-state-map (kbd "C-a") 'rvb/back-to-indentation-or-beginning)
  (define-key evil-insert-state-map (kbd "C-e") 'rvb/move-to-code-end)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)

  ;; Use j and k to move by visual lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))

(provide 'rvb-editing)
