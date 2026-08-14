(keymap-global-set "s-f" 'forward-symbol)
(keymap-global-set "s-b" 'backward-symbol)

(keymap-global-unset "s-z")
(keymap-global-unset "C-/")
(keymap-global-unset "C-?")
(keymap-global-unset "C-M-_")
(keymap-global-unset "C-_")

(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)

(keymap-global-set "C-/" 'undo-fu-only-undo)
(keymap-global-set "C-?" 'undo-fu-only-redo)

(use-package hydra
  :ensure t)

(require 'magit)
(keymap-unset magit-status-mode-map "C-c C-w" t)
(with-eval-after-load 'magit
  ;; Unbind C-c C-w in all relevant magit maps
  (dolist (map (list magit-mode-map
                     magit-status-mode-map
                     magit-log-mode-map
                     magit-diff-mode-map
                     magit-revision-mode-map))
    (define-key map (kbd "C-c w") nil)))

(which-key-mode)

(windmove-default-keybindings)

(keymap-global-set "s-k" 'rvb/kill-buffer-and-close-window)

(keymap-global-set "C-=" 'er/expand-region)

(global-set-key [remap move-beginning-of-line] 'rvb/back-to-indentation-or-beginning)
(global-set-key [remap org-beginning-of-line] 'rvb/back-to-indentation-or-beginning)

(keymap-set isearch-mode-map "C-s" 'rvb/isearch-repeat-forward+)
(keymap-set isearch-mode-map "C-r" 'rvb/isearch-repeat-backward+)

(keymap-global-set "C-c j" 'rvb/isearch-visible-region)

(keymap-global-set "C-<tab>" 'next-buffer)
(keymap-global-set "C-<iso-lefttab>" 'previous-buffer)
(keymap-global-set "C-S-<tab>" 'previous-buffer)

(keymap-global-set "C-c a" 'org-agenda)

(keymap-global-set "C-x C-b" 'ibuffer)

(keymap-global-set "C-c n" 'display-line-numbers-mode)

;;; UI settings menu
(keymap-global-set "<f6>" 'rvb/ui-menu)

;;; Projects
(keymap-global-set "s-p" 'project-switch-project)

;;; Multi-repo features
(keymap-global-set "C-c f" 'rvb-feature-dispatch)

;;; Zoom the current window to the whole frame, and back again
(keymap-global-set "s-<return>" 'rvb/toggle-full-frame)

;;; Make C-` shorthand for C-x ` (next-error), and C-~ for previous error
(keymap-global-set "C-`" 'next-error)
(keymap-global-set "C-~" 'previous-error)

(keymap-set prog-mode-map "s-u" 'revert-buffer)

(keymap-global-unset "M-s d")
(keymap-global-set "M-s d" 'consult-fd)

(provide 'rvb-bindings)
