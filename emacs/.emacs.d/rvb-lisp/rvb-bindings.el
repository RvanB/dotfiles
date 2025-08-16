;; (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
;; (global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(keymap-global-set "s-f" 'forward-symbol)
(keymap-global-set "s-b" 'backward-symbol)

;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(keymap-global-set "C-c y" 'consult-yasnippet)

(keymap-global-unset "s-z")
(keymap-global-unset "C-/")
(keymap-global-unset "C-?")
(keymap-global-unset "C-M-_")

(keymap-global-set "C-/" 'undo-fu-only-undo)
(keymap-global-set "C-?" 'undo-fu-only-redo)

(use-package hydra
  :ensure t)

;;; Window navigation
(require 'magit)
(keymap-unset magit-status-mode-map "C-c C-w" t)
(with-eval-after-load 'magit
  ;; Unbind C-c C-w in all relevant magit maps
  (dolist (map (list magit-mode-map
                     magit-status-mode-map
                     magit-log-mode-map
                     magit-diff-mode-map
                     magit-revision-mode-map))
    (define-key map (kbd "C-c C-w") nil)))

(windmove-default-keybindings)
(defhydra window-nav (global-map "C-c C-w")
  "Navigate windows"
  ("f" windmove-right "right")
  ("b" windmove-left "left")
  ("p" windmove-up "up")
  ("n" windmove-down "down"))

(keymap-global-set "s-k" 'rvb/kill-buffer-and-close-window)

(keymap-global-set "C-=" 'er/expand-region)

(global-set-key [remap move-beginning-of-line] 'rvb/back-to-indentation-or-beginning)
(global-set-key [remap org-beginning-of-line] 'rvb/back-to-indentation-or-beginning)

(keymap-set isearch-mode-map "C-s" 'rvb/isearch-repeat-forward+)
(keymap-set isearch-mode-map "C-r" 'rvb/isearch-repeat-backward+)

(keymap-global-set "C-c l" 'rvb/isearch-visible-region)

(keymap-global-set "C-c t" nil)
(keymap-global-set "C-c t e" 'eat)
(keymap-global-set "C-c t p" 'eat-project)

(keymap-global-set "C-<tab>" 'next-buffer)
(keymap-global-set "C-<iso-lefttab>" 'previous-buffer)
(keymap-global-set "C-S-<tab>" 'previous-buffer)

(keymap-global-set "C-c a" 'org-agenda)

(provide 'rvb-bindings)
