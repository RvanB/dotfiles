;; Disable fringes
(fringe-mode 0)

;;; Show line numbers in programming buffers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; Make it count lines for correct line number width
(setq display-line-numbers-width-start t)

;;; Configure eldoc box
(use-package eldoc-box
  :ensure t
  ;; :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :config
  (defun my-eldoc-box-update-faces ()
    "Update eldoc-box faces based on the current theme."
    (set-face-attribute 'eldoc-box-border nil
                        :background (frame-parameter nil 'foreground-color))
    (set-face-attribute 'eldoc-box-body nil
                        :font "Aporetic Sans Mono 14"
                        :background (frame-parameter nil 'background-color)))
  (my-eldoc-box-update-faces)
  (advice-add 'load-theme :after (lambda (&rest _) (my-eldoc-box-update-faces))))

(define-advice eldoc-display-in-buffer (:after (&rest _) update-keymap)
  (with-current-buffer eldoc--doc-buffer
    (keymap-local-set "RET" #'eglot-open-link)
    ))

(set-frame-parameter nil 'ns-appearance 'light)
(set-frame-parameter nil 'ns-transparent-titlebar nil)

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-constructs t
      ef-themes-bold-constructs t
      ef-themes-italic-comments t))

(use-package stimmung-themes :ensure t)

;; Apply theme
(load-theme 'modus-operandi t t)
(load-theme 'stimmung-themes-light t t)

(enable-theme 'modus-operandi)
(enable-theme 'stimmung-themes-light)

(require 'rvb-movement)
;; Cursor dependent on god mode
(setq god-mode-cursor-type 'box)
(setq normal-mode-cursor-type 'bar)

(defun rvb/set-cursor-according-to-mode ()
  (cond
   (god-local-mode
    (setq cursor-type god-mode-cursor-type))
   (t
    (setq cursor-type normal-mode-cursor-type))))

(defun rvb/keyboard-quit-and-god-mode ()
  (interactive)
  (god-mode-all 1)
  (keyboard-quit))

(add-hook 'post-command-hook 'rvb/set-cursor-according-to-mode)

;;; Disable menu bar
;; (menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

;;; Set the font
(set-face-attribute 'default nil :font "Aporetic Sans Mono 14")
(set-face-attribute 'variable-pitch nil :font "Aporetic Sans Mono 14")

(provide 'rvb-ui)
