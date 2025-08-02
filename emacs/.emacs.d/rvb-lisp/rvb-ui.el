;; disable fringes
(fringe-mode 0)

;; Diminish minor modes
(use-package diminish
  :ensure t)

;; Solaire
(use-package solaire-mode
  :ensure t
  :init
  (solaire-global-mode +1))

;; Modeline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (setq powerline-height 20)
  (setq powerline-default-separator 'wave))

;; Hide eldoc mode
(diminish 'eldoc-mode)

;;; Show line numbers in programming buffers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; use-package with package.el:

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-page-separator "\n
\n\n")
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  :config
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :ensure t
  :init
  (setq page-break-lines-char ?-)
  :config
  (global-page-break-lines-mode))

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
                        :font "Berkeley Mono Variable Z8XX46Z7 12"
                        :background (frame-parameter nil 'background-color)))
  (my-eldoc-box-update-faces)
  (advice-add 'load-theme :after (lambda (&rest _) (my-eldoc-box-update-faces))))

(define-advice eldoc-display-in-buffer (:after (&rest _) update-keymap)
  (with-current-buffer eldoc--doc-buffer
    (keymap-local-set "RET" #'eglot-open-link)
    ))

(set-frame-parameter nil 'ns-appearance 'light)
(set-frame-parameter nil 'ns-transparent-titlebar t)

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-constructs t
	ef-themes-bold-constructs t
	ef-themes-italic-comments t))

;; Stimmung themes
(use-package stimmung-themes
  :ensure t
  :config
  (stimmung-themes-load-light))

(require 'rvb-movement)

;;; Disable menu bar
;; (menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

;; Make a clearer division between windows
;; (window-divider-mode)

;;; Set the font
;; To disable font smoothing:
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 0
(set-face-attribute 'default nil :font "Berkeley Mono Variable Z8XX46Z7 14")
(set-face-attribute 'variable-pitch nil :font "Berkeley Mono Variable Z8XX46Z7 14")

;; Ligatures

(use-package ligature
  :ensure t

  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "~~>" "***" "||=" "||>"
				       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
				       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
				       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
				       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "#_(" "..<"
				       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
				       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
				       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
				       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
				       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
				       "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
				       "?=" "?." "??" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
				       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; (require 'rvb-dim)
;; (rvb-dim-mode)

;;; Magit todos
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(provide 'rvb-ui)
