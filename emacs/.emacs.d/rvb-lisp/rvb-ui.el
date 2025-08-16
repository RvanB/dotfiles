;; disable fringes
(fringe-mode 0)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'inkpot t)

;; Dirvish
(use-package dirvish
  :ensure t
  :config
  ;; Make project switch command dirvish
  (dirvish-override-dired-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package olivetti
  :ensure t)

;; Diminish minor modes
(use-package diminish
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init
  (with-eval-after-load "doom-modeline"
    (doom-modeline-def-modeline 'main
      '(bar buffer-info buffer-position)
      '(minor-modes major-mode)))
  (setq doom-modeline-env-version nil)
  (doom-modeline-mode 1))



;; Hide eldoc mode
(diminish 'eldoc-mode)

;;; Show line numbers in programming buffers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; use-package with package.el:

(add-hook 'after-init-hook (lambda ()
			     (org-agenda nil "a")
			     (delete-other-windows)
			     (olivetti-mode 1)))


(use-package page-break-lines
  :ensure t
  ;; :init
  ;; (setq page-break-lines-char ?-)
  :config
  (global-page-break-lines-mode))

;;; Make it count lines for correct line number width
(setq display-line-numbers-width-start t)

(set-frame-parameter nil 'ns-appearance 'light)
(set-frame-parameter nil 'ns-transparent-titlebar nil)

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-constructs t
	ef-themes-bold-constructs t
	ef-themes-italic-comments t))

;; Stimmung themes
(use-package stimmung-themes
  :ensure t)  

(require 'rvb-movement)

;;; Disable menu bar
(menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

;; Relative line numbers
(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

;; Make a clearer division between windows
;; (window-divider-mode)

;;; Set the font
;; To disable font smoothing:
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 0
(set-face-attribute 'default nil :font "Berkeley Mono Variable Z8XX46Z7 12")
(set-face-attribute 'variable-pitch nil :font "Berkeley Mono Variable Z8XX46Z7 12")

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
