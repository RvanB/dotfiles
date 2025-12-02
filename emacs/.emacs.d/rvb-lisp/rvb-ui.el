(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(use-package olivetti
  :ensure t)

;; Diminish minor modes
(use-package diminish
  :ensure t)

;; Hide eldoc mode
(diminish 'eldoc-mode)

(set-frame-parameter nil 'ns-appearance 'light)
(set-frame-parameter nil 'ns-transparent-titlebar t)

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-constructs t
	ef-themes-bold-constructs t
	ef-themes-italic-comments t))

;; Modus themes
(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-italic-comments t)
  )

;; Stimmung themes
(use-package stimmung-themes
  :ensure t)

;; Load preferred theme
(load-theme 'modus-operandi t)

(require 'rvb-movement)

;;; Disable menu bar
(menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

;; Make a clearer division between windows
(window-divider-mode)

;;; Set the font whenever frame is created
(add-hook 'after-make-frame-functions
		  (lambda (frame)
			(with-selected-frame frame
			  (set-face-attribute 'default nil :font "Berkeley Mono Variable Z8XX46Z7 14"))))
(set-face-attribute 'default nil :font "Berkeley Mono Variable Z8XX46Z7 14")

;; Ligatures
(use-package ligature
  :ensure t
  :config
  ;; Set ligatures for programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "~~>" "||=" "||>"
				       ":::" "::=" "=:=" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
				       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "-<<"
				       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
				       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "#_(" "..<"
				       "..." "+++" "/==" "///" "_|_" "&&" "^=" "~~" "~@" "~="
				       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
				       "[|" "]#" "::" ":=" ":>" ":<" "$>" "=>" "!=" "!!" ">:"
				       ">=" ">>" ">-" "-~" "-|" "->" "-<" "<~" "<*" "<|" "<:"
				       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
				       "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
				       "?=" "?." "??" "/*" "/=" "/>" "//" "~~" "(*" "*)"
				       "\\\\" "://"))
  (global-ligature-mode t))

;;; Magit todos
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(provide 'rvb-ui)
