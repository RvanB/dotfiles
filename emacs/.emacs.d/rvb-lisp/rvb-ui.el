(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(use-package olivetti
  :ensure t)

;; Diminish minor modes
(use-package diminish
  :ensure t)

;; Hide eldoc mode
(diminish 'eldoc-mode)

(set-frame-parameter nil 'ns-transparent-titlebar t)

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-constructs t
	ef-themes-bold-constructs t
	ef-themes-italic-comments t))

;; Standard themes
(use-package standard-themes
  :ensure t)

;; Stimmung themes
(use-package stimmung-themes
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(setq custom-safe-themes t)

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes nil)
  (auto-dark-allow-osascript t)
  (auto-dark-polling-interval-seconds 5)
  :hook
  (auto-dark-dark-mode
   . (lambda ()
       (set-frame-parameter nil 'ns-appearance 'dark)
       (disable-theme 'sanityinc-tomorrow-day)
       (load-theme 'sanityinc-tomorrow-night)
       ))
  (auto-dark-light-mode
   . (lambda ()
       (set-frame-parameter nil 'ns-appearance 'light)
       (disable-theme 'sanityinc-tomorrow-night)
       (load-theme 'sanityinc-tomorrow-day)
       ))
  :config
  (auto-dark-mode))

(require 'rvb-movement)

;;; Disable menu bar
(menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

;; Make a clearer division between windows
(window-divider-mode)

;; Mixed pitch buffers
(use-package mixed-pitch
  :ensure t)

;; Set the font whenever frame is created
(add-hook 'after-make-frame-functions
		  (lambda (frame)
			(with-selected-frame frame
			  (set-face-attribute 'default nil :font "Berkeley Mono Variable Z8XX46Z7 16"))))
(set-face-attribute 'default nil :font "Berkeley Mono Variable Z8XX46Z7 16")

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

;;; Window splitting preferences - prefer horizontal (side-by-side) splits
(setq split-height-threshold nil)  ; Never split vertically (top-bottom)
(setq split-width-threshold 0)     ; Always prefer horizontal splits (side-by-side)

;;; Auto-select help and temporary windows
(setq help-window-select t)  ; Automatically select help windows

;; Make help windows easier to quit
(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "q") 'quit-window))

;; Auto-select other common temporary windows
(defun rvb/auto-select-window (buffer-or-name &rest _)
  "Automatically select certain temporary windows."
  (let ((buffer (get-buffer buffer-or-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (when (and window
                   (or (string-match-p "\\*Help\\|\\*info\\|\\*Apropos\\|\\*Messages\\|\\*Warnings\\|\\*Completions\\|\\*Occur\\|\\*grep\\|\\*compilation\\|\\*Backtrace\\*"
                                      (buffer-name buffer))))
          (select-window window))))))

(advice-add 'display-buffer :after #'rvb/auto-select-window)

(provide 'rvb-ui)
