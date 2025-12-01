(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(use-package doom-themes
  :ensure t)

(use-package modus-themes
  :ensure t)

;; Dired Preview
(use-package dired-preview
  :ensure t)

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
  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery t)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t)
  (with-eval-after-load "doom-modeline"
    (doom-modeline-def-modeline 'main
      '(bar buffer-info buffer-position)
      '(battery minor-modes major-mode)))
  (setq doom-modeline-env-version nil)
  (doom-modeline-mode 1))

;; (use-package perspective
;;   :ensure t
;;   ;; :bind
;;   ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
;;   :config
;;   (setq switch-to-prev-buffer-skip
;;       (lambda (win buff bury-or-kill)
;;         (not (persp-is-current-buffer buff))))
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
;;   :init
;;   (persp-mode))

;; (tab-bar-mode 1)

;; (use-package tabspaces
;;   ;; use this next line only if you also use straight, otherwise ignore it. 
;;   :ensure t
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   ;; sessions
;;   (tabspaces-session nil)
;;   (tabspaces-session-auto-restore nil)
;;   (tab-bar-new-tab-choice "*scratch*"))

;; (use-package otpp
;;   :ensure t
;;   :after project
;;   :custom
;;   ;; Don't reconnect/create tab immediately when selecting project
;;   (otpp-reconnect-tab nil)
;;   :init
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode 1)
;;   :config
;;   ;; Remove the advice on project-current to prevent immediate tab switching
;;   (advice-remove 'project-current #'otpp--project-current-a))

;; (tab-bar-mode 1)

;; Create project-specific scratch buffer for new tabs
;; (defun rvb/create-project-scratch-buffer ()
;;   "Create a project-specific scratch buffer for the current tab."
;;   (let* ((project-root (otpp-get-tab-root-dir))
;;          (project-name (when project-root
;;                          (file-name-nondirectory (directory-file-name project-root))))
;;          (scratch-name (if project-name
;;                            (format "*scratch:%s*" project-name)
;;                          "*scratch*"))
;;          (scratch-buffer (get-buffer-create scratch-name)))
;;     (with-current-buffer scratch-buffer
;;       (unless (eq major-mode 'lisp-interaction-mode)
;;         (lisp-interaction-mode))
;;       (when (= (buffer-size) 0)
;;         (insert (substitute-command-keys
;;                  ";; This buffer is for notes you don't want to save.\n\
;; ;; If you want to create a file, visit that file with \\[find-file],\n\
;; ;; then enter the text in that file's own buffer.\n\n"))))
;;     (switch-to-buffer scratch-buffer)))

;; (add-hook 'tab-bar-tab-post-open-functions
;;           (lambda (_tab) (rvb/create-project-scratch-buffer)))

;; Hide eldoc mode
(diminish 'eldoc-mode)

;;; Show line numbers in programming buffers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; use-package with package.el:

;; (add-hook 'after-init-hook (lambda ()
;; 			     (org-agenda nil "a")
;; 			     (delete-other-windows)
;; 			     (olivetti-mode 1)))


(use-package page-break-lines
  :ensure t
  ;; :init
  ;; (setq page-break-lines-char ?-)
  :config
  (global-page-break-lines-mode))

;;; Make it count lines for correct line number width
;; (setq display-line-numbers-width-start t)

(set-frame-parameter nil 'ns-appearance 'dark)
(set-frame-parameter nil 'ns-transparent-titlebar nil)

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-constructs t
	ef-themes-bold-constructs t
	ef-themes-italic-comments t))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-italic-comments t)

;; (load-theme 'modus-operandi t)
;; (load-theme 'inkpot t)

(use-package standard-themes
  :ensure t)

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
;; (add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

;; Make a clearer division between windows
;; (window-divider-mode)

;;; Set the font
;; To disable font smoothing:
;; defaults write org.gnu.Emacs AppleFontSmoothing -int 0

(set-face-attribute 'default nil :font "Berkeley Mono Variable Z8XX46Z7 14")
(set-face-attribute 'variable-pitch nil :font "Helvetica Neue 14")

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
				       ":::" "::=" "=:=" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
				       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "-<<"
				       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
				       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "#_(" "..<"
				       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
				       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
				       "[|" "]#" "::" ":=" ":>" ":<" "$>" "=>" "!=" "!!" ">:"
				       ">=" ">>" ">-" "-~" "-|" "->" "-<" "<~" "<*" "<|" "<:"
				       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
				       "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
				       "?=" "?." "??" "/*" "/=" "/>" "//" "~~" "(*" "*)"
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
