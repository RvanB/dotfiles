(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; (add-hook 'prog-mode-hook 'hl-line-mode)

(use-package olivetti
  :ensure t)

;; Diminish minor modes
(use-package diminish
  :ensure t)

;; Hide eldoc mode
(diminish 'eldoc-mode)

(set-frame-parameter nil 'ns-transparent-titlebar nil)

(add-hook 'markdown-mode-hook 'mixed-pitch-mode)

;; (use-package vertico-posframe
;;   :ensure t
;;   :config
;;   (vertico-posframe-mode 1))

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-constructs t
	ef-themes-bold-constructs t
	ef-themes-italic-comments t))

(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)
)

;; Standard themes
(use-package standard-themes
  :ensure t)

;; Stimmung themes
(use-package stimmung-themes
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(setq custom-safe-themes t)

;; Theme toggling
(defvar current-theme 'light
  "Variable to track the current theme ('light or 'dark).")

(set-frame-parameter nil 'ns-appearance 'light)
(load-theme 'modus-operandi t)


;; (defun toggle-theme ()
;;   "Toggle between light and dark themes."
;;   (interactive)
;;   (cond
;;    ((eq current-theme 'light)
;;     (set-frame-parameter nil 'ns-appearance 'dark)
;;     (disable-theme 'modus-operandi)
;;     (load-theme 'inkpot t)
;;     (setq current-theme 'dark))
;;    ((eq current-theme 'dark)
;;     (set-frame-parameter nil 'ns-appearance 'light)
;;     (disable-theme 'inkpot)
;;     (load-theme 'modus-operandi t)
;;     (setq current-theme 'light))))


(require 'rvb-movement)

;;; Disable menu bar
(menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

(use-package mixed-pitch
  :ensure t)

(defcustom fontfamily nil
  "The font family to use as the default. If nil, uses the Emacs default."
  :type '(choice (const :tag "Use default font" nil)
                 (string :tag "Custom font family"))
  :group 'appearance)

(defcustom fontsize 14
  "The font size to use as the default. If nil, uses the Emacs default."
  :type '(choice (const :tag "Use default font size" nil)
                 (integer :tag "Custom font size"))
  :group 'appearance)

(defun update-default-font ()
  "Update the default font based on `fontfamily` and `fontsize`."
  (let ((font-spec (if (and fontfamily fontsize) 
                       (format "%s-%d" fontfamily fontsize)
                     nil)))
    (if font-spec
        (set-face-attribute 'default nil :font font-spec)
      (set-face-attribute 'default nil :font (face-attribute 'default :font)))))

(defun increase-font-size ()
  "Increase the font size by 1 and update the font."
  (interactive)
  (setq fontsize (if fontsize (1+ fontsize) 14)) ;; Start from 14 if nil
  (customize-save-variable 'fontsize fontsize)
  (update-default-font))

(defun decrease-font-size ()
  "Decrease the font size by 1 and update the font."
  (interactive)
  (setq fontsize (if fontsize (max 1 (1- fontsize)) 13)) ;; Start from 13 if nil
  (customize-save-variable 'fontsize fontsize)
  (update-default-font))

(defun list-available-fonts ()
  "Retrieve a list of available fonts on the current system."
  (delete-dups (sort (font-family-list) #'string-lessp)))

(defun select-font-family ()
  "Prompt the user to select a font family live via completion."
  (interactive)
  (let ((selected-font (completing-read "Select font family: " (list-available-fonts) nil t)))
    (setq fontfamily selected-font)
    (customize-save-variable 'fontfamily fontfamily)
    (update-default-font)))

(keymap-global-set "<f7>" 'select-font-family)
(keymap-global-set "<f8>" 'decrease-font-size)
(keymap-global-set "<f9>" 'increase-font-size)


;; Ensure the font is set initially
(update-default-font)

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

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

  ;; Optionally:
  (setq nerd-icons-corfu-mapping
	'((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; You can alternatively specify a function to perform the mapping,
          ;; use this when knowing the exact completion candidate is important.
          ;; Don't pass `:face' if the function already returns string with the
          ;; face property, though.
          (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; If you add an entry for t, the library uses that as fallback.
  ;; The default fallback (when it's not specified) is the ? symbol.

  ;; The Custom interface is also supported for tuning the variable above.
  )


;; Clearer separation between buffers
(window-divider-mode)

;;; Magit todos
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

;;; Window splitting preferences - prefer horizontal (side-by-side) splits
;; (setq split-height-threshold nil)  ; Never split vertically (top-bottom)
;; (setq split-width-threshold nil)     ; Always prefer horizontal splits (side-by-side)

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


(defgroup c64-frame nil
  "Make the current frame look a bit like a Commodore 64."
  :group 'frames)

(defcustom c64-frame-border-width 80
  "Width of the fake C64 border, in pixels."
  :type 'integer)

(defcustom c64-frame-border-color "#2b1f8f"
  "Color of the outer C64-style border."
  :type 'color)

(defvar c64-frame--saved-state nil
  "Alist mapping frames to their saved visual state.")

(defun c64-frame--save-state (frame)
  "Save FRAME settings so they can be restored later."
  (setf (alist-get frame c64-frame--saved-state nil nil #'eq)
        (list
         :internal-border-width (frame-parameter frame 'internal-border-width)
         :background-color      (face-background 'default frame t)
         :foreground-color      (face-foreground 'default frame t)
         :internal-border-color (face-background 'internal-border frame t)
         :mode-line-box         (face-attribute 'mode-line :box frame 'default)
         :menu-bar-lines        (frame-parameter frame 'menu-bar-lines)
         :tool-bar-lines        (frame-parameter frame 'tool-bar-lines)
         :vertical-scroll-bars  (frame-parameter frame 'vertical-scroll-bars))))

(defun c64-frame--restore-state (frame)
  "Restore FRAME settings previously saved by `c64-frame-mode'."
  (when-let ((state (alist-get frame c64-frame--saved-state nil nil #'eq)))
    (set-frame-parameter frame 'internal-border-width
                         (plist-get state :internal-border-width))
    (set-frame-parameter frame 'menu-bar-lines
                         (plist-get state :menu-bar-lines))
    (set-frame-parameter frame 'tool-bar-lines
                         (plist-get state :tool-bar-lines))
    (set-frame-parameter frame 'vertical-scroll-bars
                         (plist-get state :vertical-scroll-bars))

    ;; Restore frame-local face settings.
    (set-face-attribute 'default frame
                        :background (plist-get state :background-color)
                        :foreground (plist-get state :foreground-color))
    (set-face-attribute 'internal-border frame
                        :background (plist-get state :internal-border-color))
    (set-face-attribute 'mode-line frame
                        :box (plist-get state :mode-line-box))

    ;; Remove saved entry
    (setq c64-frame--saved-state
          (assq-delete-all frame c64-frame--saved-state))))

(defun c64-frame--apply (frame)
  "Apply the C64 look to FRAME."
  (c64-frame--save-state frame)

  ;; Big fake CRT border
  (set-frame-parameter frame 'internal-border-width c64-frame-border-width)

  ;; Optional cleanup
  (set-frame-parameter frame 'vertical-scroll-bars nil)
  (set-frame-parameter frame 'tool-bar-lines 0)
  (set-frame-parameter frame 'menu-bar-lines 0)

  ;; Frame-local face changes
  ;; (set-face-attribute 'default frame
  ;;                     :background c64-frame-screen-color)
  ;; (set-face-attribute 'internal-border frame
  ;;                     :background c64-frame-border-color)
  ;; (set-face-attribute 'mode-line frame
  ;;                     :box nil)

  )

;;;###autoload
(define-minor-mode c64-frame-mode
  "Toggle a Commodore-64-style border on the selected frame."
  :init-value nil
  :global nil
  :lighter " C64"
  (if c64-frame-mode
      (c64-frame--apply (selected-frame))
    (c64-frame--restore-state (selected-frame))))

(provide 'rvb-ui)
