(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'transient)

;; (add-hook 'prog-mode-hook 'hl-line-mode)

(use-package annotate
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'annotate-mode))

(use-package olivetti
  :ensure t)

;; Diminish minor modes
(use-package diminish
  :ensure t)

;; Hide eldoc mode
(diminish 'eldoc-mode)

(use-package vertico-posframe
  :ensure t
  :config
  (vertico-posframe-mode 1))

(use-package transient-posframe
  :ensure t
  :config
  (transient-posframe-mode))

(use-package hydra-posframe
  :ensure nil
  :vc (:url "https://github.com/Ladicle/hydra-posframe"
            :rev :newest)
  :hook (after-init . hydra-posframe-mode))

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; ef themes
(use-package ef-themes
  :ensure t
  :init
  (setq ef-themes-italic-comments t))

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
(defcustom rvb-current-theme 'light
  "The active RVB theme appearance."
  :type '(choice (const :tag "Light" light)
                 (const :tag "Dark" dark))
  :group 'appearance)

(defcustom rvb-light-theme 'modus-operandi
  "Theme loaded by `rvb/use-light-theme'."
  :type 'symbol
  :group 'appearance)

(defcustom rvb-dark-theme 'modus-vivendi
  "Theme loaded by `rvb/use-dark-theme'."
  :type 'symbol
  :group 'appearance)

(declare-function consult-theme "consult" (theme))

(defun rvb/set-frame-alist-parameter (alist-symbol parameter value)
  "Set PARAMETER to VALUE in frame alist ALIST-SYMBOL."
  (set alist-symbol
       (cons (cons parameter value)
             (assq-delete-all parameter (symbol-value alist-symbol)))))

(defun rvb/set-default-frame-parameter (parameter value)
  "Set default frame PARAMETER to VALUE for future frames."
  (rvb/set-frame-alist-parameter 'default-frame-alist parameter value))

(defun rvb/set-initial-frame-parameter (parameter value)
  "Set initial frame PARAMETER to VALUE for the startup frame."
  (rvb/set-frame-alist-parameter 'initial-frame-alist parameter value))

(defun rvb/set-frame-parameter-defaults (parameter value)
  "Set PARAMETER to VALUE for initial and future frames."
  (rvb/set-default-frame-parameter parameter value)
  (rvb/set-initial-frame-parameter parameter value))

(defun rvb/load-theme-preset (theme appearance)
  "Load THEME and set frame APPEARANCE."
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (load-theme theme t)
  (setq rvb-current-theme appearance)
  (customize-save-variable 'rvb-current-theme appearance)
  (rvb/set-frame-parameter-defaults 'ns-appearance appearance)
  (dolist (frame (frame-list))
    (rvb/apply-frame-appearance frame))
  ;; Loading a theme re-sets `line-number' and other faces, so re-assert the
  ;; page chrome styling on top of the freshly loaded theme.
  (when (and (bound-and-true-p rvb/ui-page-chrome-mode)
             (fboundp 'rvb/ui-page-chrome-refresh))
    (rvb/ui-page-chrome-refresh)))

(defun rvb/use-light-theme ()
  "Load `rvb-light-theme'."
  (interactive)
  (rvb/load-theme-preset rvb-light-theme 'light))

(defun rvb/use-dark-theme ()
  "Load `rvb-dark-theme'."
  (interactive)
  (rvb/load-theme-preset rvb-dark-theme 'dark))

(defun rvb/toggle-theme ()
  "Toggle between the configured light and dark themes."
  (interactive)
  (if (eq rvb-current-theme 'light)
      (rvb/use-dark-theme)
    (rvb/use-light-theme)))

(defun rvb/ensure-theme-loaded ()
  "Load the configured theme matching `rvb-current-theme' when needed."
  (unless custom-enabled-themes
    (if (eq rvb-current-theme 'dark)
        (rvb/use-dark-theme)
      (rvb/use-light-theme))))

(defun rvb/apply-frame-appearance (&optional frame)
  "Apply frame-specific appearance settings to FRAME."
  (let ((target-frame (or frame (selected-frame))))
    (when (display-graphic-p target-frame)
      (set-frame-parameter target-frame 'ns-transparent-titlebar nil)
      (set-frame-parameter target-frame 'ns-appearance rvb-current-theme))))

(use-package ns-auto-titlebar
  :ensure t
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))


(require 'rvb-movement)

;;; Disable menu bar
;; (menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

(defvar mixed-pitch-fixed-pitch-faces)
(defvar mixed-pitch-set-height)
(defvar mixed-pitch-mode)
(defvar markdown-hide-markup)

(defconst rvb/markdown-fixed-pitch-faces
  '(markdown-code-face
    markdown-inline-code-face
    markdown-pre-face
    markdown-language-info-face
    markdown-language-keyword-face)
  "Markdown faces that should stay fixed-pitch in mixed-pitch buffers.")

(defun rvb/markdown-face-p (face)
  "Return non-nil when FACE belongs to markdown-mode."
  (and (symbolp face)
       (string-prefix-p "markdown-" (symbol-name face))))

(defun rvb/configure-markdown-mixed-pitch-faces ()
  "Keep only Markdown code faces fixed-pitch for `mixed-pitch-mode'."
  (setq mixed-pitch-fixed-pitch-faces
        (append (delq nil
                      (mapcar (lambda (face)
                                (unless (rvb/markdown-face-p face)
                                  face))
                              mixed-pitch-fixed-pitch-faces))
                rvb/markdown-fixed-pitch-faces)))

(defun rvb/markdown-hide-markup ()
  "Hide Markdown formatting markup in the current buffer."
  (setq-local markdown-hide-markup t)
  (add-to-invisibility-spec 'markdown-markup))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . rvb/markdown-hide-markup)
         (gfm-mode . rvb/markdown-hide-markup)))

(use-package mixed-pitch
  :ensure t
  :init
  (setq mixed-pitch-set-height t)
  :hook ((markdown-mode . mixed-pitch-mode)
         (gfm-mode . mixed-pitch-mode))
  :config
  (rvb/configure-markdown-mixed-pitch-faces))

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

(defcustom variable-fontfamily nil
  "The font family to use for variable-pitch text.
If nil, use the Emacs default variable-pitch font family."
  :type '(choice (const :tag "Use default variable-pitch font" nil)
                 (string :tag "Custom font family"))
  :group 'appearance)

(defcustom variable-fontsize nil
  "The font size to use for variable-pitch text.
If nil, use the Emacs default variable-pitch font size."
  :type '(choice (const :tag "Use default variable-pitch font size" nil)
                 (integer :tag "Custom font size"))
  :group 'appearance)

(defun rvb/default-font-spec ()
  "Build the configured default font string."
  (when (and fontfamily fontsize)
    (format "%s-%d" fontfamily fontsize)))

(defun rvb/refresh-mixed-pitch-buffers ()
  "Refresh active `mixed-pitch-mode' buffers after a font change."
  (when (fboundp 'mixed-pitch-mode)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (boundp 'mixed-pitch-mode)
                   mixed-pitch-mode)
          (mixed-pitch-mode -1)
          (mixed-pitch-mode 1))))))

(defun update-default-font (&optional frame)
  "Update the default font for FRAME and future frames."
  (let ((font-spec (rvb/default-font-spec)))
    (if font-spec
        (progn
          (rvb/set-default-frame-parameter 'font font-spec)
          (when (display-graphic-p frame)
            (set-face-attribute 'default frame :font font-spec)))
      (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
      (when (display-graphic-p frame)
        (set-face-attribute
         'default
         frame
         :font (face-attribute 'default :font frame 'default)))))
  (rvb/refresh-mixed-pitch-buffers))

(defun update-variable-pitch-font (&optional frame)
  "Update the variable-pitch font for FRAME and future frames."
  (when (display-graphic-p frame)
    (set-face-attribute
     'variable-pitch
     frame
     :family (or variable-fontfamily 'unspecified)
     :height (if variable-fontsize (* 10 variable-fontsize) 'unspecified)))
  (rvb/refresh-mixed-pitch-buffers))

(defun rvb/initialize-ui (&optional frame)
  "Apply the configured UI to FRAME, or all live frames."
  (rvb/set-frame-parameter-defaults 'ns-transparent-titlebar t)
  (rvb/set-frame-parameter-defaults 'ns-appearance rvb-current-theme)
  (rvb/ensure-theme-loaded)
  (dolist (live-frame (if frame (list frame) (frame-list)))
    (rvb/apply-frame-appearance live-frame)
    (update-default-font live-frame)
    (update-variable-pitch-font live-frame)))

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

(defun increase-variable-pitch-font-size ()
  "Increase the variable-pitch font size by 1 and update the font."
  (interactive)
  (setq variable-fontsize (if variable-fontsize
                              (1+ variable-fontsize)
                            (or fontsize 14)))
  (customize-save-variable 'variable-fontsize variable-fontsize)
  (update-variable-pitch-font))

(defun decrease-variable-pitch-font-size ()
  "Decrease the variable-pitch font size by 1 and update the font."
  (interactive)
  (setq variable-fontsize (if variable-fontsize
                              (max 1 (1- variable-fontsize))
                            (max 1 (1- (or fontsize 14)))))
  (customize-save-variable 'variable-fontsize variable-fontsize)
  (update-variable-pitch-font))

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

(defun select-variable-pitch-font-family ()
  "Prompt the user to select a variable-pitch font family via completion."
  (interactive)
  (let ((selected-font
         (completing-read "Select variable-pitch font family: "
                          (list-available-fonts)
                          nil
                          t)))
    (setq variable-fontfamily selected-font)
    (customize-save-variable 'variable-fontfamily variable-fontfamily)
    (update-variable-pitch-font)))

(defclass rvb-theme-variable (transient-lisp-variable) ()
  "A theme variable whose reader uses `consult-theme' for previews.")

(defclass rvb-font-family-variable (transient-lisp-variable) ()
  "A font family variable with completion over installed fonts.")

(defclass rvb-font-size-variable (transient-lisp-variable) ()
  "A font size variable read as a number.")

(defun rvb/customize-set-variable (variable value)
  "Set and persist VARIABLE to VALUE."
  (set variable value)
  (customize-save-variable variable value))

(defun rvb/set-theme-variable (variable value)
  "Set and persist theme VARIABLE to VALUE, reloading it when it is active.

Changing the light theme reloads immediately when the light appearance is
current, and likewise for the dark theme, so the new choice takes effect
without toggling appearance."
  (rvb/customize-set-variable variable value)
  (when (or (and (eq variable 'rvb-light-theme) (eq rvb-current-theme 'light))
            (and (eq variable 'rvb-dark-theme) (eq rvb-current-theme 'dark)))
    (rvb/load-theme-preset value rvb-current-theme)))

(defun rvb/set-font-variable (variable value)
  "Set, persist, and apply font VARIABLE to VALUE."
  (rvb/customize-set-variable variable value)
  (pcase variable
    ((or 'fontfamily 'fontsize) (update-default-font))
    ((or 'variable-fontfamily 'variable-fontsize) (update-variable-pitch-font))))

(defun rvb/restore-enabled-themes (themes)
  "Restore THEMES after temporarily previewing other themes."
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (dolist (theme (reverse themes))
    (load-theme theme t)))

(cl-defmethod transient-infix-read ((obj rvb-theme-variable))
  "Preview themes with `consult-theme' and return the chosen theme.
The active theme is restored before returning to the settings menu."
  (let ((enabled-themes (copy-sequence custom-enabled-themes))
        selected-theme)
    (unwind-protect
        (progn
          (call-interactively #'consult-theme)
          (setq selected-theme (car custom-enabled-themes)))
      (rvb/restore-enabled-themes enabled-themes))
    selected-theme))

(cl-defmethod transient-infix-read ((obj rvb-font-family-variable))
  "Read a font family for OBJ using completion."
  (completing-read (format "Set %s: " (oref obj description))
                   (list-available-fonts) nil t
                   (symbol-value (oref obj variable))))

(cl-defmethod transient-infix-read ((obj rvb-font-size-variable))
  "Read a point size for OBJ."
  (read-number (format "Set %s: " (oref obj description))
               (or (symbol-value (oref obj variable)) 14)))

(transient-define-infix rvb/ui-light-theme ()
  :class 'rvb-theme-variable
  :variable 'rvb-light-theme
  :description "Light theme"
  :set-value #'rvb/set-theme-variable)

(transient-define-infix rvb/ui-dark-theme ()
  :class 'rvb-theme-variable
  :variable 'rvb-dark-theme
  :description "Dark theme"
  :set-value #'rvb/set-theme-variable)

(transient-define-infix rvb/ui-fixed-font-family ()
  :class 'rvb-font-family-variable
  :variable 'fontfamily
  :description "Fixed-pitch family"
  :set-value #'rvb/set-font-variable)

(transient-define-infix rvb/ui-fixed-font-size ()
  :class 'rvb-font-size-variable
  :variable 'fontsize
  :description "Fixed-pitch size"
  :set-value #'rvb/set-font-variable)

(transient-define-infix rvb/ui-variable-font-family ()
  :class 'rvb-font-family-variable
  :variable 'variable-fontfamily
  :description "Variable-pitch family"
  :set-value #'rvb/set-font-variable)

(transient-define-infix rvb/ui-variable-font-size ()
  :class 'rvb-font-size-variable
  :variable 'variable-fontsize
  :description "Variable-pitch size"
  :set-value #'rvb/set-font-variable)

(defvar rvb/ui-page-chrome--saved-header-lines nil)
(defvar rvb/ui-page-chrome--saved-mode-line-parameters nil)
(defvar rvb/ui-page-chrome--saved-line-number-faces nil
  "Alist mapping (FRAME . FACE) to the line-number background to restore.")

(defconst rvb/ui-page-chrome--line-number-gutter-extra-columns 2
  "Native line-number gutter columns not reported by `line-number-display-width'.

Emacs draws a separating space on each side of the line numbers that
`line-number-display-width' omits.  Counting both keeps the header band
aligned with the buffer text rather than with the gap before it.")

(defun rvb/ui-page-chrome--window-p (window)
  "Return non-nil when WINDOW should display RVB page chrome."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (frame-parameter (window-frame window) 'parent-frame))))

(defun rvb/ui-page-chrome--save-header-line (buffer)
  "Remember BUFFER's header line before page chrome changes it."
  (unless (assq buffer rvb/ui-page-chrome--saved-header-lines)
    (push (list buffer
                (local-variable-p 'header-line-format buffer)
                (buffer-local-value 'header-line-format buffer))
          rvb/ui-page-chrome--saved-header-lines)))

(defun rvb/ui-page-chrome--save-mode-line-parameter (window)
  "Remember WINDOW's mode-line parameter before page chrome changes it."
  (unless (assq window rvb/ui-page-chrome--saved-mode-line-parameters)
    (push (cons window (window-parameter window 'mode-line-format))
          rvb/ui-page-chrome--saved-mode-line-parameters)))

(defun rvb/ui-page-chrome--margins (window)
  "Return WINDOW's effective text margins, treating nil as zero.

This independently accounts for line-number and fringe columns so the
header aligns with text, without depending on book-mode implementation."
  (let* ((margins (window-margins window))
         (frame (window-frame window))
         (fringe-pixels (car (window-fringes window)))
         ;; The left fringe always precedes the text, so count it whether or
         ;; not line numbers are shown.
         (fringe-columns (ceiling (/ (float fringe-pixels)
                                     (frame-char-width frame))))
         (line-number-columns
          (if (and (boundp 'display-line-numbers)
                   (buffer-local-value 'display-line-numbers
                                       (window-buffer window)))
              (+ (with-selected-window window
                   (line-number-display-width))
                 rvb/ui-page-chrome--line-number-gutter-extra-columns)
            0)))
    (cons (+ (or (car margins) 0) fringe-columns line-number-columns)
          (or (cdr margins) 0))))

(defface rvb/ui-page-chrome-header
  '((t :inherit header-line
       :background "white" :foreground "black"
       ;; `:box' can't draw a top-only rule (it always adds left/right and
       ;; bottom edges), so use overline/underline for top- and bottom-only
       ;; rules spanning the band.
       :overline "black"
       ;; `:position t' drops the rule to the font's descent (lower than the
       ;; default underline position), giving the text breathing room.
       :underline (:color "black" :position t)))
  "Face for the RVB page-chrome top header band.")

(defun rvb/ui-page-chrome--pad (columns char-width char-height)
  "Return a COLUMNS-wide body-colored pad sized in absolute pixels.

Both the width (CHAR-WIDTH per column) and the height (CHAR-HEIGHT) are
pinned via the `space' display spec, so the pad stays correct in
`mixed-pitch-mode' buffers.  There the `default' face is remapped to a
taller variable-pitch font; a stretch glyph would otherwise inherit that
face's width and height, mis-aligning the band and inflating the header
line.  The `default' face is kept only for its background color."
  (propertize " " 'face 'default
              'display `(space :width (,(* (max 0 columns) char-width))
                               :height (,char-height))))

(defun rvb/ui-page-chrome--band (window content face)
  "Render CONTENT within WINDOW's margins using FACE for its central band.

The band is forced to `fixed-pitch' so its column arithmetic
\(`string-width', `truncate-string-to-width') matches what is actually
drawn, and the side padding is sized in pixels.  Together this keeps the
header aligned with the body text in `mixed-pitch-mode' buffers."
  (pcase-let* ((frame (window-frame window))
               (char-width (frame-char-width frame))
               (char-height (frame-char-height frame))
               ;; The frame's true default :height, ignoring any buffer-local
               ;; remapping.  In `mixed-pitch-mode' the buffer's `default' face
               ;; is remapped to a taller variable-pitch font, which would
               ;; otherwise inflate the band (and so the whole header line).
               (default-height (face-attribute 'default :height frame))
               (band-face (if (integerp default-height)
                              (list face 'fixed-pitch (list :height default-height))
                            (list face 'fixed-pitch)))
               (`(,left . ,right) (rvb/ui-page-chrome--margins window))
               (width (max 0 (- (window-total-width window) left right)))
               (content (truncate-string-to-width content width))
               (band (concat content
                             (make-string (max 0 (- width (string-width content)))
                                          ?\s))))
    (add-face-text-property 0 (length band) band-face nil band)
    (concat (rvb/ui-page-chrome--pad left char-width char-height)
            band
            (rvb/ui-page-chrome--pad right char-width char-height))))

(defun rvb/ui-page-chrome--header-content (window width)
  "Return WINDOW's file/status header, fitted into WIDTH columns."
  (with-current-buffer (window-buffer window)
    (let* ((file buffer-file-name)
           (path (if file (abbreviate-file-name file) (buffer-name)))
           (status (format-mode-line
                    '("%e" mode-line-front-space
                      (:propertize
                       ("" mode-line-mule-info mode-line-client
                        mode-line-modified mode-line-remote
                        mode-line-window-dedicated)
                       display (min-width (6.0))))
                    nil window))
           (gap (max 2 (- width (string-width path) (string-width status) 2))))
      (truncate-string-to-width
       (concat " " path (make-string gap ?\s) status " ") width))))

(defun rvb/ui-page-chrome--header-line-format (window)
  "Return WINDOW's margin-limited top file header."
  (let* ((margins (rvb/ui-page-chrome--margins window))
         (width (max 0 (- (window-total-width window)
                          (car margins) (cdr margins)))))
    (rvb/ui-page-chrome--band
     window (rvb/ui-page-chrome--header-content window width)
     'rvb/ui-page-chrome-header)))

(defun rvb/ui-page-chrome--apply-window (window)
  "Apply page chrome to WINDOW."
  (when (rvb/ui-page-chrome--window-p window)
    (let ((buffer (window-buffer window)))
      (rvb/ui-page-chrome--save-header-line buffer)
      (rvb/ui-page-chrome--save-mode-line-parameter window)
      (with-current-buffer buffer
        (setq-local header-line-format
                    '((:eval (rvb/ui-page-chrome--header-line-format
                               (selected-window))))))
      (set-window-parameter
       window 'mode-line-format 'none))))

(defun rvb/ui-page-chrome--apply-line-number-faces (frame)
  "Drop the line-number backgrounds in FRAME so numbers blend into the body.

A theme's real background is remembered as the restore baseline.  The
`unspecified' value page chrome sets itself is never captured, and a
later theme load (which re-sets a real background) refreshes the
baseline, so restoring always reverts to the active theme."
  (dolist (face '(line-number line-number-current-line))
    (let ((current (face-attribute face :background frame)))
      (unless (eq current 'unspecified)
        (setf (alist-get (cons frame face)
                         rvb/ui-page-chrome--saved-line-number-faces
                         nil nil #'equal)
              current))
      (set-face-attribute face frame :background 'unspecified))))

(defun rvb/ui-page-chrome--restore-line-number-faces ()
  "Restore line-number face backgrounds changed by page chrome."
  (pcase-dolist (`((,frame . ,face) . ,background)
                 rvb/ui-page-chrome--saved-line-number-faces)
    (when (frame-live-p frame)
      (set-face-attribute face frame :background background)))
  (setq rvb/ui-page-chrome--saved-line-number-faces nil))

(defun rvb/ui-page-chrome-refresh ()
  "Apply RVB page chrome to every ordinary window."
  (interactive)
  (when rvb/ui-page-chrome-mode
    (dolist (frame (frame-list))
      (unless (frame-parameter frame 'parent-frame)
        (rvb/ui-page-chrome--apply-line-number-faces frame)
        (walk-windows #'rvb/ui-page-chrome--apply-window 'no-minibuf frame)))))

(defun rvb/ui-page-chrome--restore ()
  "Restore header and mode lines changed by RVB page chrome."
  (dolist (entry rvb/ui-page-chrome--saved-mode-line-parameters)
    (when (window-live-p (car entry))
      (set-window-parameter (car entry) 'mode-line-format (cdr entry))))
  (setq rvb/ui-page-chrome--saved-mode-line-parameters nil)
  (dolist (entry rvb/ui-page-chrome--saved-header-lines)
    (pcase-let ((`(,buffer ,was-local ,header-line) entry))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if was-local
              (setq-local header-line-format header-line)
            (kill-local-variable 'header-line-format))))))
  (setq rvb/ui-page-chrome--saved-header-lines nil)
  (rvb/ui-page-chrome--restore-line-number-faces))

(defun rvb/ui-page-chrome--window-change (&rest _)
  "Refresh page chrome after window or frame geometry changes."
  (rvb/ui-page-chrome-refresh))

(define-minor-mode rvb/ui-page-chrome-mode
  "Show a margin-limited file/status header and hide the bottom mode line."
  :global t
  :lighter nil
  (if rvb/ui-page-chrome-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  #'rvb/ui-page-chrome--window-change)
        (add-hook 'window-size-change-functions
                  #'rvb/ui-page-chrome--window-change)
        (add-hook 'after-make-frame-functions
                  #'rvb/ui-page-chrome--window-change)
        (rvb/ui-page-chrome-refresh))
    (remove-hook 'window-configuration-change-hook
                 #'rvb/ui-page-chrome--window-change)
    (remove-hook 'window-size-change-functions
                 #'rvb/ui-page-chrome--window-change)
    (remove-hook 'after-make-frame-functions
                 #'rvb/ui-page-chrome--window-change)
    (rvb/ui-page-chrome--restore)))

(transient-define-prefix rvb/ui-menu ()
  "Open the UI settings menu."
  ["Actions"
   ("t" "Toggle light/dark theme" rvb/toggle-theme)
   ("p" "Toggle page chrome" rvb/ui-page-chrome-mode)]
  ["Themes"
   ("l" rvb/ui-light-theme)
   ("d" rvb/ui-dark-theme)]
  ["Fonts"
   ("f" rvb/ui-fixed-font-family)
   ("F" rvb/ui-fixed-font-size)
   ("v" rvb/ui-variable-font-family)
   ("V" rvb/ui-variable-font-size)]
  ["Custom settings"
   ("a" "Appearance settings" (lambda () (interactive) (customize-group 'appearance)))])

;; Ensure the font is set initially
(rvb/initialize-ui)
(add-hook 'after-make-frame-functions #'rvb/initialize-ui)
(add-hook 'window-setup-hook #'rvb/initialize-ui)

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
  ;; In org buffers, hand runs of letters to the font shaper and let the
  ;; font's own ligature table decide what to form (fi, ffi, etc.).  This
  ;; picks up whatever ligatures the active font provides, while leaving
  ;; org markup characters (/ * _ = ~ +) untouched.
  (ligature-set-ligatures
   'org-mode
   (mapcar (lambda (char) (list (char-to-string char) "[A-Za-z]+"))
	   (append (number-sequence ?A ?Z) (number-sequence ?a ?z))))
  (global-ligature-mode t))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

;; (use-package nerd-icons-corfu
;;   :ensure t
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;;   ;; Optionally:
;;   (setq nerd-icons-corfu-mapping
;; 	'((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
;;           (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
;;           ;; You can alternatively specify a function to perform the mapping,
;;           ;; use this when knowing the exact completion candidate is important.
;;           ;; Don't pass `:face' if the function already returns string with the
;;           ;; face property, though.
;;           (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
;;           ;; ...
;;           (t :style "cod" :icon "code" :face font-lock-warning-face)))
;;   ;; If you add an entry for t, the library uses that as fallback.
;;   ;; The default fallback (when it's not specified) is the ? symbol.

;;   ;; The Custom interface is also supported for tuning the variable above.
;;   )


;; Clearer separation between buffers
;; (window-divider-mode)

;; ;;; Magit todos
;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config (magit-todos-mode 1))

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
