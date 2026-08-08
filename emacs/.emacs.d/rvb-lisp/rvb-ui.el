(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'cl-lib)
(require 'subr-x)
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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable the standard right-click context menus globally.
(context-menu-mode 1)

;; Highlight the delimiter matching the one at point.
(setq show-paren-delay 0)
(show-paren-mode 1)

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

;; Current theme
(defcustom rvb-theme 'rvb2
  "Theme to load at startup."
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

(defun rvb/load-theme (theme &optional no-save)
  "Load THEME and make it the current RVB theme.

When NO-SAVE is non-nil, do not persist THEME."
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (load-theme theme t)
  (setq rvb-theme theme)
  (unless no-save
    (customize-save-variable 'rvb-theme theme))
  ;; Leave the native title bar's appearance to macOS.
  (rvb/set-frame-parameter-defaults 'ns-appearance nil)
  (dolist (frame (frame-list))
    (rvb/apply-frame-appearance frame))
  ;; Loading a theme re-sets `line-number' and other faces, so re-assert the
  ;; page chrome styling on top of the freshly loaded theme.
  (when (and (bound-and-true-p rvb/ui-page-chrome-mode)
             (fboundp 'rvb/ui-page-chrome-refresh))
    (rvb/ui-page-chrome-refresh)))

(defun rvb/ensure-theme-loaded ()
  "Load `rvb-theme' when no theme is currently enabled."
  (unless custom-enabled-themes
    (rvb/load-theme rvb-theme t)))

(defun rvb/apply-frame-appearance (&optional frame)
  "Apply frame-specific appearance settings to FRAME."
  (let ((target-frame (or frame (selected-frame))))
    (when (display-graphic-p target-frame)
      (set-frame-parameter target-frame 'ns-transparent-titlebar nil)
      (set-frame-parameter target-frame 'ns-appearance nil))))

;; `rvb-settings' loads the persisted Custom values before this module, so the
;; selected theme is ready to apply immediately at startup.
(rvb/ensure-theme-loaded)

(require 'rvb-movement)

;;; Disable menu bar
;; (menu-bar-mode -1)
;;; Disable the scroll bar
(scroll-bar-mode -1)
;;; Disable tool bar
(tool-bar-mode -1)

(defvar markdown-hide-markup)

(defun rvb/markdown-hide-markup ()
  "Hide Markdown formatting markup in the current buffer."
  (setq-local markdown-hide-markup t)
  (add-to-invisibility-spec 'markdown-markup))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . rvb/markdown-hide-markup)
         (gfm-mode . rvb/markdown-hide-markup)))

;; Use one fixed-pitch font everywhere.  Setting `variable-pitch' explicitly
;; prevents packages which inherit it from reintroducing proportional prose.
(set-face-attribute 'default nil :family "CommitMono" :height 180)
(set-face-attribute 'fixed-pitch nil :family "CommitMono" :height 1.0)
(set-face-attribute 'variable-pitch nil :family "CommitMono" :height 1.0)

(defun rvb/select-current-theme ()
  "Select a theme with Consult and persist it in `rvb-theme'."
  (interactive)
  (call-interactively #'consult-theme)
  (when-let ((theme (car custom-enabled-themes)))
    (setq rvb-theme theme)
    (customize-save-variable 'rvb-theme theme)
    (rvb/set-frame-parameter-defaults 'ns-appearance nil)
    (dolist (frame (frame-list))
      (rvb/apply-frame-appearance frame))
    (when (and (bound-and-true-p rvb/ui-page-chrome-mode)
               (fboundp 'rvb/ui-page-chrome-refresh))
      (rvb/ui-page-chrome-refresh))))

(defvar rvb/ui-page-chrome--saved-header-lines nil)
(defvar rvb/ui-page-chrome--saved-line-number-faces nil
  "Alist mapping (FRAME . FACE) to the line-number background to restore.")

(defcustom rvb/ui-page-chrome-vertical-padding 6
  "Vertical padding, in pixels, around page chrome header text."
  :type 'integer
  :group 'appearance)

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

(defface rvb/ui-page-chrome-header
  '((t :inherit header-line))
  "Face for the RVB page-chrome top header band.")

(defvar rvb/ui-page-chrome-breadcrumb-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-1]
                #'rvb/ui-page-chrome-open-breadcrumb)
    (define-key map [header-line mouse-1]
                #'rvb/ui-page-chrome-open-breadcrumb)
    (define-key map [down-mouse-1]
                #'rvb/ui-page-chrome-open-breadcrumb)
    (define-key map [mouse-1]
                #'rvb/ui-page-chrome-open-breadcrumb)
    map)
  "Keymap for clickable page chrome path breadcrumbs.")

(defun rvb/ui-page-chrome--event-directory (event)
  "Return the breadcrumb directory clicked in EVENT."
  (when-let* ((position (event-start event))
              (string-position (posn-string position)))
    (get-text-property (cdr string-position)
                       'rvb/ui-page-chrome-directory
                       (car string-position))))

(defun rvb/ui-page-chrome-open-breadcrumb (event)
  "Open the Dired buffer for the breadcrumb clicked in EVENT."
  (interactive "e")
  (when-let ((directory (rvb/ui-page-chrome--event-directory event)))
    (dired directory)))

(defun rvb/ui-page-chrome--breadcrumb-part (label directory)
  "Return clickable breadcrumb LABEL opening DIRECTORY in Dired."
  (propertize label
              'local-map rvb/ui-page-chrome-breadcrumb-map
              'mouse-face 'highlight
              'help-echo (format "Open %s in Dired" directory)
              'follow-link t
              'rvb/ui-page-chrome-directory directory))

(defun rvb/ui-page-chrome--path-breadcrumb (path &optional file-p)
  "Return a clickable breadcrumb for PATH.
When FILE-P is non-nil, the final path element is rendered as plain text."
  (let* ((full-path (expand-file-name path))
         (home (file-name-as-directory (expand-file-name "~")))
         (under-home (string-prefix-p home full-path))
         (root-label (if under-home "~" "/"))
         (root-dir (if under-home home "/"))
         (relative (if under-home
                       (file-relative-name full-path home)
                     (string-remove-prefix "/" full-path)))
         (parts (split-string relative "/" t))
         (current root-dir)
         (last-index (1- (length parts)))
         (crumbs (list (rvb/ui-page-chrome--breadcrumb-part
                        root-label root-dir))))
    (cl-loop for part in parts
             for index from 0
             do (let ((last-p (= index last-index)))
                  (push "/" crumbs)
                  (if (and file-p last-p)
                      (push part crumbs)
                    (setq current
                          (file-name-as-directory
                           (expand-file-name part current)))
                    (push (rvb/ui-page-chrome--breadcrumb-part part current)
                          crumbs))))
    (apply #'concat (nreverse crumbs))))

(defun rvb/ui-page-chrome--band (window content face &optional god-p)
  "Render CONTENT across WINDOW using FACE.

When GOD-P is non-nil, change only the band's colors to indicate command
state; retain the normal page-chrome face and font metrics.

The band uses the frame's `default' font attributes so its fixed-width
font matches ordinary buffer text instead of the generic `fixed-pitch' face."
  (pcase-let* ((frame (window-frame window))
               ;; Turn the active theme's error foreground into a prominent
               ;; background without hard-coding a theme-specific red.
               (header-background
                (if god-p
                    (face-foreground 'error frame t)
                  (face-background face frame t)))
               (header-foreground
                (face-foreground face frame t))
               (default-family (face-attribute 'default :family frame))
               (default-height (face-attribute 'default :height frame))
               (default-weight (face-attribute 'default :weight frame))
               (font-attrs nil)
               (_ (unless (eq default-family 'unspecified)
                    (setq font-attrs
                          (append font-attrs (list :family default-family)))))
               (_ (when (integerp default-height)
                    (setq font-attrs
                          (append font-attrs (list :height default-height)))))
               (_ (unless (eq default-weight 'unspecified)
                    (setq font-attrs
                          (append font-attrs (list :weight default-weight)))))
               (_ (when (and (integerp rvb/ui-page-chrome-vertical-padding)
                             (> rvb/ui-page-chrome-vertical-padding 0)
                             (stringp header-background))
                    (setq font-attrs
                          (append font-attrs
                                  (list :box
                                        `(:line-width
                                          (0 . ,rvb/ui-page-chrome-vertical-padding)
                                          :color ,header-background))))))
               ;; `:box' can't draw a top-only rule, so use overline and
               ;; underline for top- and bottom-only rules with theme colors.
               (_ (when (stringp header-foreground)
                    (setq font-attrs
                          (append font-attrs
                                  (list :overline header-foreground
                                        :underline
                                        `(:color ,header-foreground
                                          :position t))))))
               (band-face
                (if god-p
                    ;; Put the background override ahead of the exact normal
                    ;; header faces, preserving every font and line metric.
                    (list (list :background header-background)
                          face font-attrs)
                  (if font-attrs
                      (list face font-attrs)
                    face)))
               (width (window-total-width window))
               (content (truncate-string-to-width content width))
               (band (concat content
                             (make-string (max 0 (- width (string-width content)))
                                          ?\s))))
    (add-face-text-property 0 (length band) band-face nil band)
    band))

(defun rvb/ui-page-chrome--header-content (window width)
  "Return WINDOW's file/status header, fitted into WIDTH columns."
  (with-current-buffer (window-buffer window)
    (let* ((file buffer-file-name)
           (path (cond
                  (file
                   (rvb/ui-page-chrome--path-breadcrumb file t))
                  (default-directory
                   (rvb/ui-page-chrome--path-breadcrumb default-directory))
                  (t
                   (buffer-name))))
           (status (format-mode-line
                    '("%e" mode-line-front-space
                      (:propertize
                       ("" mode-line-mule-info mode-line-client
                        mode-line-modified mode-line-remote
                        mode-line-window-dedicated)
                       display (min-width (6.0))))
                    nil window))
           ;; Reserve the right edge before truncating long paths so status
           ;; information can never be pushed out of the header.
           (path (truncate-string-to-width
                  path (max 0 (- width (string-width status) 4))))
           (gap (max 2 (- width (string-width path) (string-width status) 2))))
      (truncate-string-to-width
       (concat " " path (make-string gap ?\s) status " ") width))))

(defun rvb/ui-page-chrome--header-line-format (window)
  "Return WINDOW's top file header."
  (let ((width (window-total-width window))
        (god-p (with-current-buffer (window-buffer window)
                 (bound-and-true-p god-local-mode))))
    (rvb/ui-page-chrome--band
     window (rvb/ui-page-chrome--header-content window width)
     'rvb/ui-page-chrome-header
     god-p)))

(defun rvb/ui-page-chrome--apply-window (window)
  "Apply page chrome to WINDOW."
  (when (rvb/ui-page-chrome--window-p window)
    (let ((buffer (window-buffer window)))
      (rvb/ui-page-chrome--save-header-line buffer)
      (with-current-buffer buffer
        (setq-local header-line-format
                    '((:eval (rvb/ui-page-chrome--header-line-format
                               (selected-window)))))))))

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
  "Restore header lines changed by RVB page chrome."
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
  "Show a margin-limited file/status header above the buffer."
  :global t
  :lighter nil
  (if rvb/ui-page-chrome-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  #'rvb/ui-page-chrome--window-change)
        (add-hook 'after-make-frame-functions
                  #'rvb/ui-page-chrome--window-change)
        (rvb/ui-page-chrome-refresh))
    (remove-hook 'window-configuration-change-hook
                 #'rvb/ui-page-chrome--window-change)
    (remove-hook 'after-make-frame-functions
                 #'rvb/ui-page-chrome--window-change)
    (rvb/ui-page-chrome--restore)))

;; Page chrome is the primary editor-state indicator as well as the file
;; header, so keep it enabled unless explicitly toggled off by the user.
(rvb/ui-page-chrome-mode 1)

(transient-define-prefix rvb/ui-menu ()
  "Open the UI settings menu."
  ["Actions"
   ("t" "Change current theme" rvb/select-current-theme)
   ("p" "Toggle page chrome" rvb/ui-page-chrome-mode)]
  ["Custom settings"
   ("a" "Appearance settings" (lambda () (interactive) (customize-group 'appearance)))])

;; Clearer separation between buffers
;; (window-divider-mode)

;; ;;; Magit todos
;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config (magit-todos-mode 1))

;;; Split along the window's longer physical dimension.
(defun rvb/split-window-longest-dimension (&optional window)
  "Split WINDOW along its longer pixel dimension.

Wide windows split side-by-side; tall windows split above-and-below.  If the
preferred direction cannot satisfy Emacs's minimum window sizes, try the other
direction and return nil when neither split is possible."
  (let* ((window (or window (selected-window)))
         (wide-p (> (window-pixel-width window)
                    (window-pixel-height window)))
         (preferred-side (if wide-p 'right 'below))
         (fallback-side (if wide-p 'below 'right)))
    (or (condition-case nil
            (split-window window nil preferred-side)
          (error nil))
        (condition-case nil
            (split-window window nil fallback-side)
          (error nil)))))

(setq split-window-preferred-function #'rvb/split-window-longest-dimension)

;; Use a solid bar instead of Diff-hl's outlined hunk-border bitmaps.
(defun rvb/diff-hl-fringe-bitmap (_type _position)
  "Return the solid fringe bitmap used for every Diff-hl change type."
  'rvb/diff-hl-solid-bitmap)

(defun rvb/diff-hl-define-fringe-bitmap ()
  "Define a narrow, filled Diff-hl bitmap matching the current line height."
  (when (display-graphic-p)
    (let ((height (frame-char-height)))
      (define-fringe-bitmap 'rvb/diff-hl-solid-bitmap
        (make-vector height #b111)
        height 3 'center))))

;; Diff-hl with mouse support
(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-draw-borders nil
        diff-hl-fringe-bmp-function #'rvb/diff-hl-fringe-bitmap)
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (rvb/diff-hl-define-fringe-bitmap)
  (clrhash diff-hl-spec-cache)
  (global-diff-hl-show-hunk-mouse-mode))

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
