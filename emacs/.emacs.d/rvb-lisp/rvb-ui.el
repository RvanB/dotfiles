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

;; (use-package vertico-posframe
;;   :ensure t
;;   :config
;;   (vertico-posframe-mode 1))

;; (use-package transient-posframe
;;   :ensure t
;;   :config
;;   (transient-posframe-mode))

;; (use-package hydra-posframe
;;   :ensure nil
;;   :vc (:url "https://github.com/Ladicle/hydra-posframe"
;;             :rev :newest)
;;   :hook (after-init . hydra-posframe-mode))

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable the standard right-click context menus globally.
(context-menu-mode 1)

;; The tab bar is set up in rvb-tabs.el.

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

(defun rvb/theme-ns-appearance (&optional theme)
  "Return the native macOS appearance requested by THEME.

Themes declare their preference with the `rvb/ns-appearance' symbol
property.  A missing preference leaves the appearance up to macOS."
  (get (or theme rvb-theme) 'rvb/ns-appearance))

(defun rvb/load-theme (theme &optional no-save)
  "Load THEME and make it the current RVB theme.

When NO-SAVE is non-nil, do not persist THEME."
  (mapc #'disable-theme (copy-sequence custom-enabled-themes))
  (load-theme theme t)
  (setq rvb-theme theme)
  (unless no-save
    (customize-save-variable 'rvb-theme theme))
  (rvb/set-frame-parameter-defaults
   'ns-appearance (rvb/theme-ns-appearance theme))
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
      (set-frame-parameter target-frame 'ns-appearance
                           (rvb/theme-ns-appearance)))))

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
(set-face-attribute 'default nil :family "SF Mono" :height 140)
(set-face-attribute 'fixed-pitch nil :family "SF Mono")
(set-face-attribute 'variable-pitch nil :family "ITC Galliard")

;; The two are the same point size but not the same apparent size: a
;; monospaced face is drawn large for its em, so prose set in Galliard
;; reads short beside code set in Berkeley Mono.
;;
;; The correction has to scale the *font*, not the face.  `mixed-pitch'
;; copies only :family and :weight from `variable-pitch' onto `default'
;; unless `mixed-pitch-set-height' is on, so a :height on that face is
;; ignored in exactly the buffers where this matters.
;; `face-font-rescale-alist' is applied when the font is chosen, so it
;; survives that and covers every other use of the family too.

(defun rvb/variable-pitch-family ()
  "Return the family `variable-pitch' asks for, or nil for the default."
  (let ((family (face-attribute 'variable-pitch :family nil t)))
    (and (stringp family) family)))

(defun rvb/apply-variable-pitch-rescale ()
  "Make `face-font-rescale-alist' agree with `rvb/variable-pitch-rescale'."
  (when-let* ((family (rvb/variable-pitch-family))
              (factor (and (boundp 'rvb/variable-pitch-rescale)
                           rvb/variable-pitch-rescale)))
    (setq face-font-rescale-alist
          (assoc-delete-all family face-font-rescale-alist))
    (unless (= factor 1.0)
      (push (cons family factor) face-font-rescale-alist))
    ;; Fonts already chosen are cached with their old size.
    (clear-face-cache t)))

(defcustom rvb/variable-pitch-rescale 1.0
  "Factor by which the `variable-pitch' family is scaled.

1.0 leaves it alone.  Setting it rewrites the family's entry in
`face-font-rescale-alist' and clears the face cache, so a new value
takes effect where you set it."
  :type 'number
  :group 'appearance
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'rvb/apply-variable-pitch-rescale)
           (rvb/apply-variable-pitch-rescale))))

(defun rvb/select-current-theme ()
  "Select a theme with Consult and persist it in `rvb-theme'."
  (interactive)
  (call-interactively #'consult-theme)
  (when-let ((theme (car custom-enabled-themes)))
    (setq rvb-theme theme)
    (customize-save-variable 'rvb-theme theme)
    (rvb/set-frame-parameter-defaults
     'ns-appearance (rvb/theme-ns-appearance theme))
    (dolist (frame (frame-list))
      (rvb/apply-frame-appearance frame))
    (when (and (bound-and-true-p rvb/ui-page-chrome-mode)
               (fboundp 'rvb/ui-page-chrome-refresh))
      (rvb/ui-page-chrome-refresh))))

(defvar rvb/ui-page-chrome--saved-header-lines nil)
(defvar rvb/ui-page-chrome--saved-line-number-faces nil
  "Alist mapping (FRAME . FACE) to the line-number background to restore.")

(defcustom rvb/ui-page-chrome-vertical-padding 4
  "Vertical padding, in pixels, above and below page chrome header text.
The hairline under the header stays at its bottom edge, below the
padding; see `rvb/ui-page-chrome--metrics'."
  :type 'integer
  :group 'appearance)

(defvar rvb/ui-page-chrome--metrics-cache (make-hash-table :test #'equal)
  "Header line metrics worked out already, by font and padding.")

(defun rvb/ui-page-chrome--metrics (frame)
  "Return the header line's metrics on FRAME, as a plist, or nil.
See `rvb/ui--line-metrics', which this is for the header."
  (rvb/ui--line-metrics frame 'default rvb/ui-page-chrome-vertical-padding))

(defun rvb/ui--line-metrics (frame face pad)
  "Return metrics for a line of FACE's text padded by PAD pixels, on FRAME.
PAD is pixels above and below alike, or (TOP . BOTTOM).

  :height        the line's height in pixels, padding included
  :ascent        how much of that is above the baseline, as a
                 percentage -- what an image or a strut as tall as the
                 line needs as its `:ascent' to sit exactly in it
  :strut         an invisible, zero-width glyph that tall, which is
                 what makes the line taller; nil with no padding
  :rule-position the underline `:position' putting the hairline on the
                 last row of the padded line; nil with no padding

A face `:box' would pad the line too, but the hairline under the band
is an underline, and an underline is drawn under the text -- above a
box's bottom edge, so it would float in the middle of the padding."
  (let* ((top (max 0 (or (if (consp pad) (car pad) pad) 0)))
         (bottom (max 0 (or (if (consp pad) (cdr pad) pad) 0)))
         (pad (cons top bottom))
         (font (face-font face frame)))
    (when font
      (let ((key (list font pad)))
        (or (gethash key rvb/ui-page-chrome--metrics-cache)
            (puthash
             key
             (when-let* ((info (font-info font frame)))
               (let* ((ascent (aref info 8))
                      (descent (aref info 9))
                      (height (+ ascent descent top bottom))
                      (percent (round (* 100.0 (+ ascent top)) height))
                      (padded (> (+ top bottom) 0)))
                 (list :height height
                       :top top
                       :bottom bottom
                       :ascent percent
                       :strut (and padded
                                   (propertize
                                    " " 'display
                                    `(space :width (0) :height (,height)
                                            :ascent ,percent)))
                       ;; Measured, not taken from the manual: an integer
                       ;; position counts up from the bottom of the line
                       ;; here, so 0 is its last row.  `t' would put the
                       ;; hairline under the text, above the padding.
                       :rule-position (and padded 0))))
             rvb/ui-page-chrome--metrics-cache))))))

(defun rvb/ui-page-chrome--rule-attributes (frame)
  "Return face attributes putting the header's hairline at its padded bottom.
Nil when there is no padding, or the header face draws no hairline."
  (when-let* ((position (plist-get (rvb/ui-page-chrome--metrics frame)
                                    :rule-position))
              (underline (face-attribute 'rvb/ui-page-chrome-header
                                         :underline frame t))
              ((consp underline)))
    (list :underline (plist-put (copy-sequence underline) :position position))))

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
  '((t :inherit default :underline (:position t)))
  "Face for the RVB page-chrome top header band.
By default the page's own background, ruled off from the text below;
see `rvb/ui-page-chrome--derive-faces'.")

(defface rvb/ui-page-chrome-command
  '((t :inherit header-line))
  "Face supplying the page-chrome command-state colours.")

(defface rvb/ui-page-chrome-breadcrumb-highlight
  '((t :inverse-video t))
  "Face used when the pointer is over a page-chrome breadcrumb.")

(defface rvb/ui-page-chrome-scroll-trough
  '((t :inherit rvb/ui-page-chrome-header))
  "Face for the length of the buffer, in the header-line scrollbar.")

(defface rvb/ui-page-chrome-scroll-handle
  '((t :inherit region))
  "Face for the part of the buffer on screen, in the header-line scrollbar.")

;;;; Page-chrome faces for any theme
;;
;; Only rvb3 styles these faces itself.  Any other theme would leave the
;; band in its `header-line' colours -- a grey or black bar -- when what
;; is wanted everywhere is the page: the buffer's background, set off by
;; a hairline beneath.  None of that can be written as a fixed face spec,
;; because the colours are the theme's, so the default specs are worked
;; out from `default' whenever a theme is enabled.  A theme that does
;; style a face still wins: this only rewrites the defaults.
;;
;; The scrollbar faces supply colours only -- the track's dots are its
;; foreground on its background, the handle's grip lines and outline
;; likewise; `rvb/ui-page-chrome--draw-bar' draws them.

(require 'color)

(defun rvb/ui-page-chrome--blend (a b amount)
  "Return colour A mixed with B, AMOUNT of the way from A to B."
  (let ((ca (color-name-to-rgb a))
        (cb (color-name-to-rgb b)))
    (if (and ca cb)
        (apply #'color-rgb-to-hex
               (append (cl-mapcar (lambda (x y) (+ x (* amount (- y x)))) ca cb)
                       '(2)))
      a)))

(defun rvb/ui-page-chrome--theme-style (key)
  "Return what the enabled themes ask page chrome to do about KEY.

A theme asks by putting a plist on its own symbol, as rvb3 does:

  (put \='rvb3 \='rvb/ui-page-chrome
       \='(:bar-slider t :tab-slants t :line-number-background t))

  :bar-slider             draw the header scrollbar as a slider -- a
                          text-coloured line with a striped handle on
                          it -- rather than a plain bar
  :tab-slants             give tabs slanted sides, and pad them by
                          `rvb/tab-bar-vertical-padding'
  :line-number-background keep the theme's line-number background
                          rather than drawing the margin in the chrome
                          colour, `rvb/ui--secondary-background'

The first enabled theme that mentions KEY decides; no theme saying
anything means nil, the plain behaviour."
  (cl-loop for theme in custom-enabled-themes
           for style = (get theme 'rvb/ui-page-chrome)
           when (plist-member style key) return (plist-get style key)))

(defun rvb/ui--secondary-background ()
  "Return the current theme's secondary background: the chrome colour.

What the header line, the line-number margin and the current tab are
drawn in -- a quiet step off the page, so that the text is set apart
from what is around it.  Themes have one, but no common name
for it; what they reliably put it in is the background of an inactive
mode line (modus: `bg-inactive', doric and standard: their dim grey, ef:
a tinted version of the page), so that is taken -- when it is a quiet
step, not a strong colour.  Failing that, the page nudged towards the
text."
  (let* ((fg (face-foreground 'default nil t))
         (bg (face-background 'default nil t))
         (candidate (face-background 'mode-line-inactive nil t))
         (bg-rgb (and bg (color-name-to-rgb bg)))
         (candidate-rgb (and candidate (color-name-to-rgb candidate))))
    (if (and bg-rgb candidate-rgb
             (not (equal bg-rgb candidate-rgb))
             ;; A step, not a leap: no channel more than a fifth away.
             (< (apply #'max (cl-mapcar (lambda (a b) (abs (- a b)))
                                        bg-rgb candidate-rgb))
                0.2))
        candidate
      (rvb/ui-page-chrome--blend bg fg 0.07))))

(defun rvb/ui-page-chrome--derive-faces (&rest _)
  "Set the page-chrome faces' default specs from the current theme's colours."
  (let* ((fg (face-foreground 'default nil t))
         (bg (face-background 'default nil t))
         (rule (rvb/ui-page-chrome--blend bg fg 0.35))
         (chrome (rvb/ui--secondary-background))
         (handle (rvb/ui-page-chrome--blend bg fg 0.55))
         (underline `(:color ,rule :position t))
         (specs
          `((rvb/ui-page-chrome-header
             ((t :foreground ,fg :background ,chrome :stipple nil
                 :underline ,underline)))
            ;; Command state is the unmistakable one: the theme's own
            ;; header-line colours, which are the band's old look.
            (rvb/ui-page-chrome-command
             ((t :inherit header-line :stipple nil)))
            (rvb/ui-page-chrome-breadcrumb-highlight
             ((t :foreground ,bg :background ,fg :stipple nil)))
            ;; A plain bar: the track is the band, the handle a solid
            ;; mid-tone.  A theme asking for `:bar-slider' styles these
            ;; itself; see `rvb/ui-page-chrome--theme-style'.
            (rvb/ui-page-chrome-scroll-trough
             ((t :foreground ,fg :background ,chrome :underline ,underline)))
            (rvb/ui-page-chrome-scroll-handle
             ((t :foreground ,fg :background ,handle :underline ,underline))))))
    (when (and fg bg)
      (pcase-dolist (`(,face ,spec) specs)
        (face-spec-set face spec 'face-defface-spec))
      ;; Whatever the theme says, under every theme: the fringes are the
      ;; page -- side by side, two windows' fringes make a strip between
      ;; them, and in any other colour it reads as a bar -- and the
      ;; divider between a window and what is below it, the minibuffer
      ;; included, is the hairline.
      (set-face-attribute 'fringe nil :background bg)
      (dolist (face '(window-divider window-divider-first-pixel
                                     window-divider-last-pixel))
        (set-face-attribute face nil :foreground rule))
      ;; The line-number backgrounds page chrome saved belong to the
      ;; theme just replaced; the next refresh saves this one's.
      (setq rvb/ui-page-chrome--saved-line-number-faces nil))))

(add-hook 'enable-theme-functions #'rvb/ui-page-chrome--derive-faces)
;; The startup theme was enabled further up, before the hook was here.
(rvb/ui-page-chrome--derive-faces)

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
              'mouse-face 'rvb/ui-page-chrome-breadcrumb-highlight
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
                  ;; The root is already a slash; a second one here
                  ;; would read "//private/...".
                  (unless (and (= index 0) (equal root-label "/"))
                    (push "/" crumbs))
                  (if (and file-p last-p)
                      (push part crumbs)
                    (setq current
                          (file-name-as-directory
                           (expand-file-name part current)))
                    (push (rvb/ui-page-chrome--breadcrumb-part part current)
                          crumbs))))
    (apply #'concat (nreverse crumbs))))

(defun rvb/ui-page-chrome--command-state-p (buffer)
  "Return non-nil when BUFFER is in God Mode's command state.

God Mode and nothing else.  Read-only used to count as well, on the
grounds that a buffer you cannot type into is one you can only give
commands to -- but that was written when God Mode was global and
exempted Magit, Dired and the rest, leaving them with no state to show
at all.  Now that command state is somewhere you go, per buffer, they
have one like everything else, and the old rule only stopped the band
from following you out of it."
  (with-current-buffer buffer
    (bound-and-true-p god-local-mode)))

(defvar rvb/ui-page-chrome--band-map
  (let ((map (make-sparse-keymap)))
    ;; Only the press.  The release still arrives as `mouse-1', which is
    ;; globally `mouse-select-window' -- clicking the band to choose its
    ;; window goes on working.
    (define-key map [header-line down-mouse-1] #'ignore)
    map)
  "Mouse map for the blank run of the page-chrome band.")

(defun rvb/ui-page-chrome--claim-drag (string)
  "Stop a drag on STRING from being a drag of the frame.

`mouse-drag-header-line' owns `down-mouse-1' on any header line, and on
a frame with `drag-with-header-line' set that drags the whole frame
about.  Which is a fine thing for a header line that is only a label,
and a poor one for a band carrying a breadcrumb trail and a scrollbar:
the scrollbar is twelve columns wide, and missing it by one would pick
up the window instead.

Only where nothing else has claimed the mouse, so that the breadcrumbs
keep opening Dired and the scrollbar keeps scrolling."
  (let ((pos 0)
        (end (length string)))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'local-map string) end)))
        (unless (get-text-property pos 'local-map string)
          (put-text-property pos next 'local-map rvb/ui-page-chrome--band-map
                             string))
        (setq pos next)))))

(defun rvb/ui-page-chrome--band-faces (face command-p)
  "Return the faces a band drawn in FACE is made of.

The command face is kept ahead of FACE rather than replacing it, so
that FACE's non-colour attributes -- its stipple above all -- survive
the composition.  Anything else drawn as part of the band asks for the
faces here, so it cannot end up a different colour from the band it is
part of."
  (if command-p (list 'rvb/ui-page-chrome-command face) (list face)))

(defun rvb/ui-page-chrome--band (window content face &optional command-p width)
  "Render CONTENT across WINDOW using FACE.

When COMMAND-P is non-nil, change only the band's colors to indicate
command state; retain the normal page-chrome face and font metrics.

WIDTH is how many columns the band fills, defaulting to the whole
window.  The scrollbar is given its columns this way: it paints its own
background, and this band's face would otherwise be laid over the top
of it.

The band uses the frame's `default' font attributes so its fixed-width
font matches ordinary buffer text instead of the generic `fixed-pitch' face."
  (pcase-let* ((frame (window-frame window))
               ;; Command state has its own theme face; it is not an error and
               ;; should not change when diagnostic styling changes.
               (header-background
                (if command-p
                    (face-background 'rvb/ui-page-chrome-command frame t)
                  (face-background face frame t)))
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
               (_ header-background)
               (rule (rvb/ui-page-chrome--rule-attributes frame))
               (band-face
                (let ((faces (rvb/ui-page-chrome--band-faces face command-p)))
                  (append (and rule (list rule))
                          faces
                          (and font-attrs (list font-attrs)))))
               (width (or width (window-total-width window)))
               (content (truncate-string-to-width content width))
               (band (concat (plist-get (rvb/ui-page-chrome--metrics frame) :strut)
                             content
                             (make-string (max 0 (- width (string-width content)))
                                          ?\s))))
    (add-face-text-property 0 (length band) band-face nil band)
    (rvb/ui-page-chrome--claim-drag band)
    band))

(defun rvb/ui-page-chrome--header-content (window width)
  "Return WINDOW's file/status header, fitted into WIDTH columns."
  (with-current-buffer (window-buffer window)
    (let* ((file buffer-file-name)
           (path (cond
                  (file
                   ;; Whether the file is modified or read-only: there
                   ;; is no mode line to say so (see below).
                   (concat (rvb/ui-page-chrome--path-breadcrumb file t)
                           (cond (buffer-read-only " (read-only)")
                                 ((buffer-modified-p) " *")
                                 (t ""))))
                  (default-directory
                   (rvb/ui-page-chrome--path-breadcrumb default-directory))
                  (t
                   (buffer-name))))
           ;; Where point is -- "Top  L12", "42%  L310" -- beside the
           ;; scrollbar that shows the same thing, there being no mode
           ;; line (see below).  `mode-line-position' is what
           ;; the mode line used, so it follows the same settings -- a
           ;; column with `column-number-mode', a size with
           ;; `size-indication-mode' -- and keeps its mouse menus.
           (status (string-trim (format-mode-line mode-line-position
                                                  nil window)))
           ;; Reserve the right edge before truncating long paths so status
           ;; information can never be pushed out of the header.  Cut from
           ;; the left: the end of the path -- the file, and whether it
           ;; is modified -- is the part worth keeping.
           (room (max 1 (- width (string-width status) 4)))
           (path (if (<= (string-width path) room)
                     path
                   (concat "…" (truncate-string-to-width
                                path (string-width path)
                                (- (string-width path) (1- room))))))
           (gap (max 2 (- width (string-width path) (string-width status) 2))))
      (truncate-string-to-width
       (concat " " path (make-string gap ?\s) status " ") width))))

(defun rvb/ui-page-chrome-scroll-drag (start-event)
  "Scroll the buffer by dragging the header-line scrollbar.

`mlscroll-mouse' is this, and would have done: it clicks, then follows
the pointer.  What it will not do is follow it here.  Its loop moves
the buffer only while `posn-area' says `mode-line', and every event of
a drag along this band says `header-line' instead -- so the click lands
and the drag that follows it does nothing at all.

That test cannot be answered from outside, either: `posn-area' is a
`defsubst', so it is compiled into MLScroll rather than called, and
rebinding it reaches nothing.  What is left is to keep the loop here,
where the one line that has to differ can differ.  Everything else is
MLScroll's, down to the pixel arithmetic, and the scrolling itself is
still `mlscroll-scroll-to'."
  (interactive "e")
  (let* ((start-posn (event-start start-event))
         (start-win (posn-window start-posn))
         (lcr (mlscroll-find-index (posn-string start-posn)))
         ;; Where in the bar the click landed, and where that is on screen.
         (x (car (posn-object-x-y start-posn)))
         (xstart-abs (car (posn-x-y start-posn)))
         (xstart (mlscroll-scroll-to x lcr start-win))
         event end xnew)
    (unless (terminal-parameter nil 'xterm-mouse-mode)
      (pcase-let ((`(,_ ,scroll-width ,border)
                   (terminal-parameter nil 'mlscroll-size))
                  (mouse-fine-grained-tracking t))
        (track-mouse
          (setq track-mouse 'dragging)
          (while (and (setq event (read-event))
                      (mouse-movement-p event))
            (setq end (event-end event)
                  xnew (+ xstart (- (car (posn-x-y end)) xstart-abs)))
            ;; The line: either band counts, so the drag keeps up whether
            ;; the pointer is over this one or the mode line below.
            (when (and (memq (posn-area end) '(header-line mode-line))
                       (>= xnew 0)
                       (<= xnew (- scroll-width border)))
              (mlscroll-scroll-to xnew nil start-win))))))))

(defvar rvb/ui-page-chrome--scroll-keymap
  (let ((map (make-sparse-keymap)))
    ;; A click on the mode line arrives as a `mode-line' event and a
    ;; click on the header line as a `header-line' one, so MLScroll's own
    ;; map is one the bar can never be reached through up here.  The
    ;; wheel commands are its; the drag is the one above.
    ;;
    ;; The press: it jumps the buffer to where it landed, and then
    ;; follows the pointer, which is what dragging is here.
    (define-key map [header-line down-mouse-1] #'rvb/ui-page-chrome-scroll-drag)
    (define-key map [header-line wheel-up] #'mlscroll-wheel)
    (define-key map [header-line wheel-down] #'mlscroll-wheel)
    (define-key map [header-line wheel-left] #'ignore)
    (define-key map [header-line wheel-right] #'ignore)
    map)
  "Mouse map for the scrollbar in the header line.")

(defun rvb/ui-page-chrome--scroll-keymap ()
  "Return the scrollbar's mouse map."
  rvb/ui-page-chrome--scroll-keymap)

(defun rvb/ui-page-chrome--scroll-spacer (faces)
  "Return the run of band between the header text and the scrollbar.

Faced, and aligned to where the bar begins.  A plain space would be
drawn in the `header-line' face instead -- a black notch beside the
bar -- and padding by columns would leave the pixels the bar does not
fill at the window's right edge showing the same thing."
  (when-let* ((size (terminal-parameter nil 'mlscroll-size))
              (pixels (- (nth 1 size) (nth 2 size))))
    (let ((spacer (propertize " " 'face faces
                              'display `(space :align-to
                                               (- (+ right right-margin)
                                                  (,pixels))))))
      (rvb/ui-page-chrome--claim-drag spacer)
      spacer)))

;;;; Drawing the bar
;;
;; MLScroll's bar is three spaces of set pixel widths: the track before
;; the handle, the handle, the track after.  Under a theme asking for
;; `:bar-slider' each is given an image of its exact width instead, so
;; the bar reads as a slider: the track is the band itself with a
;; text-coloured line through its middle, and the handle is a box of
;; grip lines sitting on that line, as tall as the text.  Images rather
;; than faces because a line through the middle of a space, or a box
;; shorter than the line it is on, is not something a face can draw.

(require 'svg)

(defvar rvb/ui-page-chrome--bar-images (make-hash-table :test #'equal)
  "Bar images already made, by what they depict.
The header line is redrawn constantly, and the bar with it.")

(defun rvb/ui-page-chrome--bar-image (kind width metrics colors)
  "Return an image of the slider's KIND, WIDTH pixels wide.
KIND is `track' or `handle'.  METRICS is `rvb/ui--line-metrics' for
the header.  COLORS is a plist: :ground, the band behind the bar;
:line, the track's line; :handle and :handle-ground, the handle's ink
and fill; :rule, the hairline along the bottom of the band, or nil."
  (let ((key (list kind width metrics colors)))
    (or (gethash key rvb/ui-page-chrome--bar-images)
        (puthash
         key
         (let* ((height (plist-get metrics :height))
                (middle (/ height 2))
                (svg (svg-create width height)))
           (svg-rectangle svg 0 0 width height
                          :fill (plist-get colors :ground))
           (pcase kind
             ('track
              (svg-rectangle svg 0 middle width 1
                             :fill (plist-get colors :line)))
             ('handle
              ;; As tall as the text, centred on the line: the padding
              ;; above and below it stays the band.
              (let ((top (plist-get metrics :top))
                    (bottom (- height (plist-get metrics :bottom) 1))
                    (ink (plist-get colors :handle)))
                (svg-rectangle svg 0.5 (+ top 0.5) (- width 1) (- bottom top)
                               :fill (plist-get colors :handle-ground)
                               :stroke ink :stroke-width 1)
                (cl-loop for x from 3 below (- width 2) by 3
                         do (svg-rectangle svg x (1+ top) 1 (- bottom top 1)
                                           :fill ink)))))
           (when-let* ((rule (plist-get colors :rule)))
             (svg-rectangle svg 0 (1- height) width 1 :fill rule))
           (svg-image svg :ascent (plist-get metrics :ascent)))
         rvb/ui-page-chrome--bar-images))))

(defun rvb/ui-page-chrome--draw-bar (bar window)
  "Replace BAR's spaces with slider images of the same widths, for WINDOW."
  (let* ((frame (window-frame window))
         (metrics (rvb/ui-page-chrome--metrics frame))
         (rule (face-attribute 'rvb/ui-page-chrome-header :underline frame t))
         (colors
          (list :ground (face-background 'rvb/ui-page-chrome-header frame t)
                :line (face-foreground 'rvb/ui-page-chrome-scroll-trough frame t)
                :handle (face-foreground 'rvb/ui-page-chrome-scroll-handle frame t)
                :handle-ground (face-background 'rvb/ui-page-chrome-scroll-handle
                                                frame t)
                :rule (and (consp rule) (plist-get rule :color)))))
    (when metrics
      (dotimes (i (length bar))
        (pcase (get-text-property i 'display bar)
          (`(space :width (,(and (pred numberp) width)))
           (when (> width 0)
             (put-text-property
              i (1+ i) 'display
              (rvb/ui-page-chrome--bar-image
               (if (eq (get-text-property i 'face bar)
                       mlscroll-cur-face-properties)
                   'handle
                 'track)
               (max 1 (round width)) metrics colors)
              bar)
             (put-text-property i (1+ i) 'face 'rvb/ui-page-chrome-header
                                bar))))))
    bar))

(defun rvb/ui-page-chrome--scrollbar (window)
  "Return the scrollbar for WINDOW's header line, or nil.

Nil whenever MLScroll is not running, so the header is exactly what it
was before without it."
  (when (and (bound-and-true-p mlscroll-mode)
             (fboundp 'mlscroll-mode-line)
             (terminal-parameter nil 'mlscroll-size))
    (let ((bar (with-selected-window window (mlscroll-mode-line))))
      (when-let* (((stringp bar))
                  (keymap (rvb/ui-page-chrome--scroll-keymap)))
        (setq bar (copy-sequence bar))
        (put-text-property 0 (length bar) 'local-map keymap bar)
        (when (and (display-graphic-p (window-frame window))
                   (rvb/ui-page-chrome--theme-style :bar-slider))
          (rvb/ui-page-chrome--draw-bar bar window)))
      bar)))

(defun rvb/ui-page-chrome--header-line-format (window)
  "Return WINDOW's top file header."
  (let* ((bar (rvb/ui-page-chrome--scrollbar window))
         ;; One column of air, then exactly the bar's own width -- it is
         ;; `mlscroll-width-chars' characters of the frame's font.
         (reserved (if bar (1+ mlscroll-width-chars) 0))
         (width (max 0 (- (window-total-width window) reserved)))
         (command-p (rvb/ui-page-chrome--command-state-p
                     (window-buffer window)))
         (band (rvb/ui-page-chrome--band
                window (rvb/ui-page-chrome--header-content window width)
                'rvb/ui-page-chrome-header command-p width)))
    (if bar
        (let ((rule (rvb/ui-page-chrome--rule-attributes (window-frame window))))
          (when rule
            (add-face-text-property 0 (length bar) rule nil bar))
          (list band
                (rvb/ui-page-chrome--scroll-spacer
                 (append (and rule (list rule))
                         (rvb/ui-page-chrome--band-faces 'rvb/ui-page-chrome-header
                                                         command-p)))
                bar))
      band)))

(defvar-local rvb/ui-page-chrome--header-remap nil
  "Cookie for this buffer's remapping of `header-line', while page chrome is on.")

(defun rvb/ui-page-chrome--apply-window (window)
  "Apply page chrome to WINDOW."
  (when (rvb/ui-page-chrome--window-p window)
    (let ((buffer (window-buffer window)))
      (rvb/ui-page-chrome--save-header-line buffer)
      (with-current-buffer buffer
        ;; The band is drawn in its own faces, but whatever pixels it
        ;; does not reach -- a few at the window's right edge, past the
        ;; scrollbar -- are drawn in `header-line', which is not the page.
        (unless rvb/ui-page-chrome--header-remap
          (setq rvb/ui-page-chrome--header-remap
                (face-remap-add-relative 'header-line
                                         'rvb/ui-page-chrome-header)))
        (setq-local header-line-format
                    '((:eval (rvb/ui-page-chrome--header-line-format
                               (selected-window)))))))))

(defun rvb/ui-page-chrome--apply-line-number-faces (frame)
  "Draw the line-number margin in FRAME in the chrome colour.

The header's background, which is `rvb/ui--secondary-background' unless
the theme says otherwise -- so the margin and the header are one
colour and only the text is the page.  The theme's own
background is saved first, once per theme, so turning page chrome off
puts it back."
  (let ((chrome (face-background 'rvb/ui-page-chrome-header frame t)))
    (dolist (face '(line-number line-number-current-line))
      (let ((key (cons frame face)))
        (unless (assoc key rvb/ui-page-chrome--saved-line-number-faces)
          (push (cons key (face-attribute face :background frame))
                rvb/ui-page-chrome--saved-line-number-faces)))
      (set-face-attribute face frame :background chrome))))

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
        ;; A theme asking to keep its own margin (rvb3) keeps it.
        (unless (rvb/ui-page-chrome--theme-style :line-number-background)
          (rvb/ui-page-chrome--apply-line-number-faces frame))
        (walk-windows #'rvb/ui-page-chrome--apply-window 'no-minibuf frame)))))

(defun rvb/ui-page-chrome--restore ()
  "Restore header lines changed by RVB page chrome."
  (dolist (entry rvb/ui-page-chrome--saved-header-lines)
    (pcase-let ((`(,buffer ,was-local ,header-line) entry))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when rvb/ui-page-chrome--header-remap
            (face-remap-remove-relative rvb/ui-page-chrome--header-remap)
            (setq rvb/ui-page-chrome--header-remap nil))
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

;; No mode line.  What it said that is worth saying is on the header
;; line -- the file, whether it is modified, where point is -- and the
;; modes it listed put menus of their own in the menu bar.  Buffers
;; whose major mode sets a mode line of its own still get one.
(setq-default mode-line-format nil)


;;; The scrollbar, in the header line rather than the mode line
;;
;; MLScroll describes its bar in *colours*: one for the length of the
;; buffer and one for the part of it on screen, worked out from the mode
;; line's own background.  Under this theme both of those come out the
;; colour of the page, which is why the bar arrives invisible -- and no
;; colour could be right anyway, since the band it sits in is black
;; under one hand and screened under the other.
;;
;; Faces can say what colours cannot: a stipple for the length of the
;; buffer, and a foreground that stays put whatever the band is doing.
;; The two variables holding the bar's appearance take anything a `face'
;; property takes, so a face name goes straight in.

(defun rvb/ui-page-chrome--scroll-faces (&rest _)
  "Point MLScroll's bar at the page-chrome faces."
  (setq mlscroll-flank-face-properties 'rvb/ui-page-chrome-scroll-trough
        mlscroll-cur-face-properties 'rvb/ui-page-chrome-scroll-handle))

(use-package mlscroll
  :ensure t
  :init
  ;; Both of these keep MLScroll's hands off the mode line: the bar is
  ;; placed by `rvb/ui-page-chrome--header-line-format', and the mode
  ;; line's percentage is left where it is.
  (setq mlscroll-right-align nil
        mlscroll-alter-percent-position nil
        ;; The border is drawn in the mode line's background, which is
        ;; not where this bar lives.
        mlscroll-border 0
        ;; Wide enough that the handle's grip lines show even when the
        ;; window is a sliver of a long buffer.
        mlscroll-minimum-current-width 12)
  :config
  ;; `mlscroll-layout' recomputes the bar's appearance from those colours
  ;; -- at startup, on a new frame, and on every theme load -- so the
  ;; faces have to be put back each time it does.
  (advice-add 'mlscroll-layout :after #'rvb/ui-page-chrome--scroll-faces)
  (mlscroll-mode 1))

(transient-define-prefix rvb/ui-menu ()
  "Open the UI settings menu."
  ["Actions"
   ("t" "Change current theme" rvb/select-current-theme)
   ("p" "Toggle page chrome" rvb/ui-page-chrome-mode)]
  ["Custom settings"
   ("a" "Appearance settings" (lambda () (interactive) (customize-group 'appearance)))])

;; A hairline along the bottom of each window: between windows stacked
;; one above the other, and -- with no mode line -- between the bottom
;; window and the minibuffer, which is otherwise hard to tell from the
;; code above it.  One pixel, in the hairline colour; see
;; `rvb/ui-page-chrome--derive-faces'.  The frame is created with it
;; already (early-init.el), so turning it on changes nothing there.
(setq window-divider-default-places 'bottom-only
      window-divider-default-bottom-width 1)
(window-divider-mode 1)

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

;; Use a chunky solid block instead of Diff-hl's thin outlined bitmaps.
(defun rvb/diff-hl-fringe-bitmap (_type _position)
  "Return the screened fringe bitmap used for every Diff-hl change type."
  'rvb/diff-hl-stipple-bitmap)

(defun rvb/diff-hl-define-fringe-bitmap ()
  "Define a six-pixel checkerboard Diff-hl bitmap at the current line height."
  (when (display-graphic-p)
    (let ((height (frame-char-height))
          (width 6)
          (rows (make-vector (frame-char-height) 0)))
      (dotimes (row height)
        (aset rows row (if (zerop (% row 2)) #b101010 #b010101)))
      (define-fringe-bitmap 'rvb/diff-hl-stipple-bitmap
        rows
        height width 'center))))

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
