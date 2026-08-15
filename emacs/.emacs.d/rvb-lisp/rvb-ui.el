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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable the standard right-click context menus globally.
(context-menu-mode 1)

;; Enable tab bar
(tab-bar-mode)

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
    (rvb/set-frame-parameter-defaults 'ns-appearance nil)
    (dolist (frame (frame-list))
      (rvb/apply-frame-appearance frame))
    (when (and (bound-and-true-p rvb/ui-page-chrome-mode)
               (fboundp 'rvb/ui-page-chrome-refresh))
      (rvb/ui-page-chrome-refresh))))

(defvar rvb/ui-page-chrome--saved-header-lines nil)
(defvar rvb/ui-page-chrome--saved-line-number-faces nil
  "Alist mapping (FRAME . FACE) to the line-number background to restore.")

(defcustom rvb/ui-page-chrome-vertical-padding 0
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

(defface rvb/ui-page-chrome-command
  '((t :inherit rvb/ui-page-chrome-header))
  "Face supplying the page-chrome command-state colours.")

(defface rvb/ui-page-chrome-breadcrumb-highlight
  '((t :inherit highlight))
  "Face used when the pointer is over a page-chrome breadcrumb.")

(defface rvb/ui-page-chrome-scroll-trough
  '((t :inherit rvb/ui-page-chrome-header))
  "Face for the length of the buffer, in the header-line scrollbar.")

(defface rvb/ui-page-chrome-scroll-handle
  '((t :inherit region))
  "Face for the part of the buffer on screen, in the header-line scrollbar.")

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
                  (push "/" crumbs)
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
               (_ (when (and (integerp rvb/ui-page-chrome-vertical-padding)
                             (> rvb/ui-page-chrome-vertical-padding 0)
                             (stringp header-background))
                    (setq font-attrs
                          (append font-attrs
                                  (list :box
                                        `(:line-width
                                          (0 . ,rvb/ui-page-chrome-vertical-padding)
                                          :color ,header-background))))))
               (band-face
                (let ((faces (rvb/ui-page-chrome--band-faces face command-p)))
                  (if font-attrs (append faces (list font-attrs)) faces)))
               (width (or width (window-total-width window)))
               (content (truncate-string-to-width content width))
               (band (concat content
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
                   (rvb/ui-page-chrome--path-breadcrumb file t))
                  (default-directory
                   (rvb/ui-page-chrome--path-breadcrumb default-directory))
                  (t
                   (buffer-name))))
           ;; Not `mode-line-modified': the mode line carries that, and
           ;; saying it twice in two bands a screen apart is worse than
           ;; saying it once where it has always been.  What is left is
           ;; the things the mode line does not show at all.
           (status (format-mode-line
                    '("%e" mode-line-front-space
                      (:propertize
                       ("" mode-line-mule-info mode-line-client
                        mode-line-remote mode-line-window-dedicated)
                       display (min-width (6.0))))
                    nil window))
           ;; Reserve the right edge before truncating long paths so status
           ;; information can never be pushed out of the header.
           (path (truncate-string-to-width
                  path (max 0 (- width (string-width status) 4))))
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
        (put-text-property 0 (length bar) 'local-map keymap bar))
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
        (list band
              (rvb/ui-page-chrome--scroll-spacer
               (rvb/ui-page-chrome--band-faces 'rvb/ui-page-chrome-header
                                               command-p))
              bar)
      band)))

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
        mlscroll-border 0)
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

;;; Tab buttons as letters rather than pictures
;;
;; The buttons in the tab bar -- the close cross, the new-tab plus, the
;; history chevrons -- are icons, and on a graphical display an icon is
;; an image.  Two things follow, and both of them show:
;;
;;   The image is drawn with a face of its own, `shadow', which carries a
;;   background as well as a foreground.  That background is painted into
;;   the middle of whatever tab the button sits on, so the current tab
;;   ends up with a white patch and a black cross in it instead of its
;;   own colours.  No theme can fix that: one static face cannot be both
;;   of the two kinds of tab it lands on.
;;
;;   The image is `:margin 1', a pixel on every side, and taller than the
;;   text beside it.  The tab bar's row is sized to the tallest thing in
;;   it, and the pixels the tabs do not reach are painted with the bar's
;;   own face -- a pale line between the tabs and the header line right
;;   under them.
;;
;; Text has neither problem.  `tab-bar-tab-name-format-face' adds the
;; tab's face to the whole name, close button included, so the cross
;; takes the colours of the tab it belongs to; and a row of text is as
;; tall as text, so the current tab fills it to the edge.
;;
;; Defined here rather than assigned: `tab-bar--load-buttons' runs each
;; time `tab-bar-mode' is turned on and would overwrite a variable set
;; from here, but it defines each icon only `unless' one already exists.
(require 'icons)

(define-icon tab-bar-close nil
  '((text " ✕"))
  "Icon for closing the clicked tab."
  :version "30.1"
  :help-echo "Click to close tab")

(define-icon tab-bar-new nil
  '((text " + "))
  "Icon for creating a new tab."
  :version "30.1"
  :help-echo "New tab")

;; The history chevrons only appear with `tab-bar-history-mode', which
;; defines them when it is turned on -- same `unless', same treatment.
(define-icon tab-bar-back nil
  '((text " < "))
  "Icon for going back in tab history."
  :version "30.1")

(define-icon tab-bar-forward nil
  '((text " > "))
  "Icon for going forward in tab history."
  :version "30.1")

;; The tab line's close button is the same picture with the same face,
;; and its button variable is built when tab-line.el loads -- which is
;; after this file, so defining the icon here is enough there too.
(define-icon tab-line-close nil
  '((text " ✕"))
  "Icon for closing the clicked tab."
  :version "30.1"
  :help-echo "Click to close tab")

;; The button *strings* are built once, when `tab-bar-mode' is turned on,
;; from whatever icons existed at that moment.  A tab bar already running
;; when this file is loaded therefore keeps its pictures until the mode
;; is toggled.  Rebuilding here means re-evaluating this file is enough.
(when (fboundp 'tab-bar--load-buttons)
  (tab-bar--load-buttons))

(provide 'rvb-ui)
