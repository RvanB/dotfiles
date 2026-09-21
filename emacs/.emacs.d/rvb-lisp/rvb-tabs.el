;;; rvb-tabs.el --- The tab bar: shape, colours and buttons  -*- lexical-binding: t; -*-

;; The tab bar looks the same under every theme: slanted, slightly
;; rounded tabs, padded above so their names sit down on the header line
;; below, the current one in the header's own colour so that it opens
;; into it, the rest reversed out of a bar in the text colour.
;;
;; None of that is left to the theme.  A theme's tab faces style the
;; tabs its way, and the point is that they look like this whichever
;; theme is on -- so the tabs are drawn in faces of their own
;; (`rvb/tabs-active', `rvb/tabs-inactive'), which no theme mentions,
;; and their colours are worked out from the theme's text and page
;; colours each time one is enabled.  Only the colours follow the
;; theme; the shape is this file's.

(require 'cl-lib)
(require 'svg)
(require 'rvb-ui)

;;; Colours

(defcustom rvb/tab-bar-color nil
  "Colour of the tab bar and the tabs not current, or nil for the text colour.
The text colour reverses the bar out of the page, as a menu bar would be;
the current tab takes the header line's colour instead, so that the two
read as one piece."
  :type '(choice (const :tag "The text colour" nil) color)
  :group 'appearance)

(defface rvb/tabs-active '((t :weight bold))
  "Face for the current tab.  Its colours are set by `rvb/tabs--apply-faces'.")

(defface rvb/tabs-inactive '((t))
  "Face for tabs other than the current one.
Its colours are set by `rvb/tabs--apply-faces'.")

(defun rvb/tabs--face (tab)
  "Return the face TAB is drawn in.  For `tab-bar-tab-face-function'."
  (if (eq (car tab) 'current-tab) 'rvb/tabs-active 'rvb/tabs-inactive))

(defun rvb/tabs--apply-faces (&rest _)
  "Colour the tab bar from the current theme.

The two tab faces get default specs, as the page-chrome faces do.  The
bar itself is `tab-bar', which themes do style, so it is set outright
-- after the theme is enabled, which is when this runs -- and loses
any box or inherited font the theme or the stock face gives it: a box
would draw a raised edge around the bar, and the stock face inherits
`variable-pitch'."
  (let* ((fg (face-foreground 'default nil t))
         (bg (face-background 'default nil t))
         (header (or (face-background 'rvb/ui-page-chrome-header nil t) bg))
         (bar (or rvb/tab-bar-color fg)))
    (when (and fg bg)
      (face-spec-set 'rvb/tabs-active
                     `((t :foreground ,fg :background ,header
                          :weight bold :box nil))
                     'face-defface-spec)
      (face-spec-set 'rvb/tabs-inactive
                     `((t :foreground ,bg :background ,bar :box nil))
                     'face-defface-spec)
      (set-face-attribute 'tab-bar nil
                          :inherit 'unspecified :box 'unspecified
                          :foreground bg :background bar)
      ;; Group names, for anyone using tab groups, match the tabs.
      (dolist (pair '((tab-bar-tab-group-current . rvb/tabs-active)
                      (tab-bar-tab-group-inactive . rvb/tabs-inactive)
                      (tab-bar-tab-ungrouped . rvb/tabs-inactive)))
        (set-face-attribute (car pair) nil :inherit (cdr pair)
                            :foreground 'unspecified :background 'unspecified
                            :box 'unspecified :weight 'unspecified)))))

(setq tab-bar-tab-face-function #'rvb/tabs--face)
;; After the page chrome's own faces are worked out (depth 0), since the
;; current tab takes the header's colour.
(add-hook 'enable-theme-functions #'rvb/tabs--apply-faces 90)
(rvb/tabs--apply-faces)

;;;; Tab buttons as letters rather than pictures
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
;; The cross is U+00D7 MULTIPLICATION SIGN rather than U+2715: SF Mono
;; has the one and not the other, and a character the default font
;; lacks sends Emacs looking through every installed font for it the
;; first time it is drawn -- over half a second of every startup, since
;; the tab bar is the first thing drawn.
;;
;; Defined here rather than assigned: `tab-bar--load-buttons' runs each
;; time `tab-bar-mode' is turned on and would overwrite a variable set
;; from here, but it defines each icon only `unless' one already exists.
(require 'icons)

(define-icon tab-bar-close nil
  '((text " ×"))
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
  '((text " ×"))
  "Icon for closing the clicked tab."
  :version "30.1"
  :help-echo "Click to close tab")

;; A space either side of each tab's label -- its name and close cross --
;; so the first tab's name does not start hard against the edge of the
;; frame, which has no border to hold it off.  Added before the face is,
;; so the padding takes the tab's colours and reads as part of the tab.
(defun rvb/tab-bar-tab-name-format-padding (name _tab _i)
  "Return NAME with a space either side.
For `tab-bar-tab-name-format-functions'."
  (concat " " name " "))

(defcustom rvb/tab-bar-slants t
  "Whether tabs have slanted sides, on a graphical display."
  :type 'boolean
  :group 'appearance)

(defcustom rvb/tab-bar-vertical-padding '(3 . 0)
  "Padding around tab names, on a graphical display with slanted tabs.
Pixels above and below alike, or (TOP . BOTTOM).  None below by
default, so the names sit down on the header line under them."
  :type '(choice integer (cons integer integer))
  :group 'appearance)

(defcustom rvb/tab-bar-corner-radius 2
  "Radius, in pixels, of the rounding on a slanted tab's corners."
  :type 'integer
  :group 'appearance)

(defvar rvb/tab-bar--slant-images (make-hash-table :test #'equal)
  "Slant images already made, by side, size and colours.")

(defun rvb/tab-bar--slant-image (side metrics tab bar)
  "Return the image for a tab's SIDE, `left' or `right'.
The tab's slanted edge, in TAB colour against BAR colour, as tall as
the padded tab bar per METRICS.  Drawn as the left side and mirrored
for the right.

Both ends of the slant are rounded by `rvb/tab-bar-corner-radius': at
the foot it curves out into the bar, at the top into the tab's top
edge.  The slant stops that far short of the image's edges so each
curve can finish flat -- the top one exactly where the tab's name,
which is flat-topped, carries on."
  (let ((key (list side metrics tab bar rvb/tab-bar-corner-radius)))
    (or (gethash key rvb/tab-bar--slant-images)
        (puthash
         key
         (let* ((height (plist-get metrics :height))
                (r (max 0 rvb/tab-bar-corner-radius))
                ;; Steep enough to read as a tab, not as a wedge.
                (run (max 4 (round height 2.5)))
                (width (+ run (* 2 r)))
                (svg (svg-create width height))
                ;; The slant runs from A, on the bottom, to B, on the top.
                (ax r) (bx (- width r))
                ;; Where the rounding meets it, R up from the bottom and
                ;; R down from the top.
                (fx (+ ax (* run (/ (float r) height))))
                (tx (- bx (* run (/ (float r) height))))
                (flip (lambda (x) (if (eq side 'left) x (- width x))))
                (d (format "M %s %d Q %s %d %s %d L %s %d Q %s 0 %s 0 L %s %d Z"
                           (funcall flip 0) height
                           (funcall flip ax) height
                           (funcall flip fx) (- height r)
                           (funcall flip tx) r
                           (funcall flip bx)
                           (funcall flip width)
                           (funcall flip width) height)))
           (svg-rectangle svg 0 0 width height :fill bar)
           (dom-append-child svg (dom-node 'path `((d . ,d) (fill . ,tab))))
           (svg-image svg :ascent (plist-get metrics :ascent)))
         rvb/tab-bar--slant-images))))

(defun rvb/tab-bar-tab-name-format-slants (name tab _i)
  "Return NAME between slanted sides, unless `rvb/tab-bar-slants' is off.
For `tab-bar-tab-name-format-functions', after the face is applied: the
sides are images in the tab's colours, and carry no face of their own.

Which also means `tab-bar-auto-width' no longer recognises the tab --
it looks for a tab face on the first character -- so tabs are as wide
as their names rather than stretched across the bar.  Stretched, the
padding it inserts would land outside the right-hand slant."
  (if-let* ((rvb/tab-bar-slants)
            ((display-graphic-p))
            (metrics (rvb/ui--line-metrics nil 'tab-bar
                                           rvb/tab-bar-vertical-padding))
            (tab-color (face-background (funcall tab-bar-tab-face-function tab)
                                        nil t))
            (bar-color (face-background 'tab-bar nil t)))
      ;; The text under each image names its colours.  It is never
      ;; seen, but the tab bar is only redrawn when its *text* changes --
      ;; properties, images among them, are not compared -- so without
      ;; it, switching themes would leave the old theme's slants up.
      (let ((key (format "%s%s" tab-color bar-color)))
        (concat (propertize key 'display (rvb/tab-bar--slant-image
                                          'left metrics tab-color bar-color))
                name
                (propertize key 'display (rvb/tab-bar--slant-image
                                          'right metrics tab-color bar-color))))
    name))

(setq tab-bar-tab-name-format-functions
      '(tab-bar-tab-name-format-hints
        tab-bar-tab-name-format-close-button
        rvb/tab-bar-tab-name-format-padding
        tab-bar-tab-name-format-face
        rvb/tab-bar-tab-name-format-slants))

;; The button *strings* are built once, when `tab-bar-mode' is turned on,
;; from whatever icons existed at that moment.  A tab bar already running
;; when this file is loaded therefore keeps its pictures until the mode
;; is toggled.  Rebuilding here means re-evaluating this file is enough.
(when (fboundp 'tab-bar--load-buttons)
  (tab-bar--load-buttons))

(tab-bar-mode 1)

(provide 'rvb-tabs)
;;; rvb-tabs.el ends here
