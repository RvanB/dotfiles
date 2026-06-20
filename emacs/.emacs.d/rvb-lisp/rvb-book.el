;;; rvb-book.el --- C64-style outer frame margins -*- lexical-binding: t; -*-

(require 'seq)
(require 'transient)

(defgroup book-mode nil
  "C64-style outer margins for Emacs frames."
  :group 'windows
  :prefix "book-mode-")

(defvar book-mode nil)
(defvar book-mode--refreshing nil)
(defvar book-mode--saved-frame-borders nil)
(defvar book-mode--extra-border-buffers nil)
(defvar book-mode--saved-content-window-margins nil)
(defvar book-mode--saved-minibuffer-margins nil)
(defvar book-mode--saved-side-window-margins nil)
(defvar book-mode--saved-fringe-faces nil)

(defconst book-mode--line-number-gutter-extra-columns 1
  "Native line-number gutter columns not reported by `line-number-display-width'.")

(defun book-mode--custom-set-and-refresh (symbol value)
  "Set SYMBOL to VALUE and refresh `book-mode' when it is active."
  (set-default symbol value)
  (when book-mode
    (book-mode-refresh)))

(defcustom book-mode-horizontal-margin-ratio (/ 2.0 9.0)
  "Outside margin ratio for the left and right sides of each frame.

The value is a fraction of the frame width.  The default 2/9 gives each
side a book-like outside margin."
  :type 'number
  :group 'book-mode
  :set #'book-mode--custom-set-and-refresh)

(defcustom book-mode-top-margin-ratio (/ 1.0 9.0)
  "Outside margin ratio for the top of each frame.

The value is a fraction of the frame height.  The default is 1/9."
  :type 'number
  :group 'book-mode
  :set #'book-mode--custom-set-and-refresh)

(defcustom book-mode-bottom-margin-ratio (/ 2.0 9.0)
  "Outside margin ratio for the bottom of each frame.

The value is a fraction of the frame height.  The default is 2/9."
  :type 'number
  :group 'book-mode
  :set #'book-mode--custom-set-and-refresh)

(defcustom book-mode-fringe-inherit-body t
  "When non-nil, make the fringe background match the default face.

This prevents the fringe from appearing as a contrasting vertical line
between content windows and book-mode margin strips."
  :type 'boolean
  :group 'book-mode
  :set #'book-mode--custom-set-and-refresh)

(defcustom book-mode-inside-horizontal-margins t
  "When non-nil, add half-width horizontal margins to content windows.

Every content window receives half the configured outside horizontal
margin on both sides.  The frame border contributes the other half on
the outer frame edges, and adjacent windows contribute one half each
between them."
  :type 'boolean
  :group 'book-mode
  :set #'book-mode--custom-set-and-refresh)

(defun book-mode--set-ratio (variable prompt)
  "Read a ratio for VARIABLE with PROMPT, persist it, and refresh."
  (let ((value (read-number prompt (symbol-value variable))))
    (customize-save-variable variable value)
    (when book-mode
      (book-mode-refresh))))

(defun book-mode-set-horizontal-ratio ()
  "Set `book-mode-horizontal-margin-ratio'."
  (interactive)
  (book-mode--set-ratio 'book-mode-horizontal-margin-ratio
                        "Horizontal ratio: "))

(defun book-mode-set-top-ratio ()
  "Set `book-mode-top-margin-ratio'."
  (interactive)
  (book-mode--set-ratio 'book-mode-top-margin-ratio
                        "Top ratio: "))

(defun book-mode-set-bottom-ratio ()
  "Set `book-mode-bottom-margin-ratio'."
  (interactive)
  (book-mode--set-ratio 'book-mode-bottom-margin-ratio
                        "Bottom ratio: "))

(defun book-mode--toggle-custom (variable)
  "Toggle boolean custom VARIABLE and refresh book-mode."
  (customize-save-variable variable (not (symbol-value variable)))
  (when book-mode
    (book-mode-refresh)))

(defun book-mode-toggle-fringe-inherit-body ()
  "Toggle `book-mode-fringe-inherit-body'."
  (interactive)
  (if book-mode-fringe-inherit-body
      (progn
        (book-mode--restore-fringe-faces)
        (book-mode--toggle-custom 'book-mode-fringe-inherit-body))
    (book-mode--toggle-custom 'book-mode-fringe-inherit-body)))

(defun book-mode-toggle-inside-horizontal-margins ()
  "Toggle `book-mode-inside-horizontal-margins'."
  (interactive)
  (if book-mode-inside-horizontal-margins
      (progn
        (book-mode--restore-content-window-margins)
        (book-mode--toggle-custom 'book-mode-inside-horizontal-margins))
    (book-mode--toggle-custom 'book-mode-inside-horizontal-margins)))

(defun book-mode-reset-ratios ()
  "Reset book-mode margin ratios to book defaults."
  (interactive)
  (customize-save-variable 'book-mode-horizontal-margin-ratio (/ 2.0 9.0))
  (customize-save-variable 'book-mode-top-margin-ratio (/ 1.0 9.0))
  (customize-save-variable 'book-mode-bottom-margin-ratio (/ 2.0 9.0))
  (when book-mode
    (book-mode-refresh)))

(defun book-mode--ratio-description (variable label)
  "Return menu description for ratio VARIABLE with LABEL."
  (format "%-10s %.3f" label (symbol-value variable)))

(defun book-mode--toggle-description (variable label)
  "Return menu description for boolean VARIABLE with LABEL."
  (format "%-18s %s" label (if (symbol-value variable) "on" "off")))

(transient-define-prefix book-mode-menu ()
  "Open the book-mode control menu."
  [["Mode"
    ("m" "Toggle book-mode" book-mode)
    ("r" "Refresh" book-mode-refresh)]
   ["Ratios"
    ("h" (lambda ()
           (book-mode--ratio-description
            'book-mode-horizontal-margin-ratio "Horizontal"))
     book-mode-set-horizontal-ratio)
    ("t" (lambda ()
           (book-mode--ratio-description
            'book-mode-top-margin-ratio "Top"))
     book-mode-set-top-ratio)
    ("b" (lambda ()
           (book-mode--ratio-description
            'book-mode-bottom-margin-ratio "Bottom"))
     book-mode-set-bottom-ratio)
    ("0" "Reset ratios" book-mode-reset-ratios)]
   ["Options"
    ("F" (lambda ()
           (book-mode--toggle-description
            'book-mode-fringe-inherit-body "Blend fringe"))
     book-mode-toggle-fringe-inherit-body)
    ("I" (lambda ()
           (book-mode--toggle-description
            'book-mode-inside-horizontal-margins "Inside margins"))
     book-mode-toggle-inside-horizontal-margins)]
   ["Customize"
    ("c" "Customize group" (lambda ()
                             (interactive)
                             (customize-group 'book-mode)))]])

(defun book-mode--ratio (value)
  "Clamp VALUE to a usable margin ratio."
  (min 0.45 (max 0 (or value 0))))

(defun book-mode--horizontal-pixels (frame)
  "Return the horizontal outside margin for FRAME in pixels."
  (* (frame-char-width frame)
     (floor (* (frame-width frame)
               (book-mode--ratio book-mode-horizontal-margin-ratio)))))

(defun book-mode--horizontal-columns (frame)
  "Return the horizontal outside margin for FRAME in columns."
  (let ((columns (floor (* (frame-width frame)
                           (book-mode--ratio
                            book-mode-horizontal-margin-ratio)))))
    (- columns (% columns 2))))

(defun book-mode--half-horizontal-columns (frame)
  "Return half the horizontal outside margin for FRAME in columns."
  (/ (book-mode--horizontal-columns frame) 2))

(defun book-mode--half-horizontal-pixels (frame)
  "Return half the horizontal outside margin for FRAME in pixels."
  (* (frame-char-width frame)
     (book-mode--half-horizontal-columns frame)))

(defun book-mode--top-pixels (frame)
  "Return the top outside margin for FRAME in pixels."
  (* (frame-char-height frame)
     (floor (* (frame-height frame)
               (book-mode--ratio book-mode-top-margin-ratio)))))

(defun book-mode--bottom-pixels (frame)
  "Return the bottom outside margin for FRAME in pixels."
  (* (frame-char-height frame)
     (floor (* (frame-height frame)
               (book-mode--ratio book-mode-bottom-margin-ratio)))))

(defun book-mode--border-pixels (frame)
  "Return the shared frame border width for FRAME in pixels."
  (min (book-mode--half-horizontal-pixels frame)
       (book-mode--top-pixels frame)
       (book-mode--bottom-pixels frame)))

(defun book-mode--extra-size (side frame)
  "Return SIDE's extra margin size for FRAME in columns or lines."
  (let* ((border (book-mode--border-pixels frame))
         (pixels (pcase side
                   ((or 'left 'right) (book-mode--half-horizontal-pixels frame))
                   ('top (book-mode--top-pixels frame))
                   ('bottom (book-mode--bottom-pixels frame))))
         (unit (if (memq side '(left right))
                   (frame-char-width frame)
                 (frame-char-height frame))))
    (max 0 (floor (/ (- pixels border) unit)))))

(defun book-mode--save-frame-border (frame)
  "Remember FRAME's original internal border width."
  (unless (assq frame book-mode--saved-frame-borders)
    (push (cons frame (frame-parameter frame 'internal-border-width))
          book-mode--saved-frame-borders)))

(defun book-mode--apply-frame (frame)
  "Apply book-mode border to FRAME."
  (book-mode--save-frame-border frame)
  (modify-frame-parameters
   frame
   `((internal-border-width . ,(book-mode--border-pixels frame)))))

(defun book-mode--extra-buffer-name (side frame)
  "Return the extra border buffer name for SIDE in FRAME."
  (format " *book-mode-extra-%s-%s*" side (frame-parameter frame 'name)))

(defun book-mode--extra-buffer (side frame)
  "Return the extra border buffer for SIDE in FRAME."
  (let ((buffer (get-buffer-create (book-mode--extra-buffer-name side frame))))
    (with-current-buffer buffer
      (setq-local cursor-type nil)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      (setq-local vertical-scroll-bar nil)
      (setq-local horizontal-scroll-bar nil)
      (setq-local window-size-fixed nil))
    (push buffer book-mode--extra-border-buffers)
    buffer))

(defun book-mode--extra-window-p (window)
  "Return non-nil when WINDOW is a book-mode extra border window."
  (window-parameter window 'book-mode-extra-border))

(defun book-mode--managed-frame-p (frame)
  "Return non-nil when FRAME is an ordinary frame managed by book-mode.

Child frames, including posframes used by transient menus, borrow window
state from their parent frame.  They are short-lived UI overlays rather
than editing frames, so applying borders or creating side windows in them
can disturb the parent's window configuration."
  (and (frame-live-p frame)
       (not (frame-parameter frame 'parent-frame))))

(defun book-mode--regular-window-count (frame)
  "Return the number of non-margin windows in FRAME."
  (length
   (seq-remove #'book-mode--extra-window-p
               (window-list frame 'no-minibuf))))

(defun book-mode--regular-window-p (window)
  "Return non-nil when WINDOW is a regular book-mode content window."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (book-mode--extra-window-p window))))

(defun book-mode--content-window-p (window)
  "Return non-nil when WINDOW should receive inside content margins."
  (and (book-mode--regular-window-p window)
       (not (window-parameter window 'window-side))))

(defun book-mode--user-side-window-p (window)
  "Return non-nil when WINDOW is a non-book side window."
  (and (window-live-p window)
       (window-parameter window 'window-side)
       (not (book-mode--extra-window-p window))))

(defun book-mode--delete-extra-windows (&optional frame)
  "Delete book-mode extra border windows.

When FRAME is non-nil, only delete extra border windows in FRAME."
  (dolist (target-frame (if frame (list frame) (frame-list)))
    (dolist (window (window-list target-frame 'no-minibuf))
      (when (book-mode--extra-window-p window)
        (delete-window window)))))

(defun book-mode--content-windows (frame)
  "Return regular content windows in FRAME."
  (seq-filter #'book-mode--content-window-p
              (window-list frame 'no-minibuf)))

(defun book-mode--save-content-window-margins (window)
  "Remember WINDOW's margins before book-mode changes them."
  (unless (assq window book-mode--saved-content-window-margins)
    (push (cons window (window-margins window))
          book-mode--saved-content-window-margins)))

(defun book-mode--line-number-gutter-columns (window)
  "Return columns used by line numbers and the left fringe in WINDOW."
  (if (buffer-local-value 'display-line-numbers (window-buffer window))
      (let* ((frame (window-frame window))
             (fringe-pixels (car (window-fringes window)))
             (fringe-columns (ceiling (/ (float fringe-pixels)
                                         (frame-char-width frame))))
             (line-number-columns
              (with-selected-window window
                (line-number-display-width))))
        (+ line-number-columns
           fringe-columns
           book-mode--line-number-gutter-extra-columns))
    0))

(defun book-mode--apply-content-window-margins (window)
  "Apply equal half-width horizontal margins to WINDOW.

Every content window receives the same visual padding on both sides.  Line
numbers and the left fringe use part of that left-side budget, keeping text
in the same column without widening the overall gutter."
  (book-mode--save-content-window-margins window)
  (let* ((half-margin (book-mode--half-horizontal-columns
                       (window-frame window)))
         ;; Line numbers render between the left window margin and buffer
         ;; text.  Reserve their width from the blank part of the book
         ;; gutter so the text still begins at the same visual column.
         (left-margin (max 0 (- half-margin
                                (book-mode--line-number-gutter-columns
                                 window)))))
    (set-window-margins window left-margin half-margin)))

(defun book-mode--apply-content-windows-margins (frame)
  "Apply inside horizontal margins to content windows in FRAME."
  (when book-mode-inside-horizontal-margins
    (dolist (window (book-mode--content-windows frame))
      (book-mode--apply-content-window-margins window))))

(defun book-mode--restore-content-window-margins ()
  "Restore content window margins saved before book-mode changed them."
  (dolist (entry book-mode--saved-content-window-margins)
    (let ((window (car entry))
          (margins (cdr entry)))
      (when (window-live-p window)
        (set-window-margins window (car margins) (cdr margins)))))
  (setq book-mode--saved-content-window-margins nil))

(defun book-mode--display-extra-window (side frame size)
  "Display an extra border window on SIDE of FRAME with SIZE."
  (let* ((size-key (if (memq side '(left right)) 'window-width 'window-height))
         (slot (pcase side
                 ((or 'left 'top) -1)
                 ((or 'right 'bottom) 1)))
         (window
          (display-buffer-in-side-window
           (book-mode--extra-buffer side frame)
           `((side . ,side)
             (slot . ,slot)
             (,size-key . ,size)
             (dedicated . t)
             (window-parameters
              . ((no-other-window . t)
                 (no-delete-other-windows . t)
                 (mode-line-format . none)))))))
    (set-window-dedicated-p window t)
    (set-window-parameter window 'book-mode-extra-border t)
    (set-window-parameter window 'no-other-window t)
    (set-window-parameter window 'no-delete-other-windows t)
    window))

(defun book-mode--apply-extra-margins (frame)
  "Apply extra side-specific spacing to FRAME when there is room."
  (book-mode--delete-extra-windows frame)
  (let ((state (window-state-get (frame-root-window frame) t))
        (regular-count (book-mode--regular-window-count frame)))
    (condition-case nil
        (progn
          ;; Horizontal padding is applied as window margins.  Unlike side
          ;; windows, margins do not acquire an asymmetric minimum width.
          (dolist (side '(top bottom))
            (let ((size (book-mode--extra-size side frame)))
              (when (> size 0)
                (book-mode--display-extra-window side frame size))))
          (unless (= regular-count (book-mode--regular-window-count frame))
            (window-state-put state (frame-root-window frame) 'safe)
            (book-mode--delete-extra-windows frame)))
      (error
       (window-state-put state (frame-root-window frame) 'safe)
       (book-mode--delete-extra-windows frame)))))

(defun book-mode--save-minibuffer-margins (window)
  "Remember WINDOW's minibuffer margins before book-mode changes them."
  (unless (assq window book-mode--saved-minibuffer-margins)
    (push (cons window (window-margins window))
          book-mode--saved-minibuffer-margins)))

(defun book-mode--apply-minibuffer-margins (frame)
  "Apply horizontal book-mode margins to FRAME's minibuffer window."
  (let ((window (minibuffer-window frame)))
    (when (window-live-p window)
      (book-mode--save-minibuffer-margins window)
      (let ((half-margin (book-mode--half-horizontal-columns frame)))
        (set-window-margins window half-margin half-margin)))))

(defun book-mode--restore-minibuffer-margins ()
  "Restore minibuffer margins saved before book-mode changed them."
  (dolist (entry book-mode--saved-minibuffer-margins)
    (let ((window (car entry))
          (margins (cdr entry)))
      (when (window-live-p window)
        (set-window-margins window (car margins) (cdr margins)))))
  (setq book-mode--saved-minibuffer-margins nil))

(defun book-mode--save-side-window-margins (window)
  "Remember WINDOW's side-window margins before book-mode changes them."
  (unless (assq window book-mode--saved-side-window-margins)
    (push (cons window (window-margins window))
          book-mode--saved-side-window-margins)))

(defun book-mode--apply-side-window-margins (window)
  "Apply horizontal book-mode margins to side WINDOW."
  (when (book-mode--user-side-window-p window)
    (let ((frame (window-frame window)))
      (book-mode--save-side-window-margins window)
      (set-window-margins window
                          (book-mode--extra-size 'left frame)
                          (book-mode--extra-size 'right frame)))))

(defun book-mode--apply-side-windows-margins (frame)
  "Apply horizontal book-mode margins to non-book side windows in FRAME."
  (walk-windows #'book-mode--apply-side-window-margins 'no-minibuf frame))

(defun book-mode--restore-side-window-margins ()
  "Restore side-window margins saved before book-mode changed them."
  (dolist (entry book-mode--saved-side-window-margins)
    (let ((window (car entry))
          (margins (cdr entry)))
      (when (window-live-p window)
        (set-window-margins window (car margins) (cdr margins)))))
  (setq book-mode--saved-side-window-margins nil))

(defun book-mode--save-fringe-face (frame)
  "Remember FRAME's fringe face attributes before book-mode changes them."
  (unless (assq frame book-mode--saved-fringe-faces)
    (push (list frame
                (face-attribute 'fringe :background frame 'default)
                (face-attribute 'fringe :foreground frame 'default)
                (face-attribute 'fringe :inherit frame 'default))
          book-mode--saved-fringe-faces)))

(defun book-mode--apply-fringe-face (frame)
  "Make FRAME's fringe background inherit the body color."
  (when book-mode-fringe-inherit-body
    (book-mode--save-fringe-face frame)
    (set-face-attribute 'fringe
                        frame
                        :background (face-background 'default frame)
                        :foreground 'unspecified
                        :inherit 'default)))

(defun book-mode--restore-fringe-faces ()
  "Restore fringe face attributes changed by book-mode."
  (dolist (entry book-mode--saved-fringe-faces)
    (pcase-let ((`(,frame ,background ,foreground ,inherit) entry))
      (when (frame-live-p frame)
        (set-face-attribute 'fringe
                            frame
                            :background background
                            :foreground foreground
                            :inherit inherit))))
  (setq book-mode--saved-fringe-faces nil))

(defun book-mode--restore-frame (frame)
  "Restore FRAME's internal border width."
  (let ((saved (alist-get frame book-mode--saved-frame-borders)))
    (modify-frame-parameters frame `((internal-border-width . ,saved)))))

(defun book-mode-refresh ()
  "Refresh book-mode margins in all live frames."
  (interactive)
  (when (and book-mode (not book-mode--refreshing))
    (let ((book-mode--refreshing t))
      (dolist (frame (frame-list))
        (when (book-mode--managed-frame-p frame)
          (book-mode--apply-frame frame)
          (book-mode--apply-fringe-face frame)
          (book-mode--apply-extra-margins frame)
          (book-mode--apply-content-windows-margins frame)
          (book-mode--apply-minibuffer-margins frame)
          (book-mode--apply-side-windows-margins frame))))))

(defun book-mode--window-configuration-change ()
  "Style windows after a layout change without rebuilding border windows.

Transient and posframe displays also trigger this hook.  Recreating the
border side windows here can alter a multi-window editing layout, so only
apply per-window presentation settings.  `book-mode-refresh' performs a
full rebuild when frame geometry changes or when explicitly requested."
  (when (and book-mode (not book-mode--refreshing))
    (let ((book-mode--refreshing t))
      (dolist (frame (frame-list))
        (when (book-mode--managed-frame-p frame)
          (book-mode--apply-content-windows-margins frame)
          (book-mode--apply-minibuffer-margins frame)
          (book-mode--apply-side-windows-margins frame))))))

(defun book-mode--line-number-mode-change ()
  "Refresh book gutters after line numbers are enabled or disabled."
  (when book-mode
    ;; This is deliberately non-structural: changing line numbers should not
    ;; rebuild the top and bottom border windows.
    (book-mode--window-configuration-change)
    (when (fboundp 'rvb/ui-page-chrome-refresh)
      (rvb/ui-page-chrome-refresh))))

;; Keep a live session safe when this file is re-evaluated after upgrading
;; book-mode from the older hook implementation.
(when book-mode
  (remove-hook 'window-configuration-change-hook #'book-mode-refresh)
  (add-hook 'window-configuration-change-hook
            #'book-mode--window-configuration-change)
  (add-hook 'display-line-numbers-mode-hook
            #'book-mode--line-number-mode-change))

(defun book-mode--enable ()
  "Enable book-mode internals."
  (add-hook 'window-size-change-functions #'book-mode--window-size-change)
  (add-hook 'window-configuration-change-hook
            #'book-mode--window-configuration-change)
  (add-hook 'display-line-numbers-mode-hook
            #'book-mode--line-number-mode-change)
  (add-hook 'after-make-frame-functions #'book-mode--after-make-frame)
  (book-mode-refresh))

(defun book-mode--disable ()
  "Disable book-mode internals."
  (remove-hook 'window-size-change-functions #'book-mode--window-size-change)
  (remove-hook 'window-configuration-change-hook
               #'book-mode--window-configuration-change)
  (remove-hook 'display-line-numbers-mode-hook
               #'book-mode--line-number-mode-change)
  (remove-hook 'after-make-frame-functions #'book-mode--after-make-frame)
  (book-mode--delete-extra-windows)
  (dolist (buffer (delete-dups book-mode--extra-border-buffers))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setq book-mode--extra-border-buffers nil)
  (dolist (frame (frame-list))
    (book-mode--restore-frame frame))
  (book-mode--restore-content-window-margins)
  (book-mode--restore-minibuffer-margins)
  (book-mode--restore-side-window-margins)
  (book-mode--restore-fringe-faces)
  (setq book-mode--saved-frame-borders nil))

(defun book-mode--window-size-change (_frame)
  "Refresh book-mode after a frame or window size change."
  (book-mode-refresh))

(defun book-mode--after-make-frame (frame)
  "Apply book-mode margins to newly created FRAME."
  (when (and book-mode (book-mode--managed-frame-p frame))
    (book-mode--apply-frame frame)
    (book-mode--apply-fringe-face frame)
    (book-mode--apply-extra-margins frame)
    (book-mode--apply-content-windows-margins frame)
    (book-mode--apply-minibuffer-margins frame)
    (book-mode--apply-side-windows-margins frame)))

;;;###autoload
(define-minor-mode book-mode
  "Toggle C64-style outer margins around each Emacs frame.

This uses the frame's `internal-border-width' as the shared margin base,
then adds side-specific extra strips to reach the configured left/right,
top, and bottom ratios.  Extra strips are dropped before they can replace
regular editing windows.  Side-by-side content windows receive inside
horizontal margins equal to half the outside horizontal margin on each
side.  The minibuffer receives the same left/right
window margins so its prompt follows the same horizontal spread, and
non-book side windows such as transient popups receive the same horizontal
margins.  When
`book-mode-fringe-inherit-body' is non-nil, the fringe background is
matched to the default face."
  :global t
  :lighter " Book"
  (if book-mode
      (book-mode--enable)
    (book-mode--disable)))

;; A live session may still have page chrome installed by an older version of
;; book-mode.  Restore it before `rvb/ui-page-chrome-mode' takes ownership.
(when (fboundp 'book-mode--restore-top-modelines)
  (book-mode--restore-top-modelines))

(provide 'rvb-book)

;;; rvb-book.el ends here
