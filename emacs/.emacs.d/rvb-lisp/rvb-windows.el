;;; rvb-windows.el --- Window layout behaviour  -*- lexical-binding: t; -*-

;; Some buffers are destinations rather than references: a feature
;; dashboard, a status buffer.  Squeezing one into half a split wastes
;; the frame and makes it harder to read.  These take the whole frame,
;; and quitting puts back the layout they replaced.
;;
;; Emacs ships `display-buffer-full-frame', but it works by calling
;; `delete-other-windows' -- the other windows are gone, and the
;; `quit-restore' parameter records only enough to restore the single
;; window that was reused.  So quitting leaves you with the one window
;; rather than the layout you started from.  The missing half is saving
;; the window configuration on the way in and restoring it on the way
;; out, which is what this file adds.
;;
;; It saves the exact configuration rather than undoing the last window
;; change (`winner-mode' and the like) because those are not the same
;; thing: by the time you quit a status buffer you may have opened a
;; file, a `magit-status', a description buffer, each of which changed
;; the layout again.  Stepping back one change would not get you home,
;; and nothing says how many steps would.  Quitting should need no
;; thought.

(require 'cl-lib)

(defgroup rvb/windows nil
  "Window layout behaviour."
  :group 'windows
  :prefix "rvb/")

(defcustom rvb/full-frame-modes
  '()
  "Major modes whose buffers should take over the whole frame.
Derived modes count, so naming a parent mode covers its children.
Quitting such a buffer restores the layout that preceded it.

A feature's status buffer is deliberately not here: it is where you
work, next to the code it describes, so it replaces the window you
called it from rather than the whole frame.  `rvb/toggle-full-frame'
zooms it when you do want the frame."
  :type '(repeat symbol)
  :group 'rvb/windows)

(defvar rvb/full-frame--configurations
  (make-hash-table :test #'eq :weakness 'key)
  "Window configuration to restore, keyed by the buffer that replaced it.
Weak on its keys, so a buffer that is killed without going through
`quit-window' does not pin its old layout forever.")

(defun rvb/full-frame-buffer-p (buffer &optional _alist)
  "Return non-nil if BUFFER should be displayed full-frame."
  (when-let* ((buffer (get-buffer buffer)))
    (with-current-buffer buffer
      (and rvb/full-frame-modes
           (derived-mode-p rvb/full-frame-modes)
           t))))

(defun rvb/display-buffer-full-frame (buffer alist)
  "Display BUFFER alone in the frame, remembering the layout it replaces.
An action function for `display-buffer'."
  ;; Nothing worth restoring if the frame already holds one window --
  ;; and saving then would make quitting a no-op that looks like a bug.
  (let ((config (unless (one-window-p t) (current-window-configuration))))
    (when-let* ((window (display-buffer-full-frame buffer alist)))
      ;; Never overwrite: re-displaying an open dashboard must not
      ;; replace the original layout with the full-frame one.
      (when (and config (not (gethash buffer rvb/full-frame--configurations)))
        (puthash buffer config rvb/full-frame--configurations))
      window)))

(defun rvb/full-frame-restore (buffer)
  "Restore the layout BUFFER replaced, if there is one."
  (when-let* ((config (gethash buffer rvb/full-frame--configurations)))
    (remhash buffer rvb/full-frame--configurations)
    ;; Only if we are still on the frame the configuration came from;
    ;; restoring across frames would yank the user somewhere else.
    (when (and (window-configuration-p config)
               (eq (window-configuration-frame config) (selected-frame)))
      (set-window-configuration config)
      t)))

(define-advice quit-window (:around (fn &optional kill window) rvb/full-frame)
  "Restore the layout a full-frame buffer replaced.
No-op for every buffer that did not take over the frame."
  (let ((buffer (window-buffer (or window (selected-window)))))
    (funcall fn kill window)
    (rvb/full-frame-restore buffer)))

(defun rvb/full-frame-restore-on-kill ()
  "Restore the layout when a full-frame buffer is killed outright.
For exits that bypass `quit-window', such as
`rvb/kill-buffer-and-close-window'."
  ;; `set-window-configuration' selects a window and so changes the
  ;; current buffer; `kill-buffer' still needs ours to be current.
  (save-current-buffer
    (rvb/full-frame-restore (current-buffer))))

;;;###autoload
(defun rvb/toggle-full-frame ()
  "Fill the frame with the selected window, or put the layout back.

Uses the same saved configuration as `rvb/full-frame-modes', so a
buffer zoomed by hand is also restored when you quit or kill it -- and
zooming a buffer that arrived full-frame simply un-zooms it."
  (interactive)
  (cond
   ((rvb/full-frame-restore (current-buffer)))
   ((one-window-p t)
    (user-error "Already the only window"))
   (t
    (puthash (current-buffer) (current-window-configuration)
             rvb/full-frame--configurations)
    (delete-other-windows))))

(add-hook 'kill-buffer-hook #'rvb/full-frame-restore-on-kill)

(add-to-list 'display-buffer-alist
             '(rvb/full-frame-buffer-p (rvb/display-buffer-full-frame)))


;;; Stepping through results reuses one window

;; `compilation-goto-locus' displays each hit with
;;
;;     (let ((pop-up-windows t)) (pop-to-buffer source 'other-window))
;;
;; when you are inside the results buffer.  That action tries
;; `display-buffer-pop-up-window' first, and it force-binds
;; `pop-up-windows', so every result splits the frame again: four hits
;; leave you with five windows.  `display-buffer-alist' takes precedence
;; over the action a caller passes, so matching here is what overrides
;; it -- reuse a window instead of making one.

(defcustom rvb/results-modes '(compilation-mode occur-mode)
  "Modes listing results that you step through one at a time.
While one of these is the selected window, other buffers are shown by
reusing a window rather than splitting.  `grep-mode' and friends derive
from `compilation-mode', so they are covered."
  :type '(repeat symbol)
  :group 'rvb/windows)

(defun rvb/displaying-from-results-p (_buffer &optional alist)
  "Return non-nil when the selected window holds a results buffer.

Stands down when the caller explicitly allowed the selected window --
`pop-to-buffer-same-window' passes `inhibit-same-window' nil.  This
rule is for the hits you step through, not for a buffer you asked to
see where you are standing, and `display-buffer-alist' otherwise
overrides the caller."
  (let ((asked-for-same-window (assq 'inhibit-same-window alist)))
    (and rvb/results-modes
         (not (and asked-for-same-window (null (cdr asked-for-same-window))))
         (with-current-buffer (window-buffer (selected-window))
           (and (derived-mode-p rvb/results-modes) t)))))

;; Appended, so the full-frame entry above still wins where both match.
(add-to-list 'display-buffer-alist
             '(rvb/displaying-from-results-p
               (display-buffer-reuse-window display-buffer-use-some-window)
               (inhibit-same-window . t)
               ;; Most-recently-used, so it keeps landing in the window
               ;; the last result opened rather than rotating.
               (some-window . mru))
             t)

(provide 'rvb-windows)
;;; rvb-windows.el ends here
