
;;; Window management

(defun rvb/kill-buffer-and-close-window ()
  "Kill the current buffer and close the window."
  (interactive)
  (let ((buffer (current-buffer)))
    (kill-buffer buffer)
    (when (one-window-p)
      (delete-window))
    (when (and (not (one-window-p))
               (not (window-live-p (get-buffer-window buffer))))
      (delete-window))))

;;; Window traversal
(defun rvb/other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun rvb/move-point-to-window-center ()
  "Move point to the line at the vertical center of the window."
  (interactive)
  (let* ((center-line (/ (window-body-height) 2))
         (target-pos (save-excursion
                       (move-to-window-line center-line)
                       (point))))
    (goto-char target-pos)))

;;; Buffer movement

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

;; https://www.reddit.com/r/emacs/comments/1g092xp/hack_use_pixelscroll_for_all_scrolling_and/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(defun kb/pixel-recenter (&optional arg redisplay)
  "Similar to `recenter' but with pixel scrolling.
ARG and REDISPLAY are identical to the original function."
  ;; See the links in line 6676 in window.c for
  (when-let* ((current-pixel (pixel-posn-y-at-point))
              (target-pixel (if (numberp arg)
                                (* (line-pixel-height) arg)
                              (* 0.5 (window-body-height nil t))))
              (distance-in-pixels 0)
              (pixel-scroll-precision-interpolation-total-time
               (/ pixel-scroll-precision-interpolation-total-time 2.0)))
    (setq target-pixel
          (if (<= 0 target-pixel)
              target-pixel
            (- (window-body-height nil t) (abs target-pixel))))
    (setq distance-in-pixels (- target-pixel current-pixel))
    (condition-case err
        (pixel-scroll-precision-interpolate distance-in-pixels nil 1)
      (error (message "[kb/pixel-recenter] %s" (error-message-string err))))
    (when redisplay (redisplay t))))

(defconst scroll-ratio 0.5 "Multiplier of the window height to scroll")

(defun kb/pixel-scroll-up (&optional arg)
  "(Nearly) drop-in replacement for `scroll-up' using golden ratio if ARG is nil."
  (cond
   ((eq this-command 'scroll-up-line)
    (funcall (ad-get-orig-definition 'scroll-up) (or arg 1)))
   (t
    (unless (eobp)
      (let* ((lines (or arg (truncate (* (window-text-height) scroll-ratio))))
             (pixels (* (line-pixel-height) lines)))
        (pixel-scroll-precision-interpolate (- pixels) nil 1)
        (rvb/move-point-to-window-center))))))

(defun kb/pixel-scroll-down (&optional arg)
  "(Nearly) drop-in replacement for `scroll-down' using golden ratio if ARG is nil."
  (cond
   ((eq this-command 'scroll-down-line)
    (funcall (ad-get-orig-definition 'scroll-down) (or arg 1)))
   (t
    (let* ((lines (or arg (truncate (* (window-text-height) scroll-ratio))))
           (pixels (* (line-pixel-height) lines)))
      (pixel-scroll-precision-interpolate pixels nil 1)
      (rvb/move-point-to-window-center)))))

(use-package ultra-scroll
  :pin "manual"
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
	    :rev :newest
	    :branch "main")
  :init
  (setq scroll-conservatively 1
        scroll-margin 0)
  (add-hook 'ultra-scroll-mode-hook
            (lambda ()
              (cond
               (pixel-scroll-precision-mode
                (advice-add 'scroll-up-command :override 'kb/pixel-scroll-up)
                (advice-add 'scroll-down-command :override 'kb/pixel-scroll-down)
                ;; (advice-add 'recenter-top-bottom :override 'kb/pixel-recenter)
                )
               (t
                (advice-remove 'scroll-up-command 'kb/pixel-scroll-up)
                (advice-remove 'scroll-down-command 'kb/pixel-scroll-down)
                ;; (advice-remove 'recenter-top-bottom 'kb/pixel-recenter)
                ))))
  :config
  (ultra-scroll-mode 1))

(defun rvb/back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun rvb/code-end-column ()
  "Return the column of the logical end-of-code on the current line.

If the line contains a comment, return the column just *after* the last
non-whitespace character that precedes the comment (so the point will be on
the following space if one exists, or immediately before the comment if not).
If there's no comment on the line, return the visual end-of-line column."
  (save-excursion
    (beginning-of-line)
    ;; find the buffer position of the comment start on this line, if any
    (let ((comment-pos
           (catch 'found
             (while (not (eolp))
               (let ((ppss (syntax-ppss)))
                 (when (nth 4 ppss)
                   (throw 'found (nth 8 ppss))))
               (forward-char 1))
             nil)))
      (if (not comment-pos)
          ;; no comment: return column at end of line
          (progn (end-of-line) (current-column))
        ;; there's a comment: compute the column just after the last non-space
        (save-excursion
          (goto-char comment-pos)                     ; land on the first comment char
          ;; Move back over any spaces/tabs between code and comment; then,
          ;; if possible, step back one more char to be on the last code char.
          (skip-chars-backward " \t" (line-beginning-position))
          (when (> (point) (line-beginning-position))
            (backward-char 1))
          ;; If we've backed all the way to column 0, return 0.
          ;; Otherwise return one column after the current column.
          (if (= (current-column) 0)
              0
            (1+ (current-column))))))))

(defun rvb/move-to-code-end ()
  "Toggle move: go to the logical end-of-code on the line, or, if already there,
go to the real end-of-line.  Useful as an alternative to `end-of-line`."
  (interactive)
  (let ((code-col (rvb/code-end-column)))
    (if (= (current-column) code-col)
        (end-of-line)
      (move-to-column code-col))))

;; Set visual line mode C-a and C-e to use rvb/back-to-indentation-or-beginning and rvb/move-to-code-end
(keymap-set visual-line-mode-map "C-a" 'rvb/back-to-indentation-or-beginning)
(keymap-set visual-line-mode-map "C-e" 'rvb/move-to-code-end)


;;; i-search changes
;; https://emacs.stackexchange.com/questions/53004/improving-isearch/53006#53006
(defun rvb/isearch-repeat-forward+ ()
  (interactive)
  (unless isearch-forward
    (when isearch-other-end
      (goto-char isearch-other-end)))
  (isearch-repeat-forward)
  (unless isearch-success
    (isearch-repeat-forward)))

(defun rvb/isearch-repeat-backward+ ()
  (interactive)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward)
  (unless isearch-success
    (isearch-repeat-backward)))

(advice-add 'keyboard-quit :before
            (lambda ()
              (isearch-done)))

(defvar-local rvb/isearch-narrowed nil)
(defvar-local rvb/isearch-wrapped nil)

(defun rvb/isearch-visible-region ()
  "Narrow buffer to visible region and start isearch. Auto-widens on exit."
  (interactive)
  (let ((start (window-start))
        (end (save-excursion
               (goto-char (window-end nil t))
               (point))))
    (narrow-to-region start end)
    (setq rvb/isearch-narrowed t)
    (setq rvb/isearch-wrapped nil)
    (add-hook 'isearch-update-post-hook #'rvb/auto-wrap-isearch nil t)
    (call-interactively #'isearch-forward)))

(defun rvb/widen-after-isearch ()
  "Widen the buffer if it was narrowed by rvb/isearch-visible-region."
  (when rvb/isearch-narrowed
    (setq rvb/isearch-narrowed nil)
    (setq rvb/isearch-wrapped nil)
    (remove-hook 'isearch-update-post-hook #'rvb/auto-wrap-isearch t)
    (widen)))

(defun rvb/auto-wrap-isearch ()
  "Automatically wrap Isearch when no match is found in narrowed region."
  (when (and isearch-forward
             isearch-string
             (not isearch-success)
             (not rvb/isearch-wrapped))  ;; prevent infinite looping
    (setq rvb/isearch-wrapped t)
    (goto-char (point-min))
    (isearch-repeat-forward)))

(add-hook 'isearch-mode-end-hook #'rvb/widen-after-isearch)

(defun backward-symbol (&optional n)
  (interactive)
  "Move point backward across N symbols (default 1).
This is the opposite of `forward-symbol`."
  (interactive "p")
  (forward-symbol (- (or n 1))))

(provide 'rvb-movement)
