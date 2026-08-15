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
(defgroup rvb-movement nil
  "RVB movement and scrolling preferences."
  :group 'convenience)

(defun rvb/toggle-god-mode ()
  "Toggle God Mode's command state in the current buffer."
  (interactive)
  (god-local-mode (if (bound-and-true-p god-local-mode) -1 1)))

(setq-default cursor-type 'box)

(defface rvb/god-minibuffer-indicator
  '((t (:inherit error)))
  "Face for the God Mode state indicator in minibuffer prompts."
  :group 'rvb-movement)

(defvar-local rvb/god-minibuffer-indicator-overlay nil
  "Overlay displaying the minibuffer's local God Mode state.")

(defun rvb/update-god-minibuffer-indicator ()
  "Update the GOD indicator for the current minibuffer's own state."
  (when (minibufferp)
    (unless (overlayp rvb/god-minibuffer-indicator-overlay)
      (setq rvb/god-minibuffer-indicator-overlay
            (make-overlay (point-min) (point-min) nil t t)))
    (overlay-put
     rvb/god-minibuffer-indicator-overlay 'before-string
     (if (bound-and-true-p god-local-mode)
         (concat (propertize "■" 'face 'rvb/god-minibuffer-indicator) " ")
       ""))))

(defun rvb/silence-god-mode-message-in-minibuffer (function &rest args)
  "Call FUNCTION with ARGS without mode messages in a minibuffer."
  (let ((inhibit-message (or inhibit-message (minibufferp))))
    (apply function args)))

(use-package god-mode
  :ensure t
  :init
  ;; Page chrome provides the state indicator, so suppress the package's
  ;; ordinary minor-mode lighter.
  (setq god-mode-lighter-string nil)
  :bind (("<escape>" . rvb/toggle-god-mode))
  :config
  (advice-add 'god-local-mode :around
              #'rvb/silence-god-mode-message-in-minibuffer)
  ;; Vim-like state transitions without adopting Vim's editing grammar.
  (keymap-set god-local-mode-map "<escape>" #'rvb/toggle-god-mode)
  (keymap-set god-local-mode-map "i" #'god-local-mode)
  (keymap-set god-local-mode-map "." #'repeat)
  (keymap-set god-local-mode-map "[" #'scroll-down-command)
  (keymap-set god-local-mode-map "]" #'scroll-up-command)
  (add-hook 'minibuffer-setup-hook #'rvb/update-god-minibuffer-indicator)
  (add-hook 'god-mode-enabled-hook #'rvb/update-god-minibuffer-indicator)
  (add-hook 'god-mode-disabled-hook #'rvb/update-god-minibuffer-indicator))

;; Deliberately not `(god-mode)', which is the global mode: that turns
;; command state on in every buffer it is not exempt from, including
;; every buffer opened from then on.  A buffer should start where you
;; would start typing in it, and command state should be somewhere you
;; went -- with `\\[rvb/toggle-god-mode]', per buffer, which is what
;; `god-local-mode' already is.

(defcustom rvb-scroll-distance 20
  "Number of lines moved by keyboard scrolling without a prefix argument.

This distance is shared by immediate and smooth scrolling so changing the
animation setting never changes where the command lands."
  :type 'natnum
  :group 'rvb-movement)

(defcustom rvb-smooth-scroll nil
  "When non-nil, animate keyboard scrolling over `rvb-scroll-distance' lines."
  :type 'boolean
  :group 'rvb-movement)

(defun rvb/scroll-lines (arg)
  "Return the requested scroll distance for prefix ARG."
  (if arg
      (prefix-numeric-value arg)
    rvb-scroll-distance))

(defun rvb/move-and-scroll (lines)
  "Move point by LINES visual lines, scrolling the window as needed.

When `rvb-smooth-scroll' is non-nil, animate toward the same destination
before placing point there."
  (let* ((origin (point))
         (target (save-excursion
                   (line-move lines t)
                   (copy-marker (point))))
         (direction (if (>= target origin) 1 -1))
         (moved-lines (count-screen-lines
                       (min origin target) (max origin target))))
    (when (and rvb-smooth-scroll (> moved-lines 0))
      (pixel-scroll-precision-interpolate
       (- (* direction (line-pixel-height) moved-lines)) nil 1))
    (goto-char target)
    (set-marker target nil)))

(defun rvb/scroll-up-command (&optional arg)
  "Move point forward by ARG lines or `rvb-scroll-distance'."
  (interactive "P")
  (rvb/move-and-scroll (rvb/scroll-lines arg)))

(defun rvb/scroll-down-command (&optional arg)
  "Move point backward by ARG lines or `rvb-scroll-distance'."
  (interactive "P")
  (rvb/move-and-scroll (- (rvb/scroll-lines arg))))

(advice-add 'scroll-up-command :override #'rvb/scroll-up-command)
(advice-add 'scroll-down-command :override #'rvb/scroll-down-command)

(use-package ultra-scroll
  :pin "manual"
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
	    :rev :newest
	    :branch "main")
  :init
  (setq scroll-conservatively 1
        scroll-margin 0)
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
          ;; Move back over any spaces/tabs between code and comment
          (skip-chars-backward " \t" (line-beginning-position))
          ;; Check if we're at the beginning of the line (comment-only line)
          (if (= (point) (line-beginning-position))
              ;; Comment-only line: return indentation column
              (progn
                (back-to-indentation)
                (current-column))
            ;; There's code before the comment: step back one more char to be on the last code char
            (backward-char 1)
            (1+ (current-column))))))))


(defun rvb/move-to-code-end ()
  "Toggle move: go to the logical end-of-code on the line, or, if already there,
go to the real end-of-line.  Useful as an alternative to `end-of-line`."
  (interactive)
  (let* ((code-col (rvb/code-end-column))
         (indent-col (save-excursion
                       (back-to-indentation)
                       (current-column)))
         (line-is-comment (= code-col indent-col)))
    (if line-is-comment
        (if (>= (current-column) indent-col)
            (end-of-line)
          (move-to-column indent-col))
      (if (>= (current-column) code-col)
          (end-of-line)
        (move-to-column code-col)))))

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

;; God mode


(provide 'rvb-movement)
