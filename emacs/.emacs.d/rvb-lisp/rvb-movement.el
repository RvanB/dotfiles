
;;; Window management
;;; Enable keybindings for window switching
(windmove-default-keybindings)

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

;;; Killing a window closes the buffer
(global-set-key (kbd "s-k") 'rvb/kill-buffer-and-close-window)

;;; Window traversal
(defun rvb/other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))


;;; In-Buffer Movement / Navigation

;; God mode
(use-package god-mode
  :ensure t
  :init
  
  (setq god-mode-enable-function-key-translation nil)
  :config
  (add-to-list 'god-exempt-major-modes 'eat-mode)
  (god-mode)
  
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
  
  (global-set-key (kbd "C-g") #'rvb/keyboard-quit-and-god-mode)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-window-below)
  (global-set-key (kbd "C-x C-3") #'split-window-right)
  (global-set-key (kbd "C-x C-0") #'delete-window)
  (global-set-key (kbd "C-x C-o") #'other-window)

  (global-set-key (kbd "C-c C-s i") #'surround-insert)
  (global-set-key (kbd "C-c C-s d") #'surround-delete)
  (global-set-key (kbd "C-c C-s c") #'surround-change)
  
  (global-set-key (kbd "C-x C-g") #'magit))

;; forward-to-word / forward-word
(require 'misc)
(global-set-key (kbd "M-f") #'forward-to-word)
(global-set-key (kbd "M-F") #'forward-word)
(global-set-key (kbd "M-b") #'backward-to-word)
(global-set-key (kbd "M-B") #'backward-word)

(add-hook 'prog-mode-hook 'hl-line-mode)

(defun rvb/move-point-to-window-center ()
  "Move point to the line at the vertical center of the window."
  (interactive)
  (let* ((center-line (/ (window-body-height) 2))
         (target-pos (save-excursion
                       (move-to-window-line center-line)
                       (point))))
    (goto-char target-pos)))

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

(defconst golden-ratio 1.618 "Golden ratio constant.")

(defun kb/pixel-scroll-up (&optional arg)
  "(Nearly) drop-in replacement for `scroll-up' using golden ratio if ARG is nil."
  (cond
   ((eq this-command 'scroll-up-line)
    (funcall (ad-get-orig-definition 'scroll-up) (or arg 1)))
   (t
    (unless (eobp)
      (let* ((lines (or arg (truncate (/ (window-text-height) golden-ratio))))
             (pixels (* (line-pixel-height) lines)))
        (pixel-scroll-precision-interpolate (- pixels) nil 1)
        (rvb/move-point-to-window-center))))))

(defun kb/pixel-scroll-down (&optional arg)
  "(Nearly) drop-in replacement for `scroll-down' using golden ratio if ARG is nil."
  (cond
   ((eq this-command 'scroll-down-line)
    (funcall (ad-get-orig-definition 'scroll-down) (or arg 1)))
   (t
    (let* ((lines (or arg (truncate (/ (window-text-height) golden-ratio))))
           (pixels (* (line-pixel-height) lines)))
      (pixel-scroll-precision-interpolate pixels nil 1)
      (rvb/move-point-to-window-center)))))

(use-package ultra-scroll
  :pin "manual"
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
	    :rev :newest
	    :branch "main")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  (add-hook 'ultra-scroll-mode-hook
            (lambda ()
              (cond
               (pixel-scroll-precision-mode
                (advice-add 'scroll-up-command :override 'kb/pixel-scroll-up)
                (advice-add 'scroll-down-command :override 'kb/pixel-scroll-down)
                (advice-add 'recenter-top-bottom :override 'kb/pixel-recenter))
               (t
                (advice-remove 'scroll-up-command 'kb/pixel-scroll-up)
                (advice-remove 'scroll-down-command 'kb/pixel-scroll-down)
                (advice-remove 'recenter-top-bottom 'kb/pixel-recenter)))))
  :config
  (ultra-scroll-mode 1))


;;; Disable changing text scale with the mouse
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(defun rvb/back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(global-set-key [remap move-beginning-of-line] 'rvb/back-to-indentation-or-beginning)
(global-set-key [remap org-beginning-of-line] 'rvb/back-to-indentation-or-beginning)

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

(keymap-set isearch-mode-map "C-s" 'rvb/isearch-repeat-forward+)
(keymap-set isearch-mode-map "C-r" 'rvb/isearch-repeat-backward+)

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

(global-set-key (kbd "C-c C-l") #'rvb/isearch-visible-region)

(provide 'rvb-movement)
