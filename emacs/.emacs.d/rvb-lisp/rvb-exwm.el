(require 'exwm)
(require 'exwm-systemtray)

(exwm-systemtray-mode)

;; Set the initial workspace number
(setq exwm-workspace-number 4)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
	  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

(defun rvb/launch-app () ;; s-&: Launch application with Vertico completion
  (interactive)
  (let* ((cmd (completing-read "Shell command: "
                               (split-string (shell-command-to-string "whence -pm '*'") "\n" t)
                               nil t)))
    (start-process-shell-command cmd nil cmd)))

;; Global keybindings
(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode)
        ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace
        ([?\s-&] . rvb/launch-app) ;; Non-blocking start
        ;; s-N: Switch to certain workspace
        ,@(mapcar (lambda (i)
                   `(,(kbd (format "s-%d" i)) .
                     (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create ,i))))
                 (number-sequence 0 9))))

;; Enable EXWM
(add-hook 'after-init-hook 'exwm-wm-mode)

(require 'pulseaudio-control)

;; Select the first sink
(pulseaudio-control-select-sink-by-index (caar (pulseaudio-control--get-sinks)))

(exwm-input-set-key
  (kbd "<XF86AudioRaiseVolume>")
  (lambda ()
    (interactive)
    (pulseaudio-control-increase-sink-volume 10)))

(exwm-input-set-key
  (kbd "<XF86AudioLowerVolume>")
  (lambda ()
     (interactive)
     (pulseaudio-control-decrease-sink-volume 10)))

 (exwm-input-set-key
  (kbd "<XF86AudioMute>")
   (lambda ()
     (interactive)
     '(pulseaudio-control-toggle-current-sink-mute)))

;; Print screen
(global-set-key (kbd "<print>")
  (lambda ()
    (interactive)
    (let ((path (concat "~/Documents/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
      (start-process-shell-command
       "import" nil (concat "import -window root " path))
    (message (concat "Screenshot saved to " path)))
    ))

(provide 'rvb-exwm)
source
