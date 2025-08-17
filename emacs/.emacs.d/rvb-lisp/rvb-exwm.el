(require 'exwm)
(require 'exwm-systemtray)

(require 'exwm-randr)



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
(exwm-input-set-key (kbd "<print>")
  (lambda ()
    (interactive)
    (let ((path (concat "~/Documents/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
      (start-process-shell-command
       "import" nil (concat "import -window root " path))
    (message (concat "Screenshot saved to " path)))
    ))

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil
             (if (string-match " connected" (shell-command-to-string "xrandr | grep '^DP-1'"))
                 "xrandr --output DP-1 --primary --auto --output eDP-1 --off"
               "xrandr --output eDP-1 --primary --auto --output DP-1 --off"))))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output DP-1 --left-of eDP-1 --auto")))

(exwm-randr-mode)
(exwm-wm-mode)
(exwm-systemtray-mode)
(display-battery-mode)

(provide 'rvb-exwm)
