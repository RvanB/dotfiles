(require 'exwm)
(require 'exwm-systemtray)

(exwm-systemtray-mode)

;; Set the initial workspace number
(setq exwm-workspace-number 4)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
	  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

;; Global keybindings
(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode)
	([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace
	([?\s-&] . (lambda (cmd) ;; s-&: Launch application
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command cmd nil cmd)))
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

(provide 'rvb-exwm)
source
