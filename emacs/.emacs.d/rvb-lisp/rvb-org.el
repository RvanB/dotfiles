;;; Org mode
(setq org-directory "~/orgfiles/")

(custom-set-faces
 ;; Emacs 27 and later
 '(org-block ((t (:inherit fixed-pitch :background unspecified))))
 ;; For older Emacs versions
 '(org-block-background ((t (:background nil))))
 )

(custom-set-faces
 '(org-block-begin-line ((t (:background nil))))
 '(org-block-end-line ((t (:background nil))))
 )

;;; Set agenda files to the org directory
(setq org-agenda-files (list org-directory))

;;; Auto-add CLOSED timestamp when marking items DONE
(setq org-log-done 'time)

;;; Auto-delete completed items older than a month
(defun rvb/cleanup-old-done-items ()
  "Remove DONE items with CLOSED timestamp older than 30 days."
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((now (time-to-seconds))
          (cutoff-days 30))
      (org-map-entries
       (lambda ()
         (when (and (member (org-get-todo-state) org-done-keywords)
                    (org-entry-get nil "CLOSED"))
           (let* ((closed-str (org-entry-get nil "CLOSED"))
                  (closed-time (org-time-string-to-seconds closed-str))
                  (days-old (/ (- now closed-time) 86400)))
             (when (> days-old cutoff-days)
               (org-cut-subtree)))))
       nil 'file))))

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook 'rvb/cleanup-old-done-items nil t)))


(use-package org-modern
  :ensure t
  :init
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "…")

  :config
  (global-org-modern-mode))

;;; Website publishing
(let ((publish-config "~/RvanB.github.io/org-publish-config.el")
      (template-config "~/RvanB.github.io/post-template.el")
      (chapbook-config "~/RvanB.github.io/chapbook-export.el"))
  (when (file-exists-p publish-config)
    (load-file publish-config))
  (when (file-exists-p template-config)
    (load-file template-config))
  (when (file-exists-p chapbook-config)
    (load-file chapbook-config)))


(provide 'rvb-org)
