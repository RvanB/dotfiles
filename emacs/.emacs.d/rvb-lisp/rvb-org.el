;;; Org mode
(setq org-directory
      (file-name-as-directory
       (expand-file-name "orgfiles" user-emacs-directory)))
(make-directory org-directory t)

(custom-set-faces
 ;; Emacs 27 and later
 '(org-block ((t (:inherit nil :background unspecified))))
 ;; For older Emacs versions
 '(org-block-background ((t (:background nil))))
 )

(custom-set-faces
 '(org-block-begin-line ((t (:background nil))))
 '(org-block-end-line ((t (:background nil))))
 )

;;; Set agenda files to the org directory
(setq org-agenda-files (list org-directory))
(setq org-hide-drawer-startup t)
(setq org-use-sub-superscripts '{})
(add-hook 'org-cycle-hook #'org-cycle-hide-drawers)

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

(add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook 'rvb/cleanup-old-done-items nil t)))
(add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'org-mode-hook 'visual-line-mode)


;; Keep Org's display entirely ASCII: no modern bullets, pretty entities, or
;; Unicode folding marker.
(setq org-auto-align-tags nil
      org-tags-column 0
      org-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t
      org-hide-emphasis-markers t
      org-pretty-entities nil
      org-agenda-tags-column 0
      org-ellipsis "...")

(use-package org-tidy
  :ensure t
  :hook
  (org-mode . org-tidy-mode))

(use-package org-autolist
  :ensure t
  :hook (org-mode . org-autolist-mode))

(setq org-hide-emphasis-markers t)


(provide 'rvb-org)
