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

(add-hook 'org-mode-hook 'mixed-pitch-mode)


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


(provide 'rvb-org)
