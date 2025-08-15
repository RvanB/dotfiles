;;; Org mode
(setq org-directory "~/orgfiles/")

;;; Set agenda files to the org directory
(setq org-agenda-files (list org-directory))

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
