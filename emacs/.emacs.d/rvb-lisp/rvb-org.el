;;; Org mode
;; Everything below sets Org's own variables and calls its functions, so
;; load it first rather than relying on `org-mouse' further down to.
(require 'org)

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
;; (add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'mixed-pitch-mode)

;; Keep Org's display entirely ASCII: no modern bullets, pretty entities, or
;; Unicode folding marker.
(setq org-auto-align-tags nil
      org-tags-column 0
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t
      org-hide-emphasis-markers t
      org-pretty-entities nil
      org-agenda-tags-column 0
      org-ellipsis nil)

(defun rvb/org-hide-fold-ellipsis ()
  "Remove the display text appended to every folded Org region.
Recent Org versions interpret an empty `org-ellipsis' string as a
request for the default three dots, so set the initialized folding
specifications to no ellipsis explicitly."
  (dolist (spec org-fold-core--specs)
    (when (assq :ellipsis (cdr spec))
      (org-fold-core-set-folding-spec-property
       (car spec) :ellipsis nil t))))

(add-hook 'org-mode-hook #'rvb/org-hide-fold-ellipsis)

(use-package org-tidy
  :ensure t
  :hook
  (org-mode . org-tidy-mode))

(use-package org-autolist
  :ensure t
  :hook (org-mode . org-autolist-mode))

(setq org-hide-emphasis-markers t)

;;; Mouse support.  `org-mouse' normally gives headline stars a
;;; `mouse-face' highlight.  With `org-indent-mode' that highlight
;;; reveals the raw leading stars, and the same artifact shows beside an
;;; Org Modern folding arrow.  Keep the click behavior but use a direct
;;; mouse binding with no hover face.
(require 'org-mouse)
(setq org-mouse-features (remove 'activate-stars org-mouse-features))

(defun rvb/org-mouse-cycle-heading (event)
  "Cycle the Org heading clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (org-cycle))

(defvar rvb/org-heading-mouse-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mouse-map)
    (define-key map [mouse-1] #'rvb/org-mouse-cycle-heading)
    map)
  "Mouse map for clickable Org heading markers without a hover face.")

(defun rvb/org-activate-heading-mouse-map ()
  "Make Org heading markers clickable without highlighting them."
  (font-lock-add-keywords
   nil
   `((,org-outline-regexp
      0 `(face org-link keymap ,rvb/org-heading-mouse-map) 'prepend))
   t))

(add-hook 'org-mode-hook #'rvb/org-activate-heading-mouse-map)

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-todo nil
        org-modern-block-fringe t
        org-modern-table-vertical 0.5
        org-modern-table-horizontal 0.5))


(provide 'rvb-org)
