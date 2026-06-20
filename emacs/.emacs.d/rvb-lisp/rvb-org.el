;;; Org mode
(require 'cl-lib)

(defvar org-src-block-faces)

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

(defface rvb/org-markdown-src-block
  '((t (:inherit variable-pitch :background unspecified)))
  "Face used for prose inside Markdown Org source blocks."
  :group 'org)

(defun rvb/set-org-src-block-face (language face)
  "Use FACE for Org source blocks named LANGUAGE."
  (setq org-src-block-faces
        (cl-remove-if (lambda (entry)
                        (and (consp entry)
                             (string= language (car entry))))
                      org-src-block-faces))
  (push (list language face) org-src-block-faces))

(defun rvb/configure-org-markdown-src-blocks ()
  "Render Markdown source block prose as variable-pitch in Org buffers."
  (dolist (language '("markdown" "md" "gfm"))
    (rvb/set-org-src-block-face language 'rvb/org-markdown-src-block)))

(with-eval-after-load 'org-src
  (rvb/configure-org-markdown-src-blocks))

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

;;; Use curly typographic quotes when typing ' and " in org buffers
(setq electric-quote-context-sensitive t ; ' -> ‘ or ’ depending on context
      electric-quote-replace-double t)   ; " -> “ or ”
(add-hook 'org-mode-hook #'electric-quote-local-mode)

(defun rvb/org-electric-quote-open-after-markup (orig &rest args)
  "Treat Org emphasis markers as opening context for electric quotes.
`electric-quote' only opens a quote after whitespace or open-paren
syntax, so a quote typed right after a marker like / or * would close.
Give those markers open-paren syntax for the duration of the call so
typing e.g. /\" yields an opening quote."
  (if (derived-mode-p 'org-mode)
      (let ((table (make-syntax-table (syntax-table))))
        (dolist (ch '(?/ ?* ?_ ?= ?~ ?+))
          (modify-syntax-entry ch "(" table))
        (with-syntax-table table
          (apply orig args)))
    (apply orig args)))

(advice-add 'electric-quote-post-self-insert-function :around
            #'rvb/org-electric-quote-open-after-markup)

(add-hook 'org-mode-hook 'mixed-pitch-mode)
(add-hook 'org-mode-hook (lambda () (add-hook 'before-save-hook 'rvb/cleanup-old-done-items nil t)))
(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'org-mode-hook 'visual-line-mode)


;; (use-package org-modern
;;   :ensure t
;;   :init
;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-agenda-tags-column 0
;;    org-ellipsis "…")

;;   :config
;;   (global-org-modern-mode))

;; Fix font face for org tables
(set-face-attribute 'org-table nil
                    :inherit 'default
                    :background 'unspecified)

;;; Website publishing
;; (let ((publish-config "~/RvanB.github.io/org-publish-config.el")
;;       (template-config "~/RvanB.github.io/post-template.el")
;;       (chapbook-config "~/RvanB.github.io/chapbook-export.el"))
;;   (when (file-exists-p publish-config)
;;     (load-file publish-config))
;;   (when (file-exists-p template-config)
;;     (load-file template-config))
;;   (when (file-exists-p chapbook-config)
;;     (load-file chapbook-config)))

(use-package org-tidy
  :ensure t
  :hook
  (org-mode . org-tidy-mode))

(use-package org-autolist
  :ensure t
  :hook (org-mode . org-autolist-mode))

(setq org-hide-emphasis-markers t)


(provide 'rvb-org)
