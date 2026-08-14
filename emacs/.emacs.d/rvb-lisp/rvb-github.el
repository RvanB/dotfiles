;;; rvb-github.el --- Render GitHub issue links in Org  -*- lexical-binding: t; -*-

;; Paste a GitHub issue or pull-request URL into an Org file and it is
;; displayed as a short reference with the issue's title and state:
;;
;;   in the file   https://github.com/cdlib/zephir-reports/issues/42
;;   on screen     cdlib/zephir-reports#42  Retry failed sends  * open
;;
;; The file keeps the real URL.  Nothing here is a custom link syntax,
;; so the text stays meaningful to GitHub, to other Org readers, and to
;; anyone reading it as plain text -- only the display changes.
;;
;; Titles come from the `gh' CLI, so private repositories work with no
;; extra configuration.  Lookups are asynchronous and cached for the
;; session: a reference renders immediately as `owner/repo#42' and
;; gains its title when the answer arrives.

(require 'ol)
(require 'subr-x)

(defgroup rvb/github nil
  "GitHub references in Org buffers."
  :group 'org
  :prefix "rvb/github-")

(defcustom rvb/github-fetch-titles t
  "Whether to look up issue titles and state with the `gh' CLI."
  :type 'boolean
  :group 'rvb/github)

(defcustom rvb/github-executable "gh"
  "The GitHub CLI executable."
  :type 'string
  :group 'rvb/github)

(defface rvb/github-ref '((t :inherit org-link))
  "Face for the owner/repo#number part of a reference."
  :group 'rvb/github)

(defface rvb/github-title '((t :inherit font-lock-doc-face))
  "Face for a fetched issue title."
  :group 'rvb/github)

(defface rvb/github-open '((t :inherit success))
  "Face for an open issue or pull request."
  :group 'rvb/github)

(defface rvb/github-closed '((t :inherit error))
  "Face for a closed issue or pull request."
  :group 'rvb/github)

(defface rvb/github-merged '((t :inherit magit-branch-remote))
  "Face for a merged pull request."
  :group 'rvb/github)

(defconst rvb/github--url-regexp
  (rx "//github.com/"
      (group (+ (not (any "/")))) "/"
      (group (+ (not (any "/")))) "/"
      (or "issues" "pull") "/"
      (group (+ digit)))
  "Match the path of a GitHub issue or pull-request URL.
Org hands `:activate-func' the path, which for an https link begins
with the double slash rather than the scheme.")

(defvar rvb/github--cache (make-hash-table :test #'equal)
  "Cache of \"owner/repo#number\" to what GitHub said about it.
A value is `pending' while a lookup is in flight, `unknown' if the
lookup failed, or a plist with :title and :state.")


;;; Rendering

(defun rvb/github-state-string (state)
  "Return a display string for STATE, one of \"open\", \"closed\", \"merged\"."
  (pcase state
    ("open" (propertize "● open" 'face 'rvb/github-open))
    ("merged" (propertize "✔ merged" 'face 'rvb/github-merged))
    ("closed" (propertize "✔ closed" 'face 'rvb/github-closed))
    (_ "")))

(defun rvb/github--render (key)
  "Return the display string for KEY, using whatever is cached."
  (let ((info (gethash key rvb/github--cache))
        (ref (propertize key 'face 'rvb/github-ref)))
    (if (not (consp info))
        ;; Pending, failed, or not looked up: the reference alone is
        ;; still more readable than the URL.
        ref
      (string-join
       (delq nil
             (list ref
                   (when-let* ((title (plist-get info :title)))
                     (propertize title 'face 'rvb/github-title))
                   (let ((s (rvb/github-state-string (plist-get info :state))))
                     (unless (string-empty-p s) s))))
       "  "))))


;;; Looking up

(defun rvb/github-lookup (key &optional refresh)
  "Ensure KEY is looked up and return what is known about it.

Returns a plist with :title and :state, or nil while the answer is
still unknown.  Callers that want to compose their own display -- a
bare title rather than the full reference, say -- use this; REFRESH is
called when a pending lookup lands."
  (when (string-match (rx bos (group (+ (not (any "/")))) "/"
                          (group (+ (not (any "#")))) "#" (group (+ digit)) eos)
                      key)
    (rvb/github--fetch key
                       (concat (match-string 1 key) "/" (match-string 2 key))
                       (match-string 3 key)
                       refresh))
  (let ((info (gethash key rvb/github--cache)))
    (and (consp info) info)))

(defun rvb/github-url (key)
  "Return the github.com URL for KEY, of the form \"owner/repo#42\".
Always the issues path: GitHub redirects to the pull request when the
number is one."
  (when (string-match (rx bos (group (+ (not (any "/")))) "/"
                          (group (+ (not (any "#")))) "#" (group (+ digit)) eos)
                      key)
    (format "https://github.com/%s/%s/issues/%s"
            (match-string 1 key) (match-string 2 key) (match-string 3 key))))

(defun rvb/github--key-parts (key)
  "Split KEY, of the form \"owner/repo#42\", into (REPO . NUMBER)."
  (when (string-match (rx bos (group (+ (not (any "/")))) "/"
                          (group (+ (not (any "#")))) "#" (group (+ digit)) eos)
                      key)
    (cons (concat (match-string 1 key) "/" (match-string 2 key))
          (match-string 3 key))))

(defun rvb/github--run (args stdin-file callback what)
  "Run gh with ARGS, optionally sending STDIN-FILE, then call CALLBACK.
CALLBACK receives the trimmed output, or nil if the call failed.  WHAT
names the operation for error messages."
  (unless (executable-find rvb/github-executable)
    (user-error "%s is not installed" rvb/github-executable))
  (let ((out (generate-new-buffer " *rvb-github*")))
    (make-process
     :name "rvb-github"
     :buffer out
     :noquery t
     :connection-type 'pipe
     :command (cons rvb/github-executable args)
     :stderr out
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((text (with-current-buffer (process-buffer proc) (buffer-string)))
               (ok (zerop (process-exit-status proc))))
           (kill-buffer (process-buffer proc))
           (when stdin-file (ignore-errors (delete-file stdin-file)))
           (if ok
               (funcall callback text)
             (message "%s failed: %s" what (string-trim text))
             (funcall callback nil))))))))

(defun rvb/github-fetch-body (key callback)
  "Fetch the body of issue KEY, then call CALLBACK with it.
CALLBACK receives the body as a string, or nil on failure."
  (let ((parts (or (rvb/github--key-parts key)
                   (user-error "Not an issue reference: %s" key))))
    (rvb/github--run
     (list "api" (format "repos/%s/issues/%s" (car parts) (cdr parts))
           "--jq" ".body")
     nil callback (format "Fetching %s" key))))

(defun rvb/github-set-body (key body callback)
  "Set the body of issue KEY to BODY, then call CALLBACK.
CALLBACK receives non-nil on success.  The body is sent as JSON so
that any content survives verbatim."
  (let* ((parts (or (rvb/github--key-parts key)
                    (user-error "Not an issue reference: %s" key)))
         (file (make-temp-file "rvb-github" nil ".json")))
    (with-temp-file file
      (insert (json-encode `(("body" . ,body)))))
    (rvb/github--run
     (list "api" "--method" "PATCH"
           (format "repos/%s/issues/%s" (car parts) (cdr parts))
           "--input" file)
     file
     (lambda (result)
       ;; The answer is the updated issue; drop it from the cache so the
       ;; next render shows what GitHub now has.
       (when result (remhash key rvb/github--cache))
       (funcall callback result))
     (format "Updating %s" key))))

(defun rvb/github-reference (key &optional refresh)
  "Return the display string for KEY, of the form \"owner/repo#42\".

Looks the reference up in the background if it is not cached yet and
calls REFRESH, a function of no arguments, once the answer lands.  The
caller decides what refreshing means -- refontifying a buffer,
redrawing a report -- so this works outside Org too."
  (rvb/github-lookup key refresh)
  (rvb/github--render key))

(defun rvb/github--fetch (key repo number &optional refresh)
  "Look up NUMBER in REPO asynchronously, then call REFRESH."
  (when (and rvb/github-fetch-titles
             (executable-find rvb/github-executable)
             (not (gethash key rvb/github--cache)))
    (puthash key 'pending rvb/github--cache)
    (let ((out (generate-new-buffer " *rvb-github*")))
      (make-process
       :name "rvb-github"
       :buffer out
       :noquery t
       :connection-type 'pipe
       :command (list rvb/github-executable "api"
                      (format "repos/%s/issues/%s" repo number)
                      "--jq"
                      ;; One call covers issues and pull requests: the
                      ;; issues endpoint returns both, and only a pull
                      ;; request carries `pull_request'.
                      "[.title, .state, (.pull_request.merged_at // \"\")] | @tsv")
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((text (with-current-buffer (process-buffer proc) (buffer-string)))
                 (ok (zerop (process-exit-status proc))))
             (kill-buffer (process-buffer proc))
             (puthash key
                      (if (not ok)
                          'unknown
                        (pcase-let ((`(,title ,state ,merged)
                                     (split-string (string-trim text) "\t")))
                          (if (null title)
                              'unknown
                            (list :title title
                                  :state (if (and merged
                                                  (not (string-empty-p merged)))
                                             "merged"
                                           state)))))
                      rvb/github--cache)
             (when refresh (funcall refresh)))))))))


;;; Org integration

(defun rvb/github--described-p (start end)
  "Return non-nil if the link between START and END has a description."
  (save-excursion
    (goto-char start)
    (search-forward "][" end t)))

(defun rvb/github-activate (start end path bracketp)
  "Display a GitHub issue link between START and END compactly.
An `:activate-func' for Org's https links.  PATH is the link path and
BRACKETP is non-nil for a bracketed link."
  ;; Always clear first, so a link edited into something else does not
  ;; keep a stale rendering.  This is why `display' does not need to be
  ;; in `font-lock-extra-managed-props'.
  (with-silent-modifications
    (remove-text-properties start end '(display nil help-echo nil))
    (when (and (string-match rvb/github--url-regexp path)
               ;; A link the user gave a description already says what
               ;; they wanted it to say.
               (not (and bracketp (rvb/github--described-p start end))))
      (let* ((owner (match-string 1 path))
             (name (match-string 2 path))
             (number (match-string 3 path))
             (repo (concat owner "/" name))
             (key (format "%s#%s" repo number)))
        (put-text-property start end 'help-echo (concat "https:" path))
        (let ((buffer (current-buffer)))
          (put-text-property
           start end 'display
           (rvb/github-reference
            key (lambda ()
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer (font-lock-flush)))))))))))

(org-link-set-parameters "https" :activate-func #'rvb/github-activate)
(org-link-set-parameters "http" :activate-func #'rvb/github-activate)

(defun rvb/github-refresh ()
  "Forget every cached issue title and look them up again."
  (interactive)
  (clrhash rvb/github--cache)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (font-lock-flush))))
  (message "GitHub references refreshed"))

(provide 'rvb-github)
;;; rvb-github.el ends here
