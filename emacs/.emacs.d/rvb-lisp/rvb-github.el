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

(require 'json)
(require 'ol)
(require 'subr-x)

(defgroup rvb/github nil
  "GitHub references in Org buffers."
  :group 'org
  :prefix "rvb/github-")

(defcustom rvb/github-fetch-titles t
  "Whether to ask the `gh' CLI what GitHub knows, for display.
Issue and pull-request titles and state, and whether a branch has a
pull request at all.  Off, everything here renders from what is written
down locally."
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
  "Cache of what GitHub said, keyed by what was asked.

A key is either \"owner/repo#number\" for an issue or pull request, or
`rvb/github--pr-key' for \"the pull request for this branch\" -- two
questions, one session's worth of answers, so `rvb/github-refresh'
clears both.

A value is `pending' while a lookup is in flight, `unknown' if the
lookup failed, `none' if GitHub answered that there is nothing, or a
plist of what it said.")


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

(defun rvb/github-fetch-issue (key callback)
  "Fetch issue KEY, then call CALLBACK with what GitHub said.
CALLBACK receives a plist of :title, :body and :state, or nil on
failure.  One request rather than one per field, and the whole JSON
rather than a `--jq' expression per caller: a body runs to many lines
and would not survive being packed into a tab-separated row."
  (let ((parts (or (rvb/github--key-parts key)
                   (user-error "Not an issue reference: %s" key))))
    (rvb/github--run
     (list "api" (format "repos/%s/issues/%s" (car parts) (cdr parts)))
     nil
     (lambda (text)
       (funcall callback
                (when text
                  (condition-case nil
                      (let ((json (json-parse-string text
                                                     :object-type 'alist
                                                     :null-object nil)))
                        (list :title (alist-get 'title json)
                              :body (alist-get 'body json)
                              :state (alist-get 'state json)))
                    (error (message "Could not read GitHub's answer for %s" key)
                           nil)))))
     (format "Fetching %s" key))))

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

(defun rvb/github--result-url (text)
  "Return the last https URL on a line of its own in TEXT, or nil.
What `gh' prints on success is the new object's URL, after whatever it
had to say about getting there."
  (let (url)
    (dolist (line (split-string (or text "") "\n" t) url)
      (let ((line (string-trim line)))
        (when (string-prefix-p "https://" line)
          (setq url line))))))

(defun rvb/github-create-pr (dir title body base head callback)
  "Open a pull request in the repository at DIR, then call CALLBACK.

TITLE and BODY are the pull request's, BASE the branch it merges into
and HEAD the branch it merges from.  CALLBACK receives the new pull
request's URL, or nil if gh refused -- which is also how an existing
pull request for HEAD is reported, gh naming it in the error.

Which repository this is comes from DIR, the way gh resolves one
anywhere else.  The body goes through a file: it runs to many lines,
and `--body-file' is read verbatim where an argument would be at the
mercy of the shell."
  (let ((file (make-temp-file "rvb-github" nil ".md"))
        (default-directory (file-name-as-directory dir)))
    (with-temp-file file (insert (or body "")))
    (rvb/github--run
     (list "pr" "create" "--title" title "--body-file" file
           "--base" base "--head" head)
     file
     (lambda (text)
       ;; There is one now, whatever was cached about this branch.
       (when text (rvb/github-forget-pull-request dir head))
       (funcall callback (and text (rvb/github--result-url text))))
     (format "Opening a pull request for %s" head))))

(defun rvb/github--pr-key (dir branch)
  "Return the cache key for BRANCH's pull request in DIR's repository.
Keyed by worktree rather than by repository name: that is what the
caller has, and asking gh in DIR is how the repository gets decided
anyway."
  (concat (expand-file-name dir) "@" branch))

(defconst rvb/github--pr-query "
query($owner:String!,$repo:String!,$branch:String!){
  repository(owner:$owner,name:$repo){
    pullRequests(headRefName:$branch, first:1,
                 orderBy:{field:CREATED_AT,direction:DESC}){
      nodes{
        number url title state
        reviewThreads(first:100){ nodes{ isResolved } }
      }
    }
  }
}"
  "GraphQL asking for the pull request on a branch.

GraphQL rather than `gh pr list' because of the review threads: whether
a conversation is resolved is not in the REST pull request at all, and
this asks for the pull request and its threads in one round trip.

The newest pull request for the branch, since reopening the same branch
is how a rejected one is followed up.  A hundred threads is more than
any pull request worth reading has; past that the count is short.")

(defun rvb/github--parse-pr (text)
  "Return the pull request in GraphQL answer TEXT as a plist, or nil."
  (condition-case nil
      (let* ((json (json-parse-string text :object-type 'alist
                                      :null-object nil :false-object nil))
             (nodes (alist-get 'nodes
                               (alist-get 'pullRequests
                                          (alist-get 'repository
                                                     (alist-get 'data json))))))
        (when (> (length nodes) 0)
          (let* ((pr (aref nodes 0))
                 (threads (alist-get 'nodes (alist-get 'reviewThreads pr)))
                 (unresolved 0))
            ;; `isResolved' is false -- nil here -- for a conversation
            ;; still wanting an answer, outdated by later commits or not,
            ;; which is what GitHub counts as unresolved too.
            (dotimes (i (length threads))
              (unless (alist-get 'isResolved (aref threads i))
                (setq unresolved (1+ unresolved))))
            (list :number (alist-get 'number pr)
                  :url (alist-get 'url pr)
                  :title (alist-get 'title pr)
                  :unresolved unresolved
                  ;; GitHub answers OPEN, CLOSED, MERGED; everything
                  ;; here speaks the API's lower case.
                  :state (downcase (or (alist-get 'state pr) ""))))))
    (error nil)))

(defun rvb/github-pull-request-pending-p (dir branch)
  "Return non-nil while BRANCH's pull-request lookup is still in flight.
Nil and nil are otherwise the same answer -- see
`rvb/github-pull-request' -- and a caller about to say \"there is no
pull request\" had better be sure."
  (eq (gethash (rvb/github--pr-key dir branch) rvb/github--cache) 'pending))

(defun rvb/github-forget-pull-request (dir branch)
  "Forget what was cached about BRANCH's pull request in DIR."
  (remhash (rvb/github--pr-key dir branch) rvb/github--cache))

(defun rvb/github-pull-request (dir branch &optional refresh)
  "Return the pull request for BRANCH in DIR's repository, or nil.

A plist of :number, :url, :title, :state and :unresolved -- how many
review conversations are still waiting on somebody -- once GitHub has
answered.
Nil covers both \"there is none\" and \"nobody has asked yet\", because
a caller does the same thing with either: offer to open one, and draw
again if that turns out to be wrong.  REFRESH is called when a pending
lookup lands.

Asked once per session and cached, like every other lookup here -- this
is called on every redraw of a status buffer.  What changes the answer
is usually opening a pull request, which `rvb/github-create-pr' forgets
the old answer for; `rvb/github-refresh' forgets the rest."
  (let* ((key (rvb/github--pr-key dir branch))
         (info (gethash key rvb/github--cache)))
    (when (and rvb/github-fetch-titles
               (executable-find rvb/github-executable)
               ;; A process cannot start in a directory that is not
               ;; there, and this is called from a redraw, where
               ;; signalling would cost the whole buffer.
               (file-directory-p dir)
               (null info))
      (puthash key 'pending rvb/github--cache)
      (let ((out (generate-new-buffer " *rvb-github*"))
            (default-directory (file-name-as-directory dir)))
        (make-process
         :name "rvb-github-pr"
         :buffer out
         :noquery t
         :connection-type 'pipe
         ;; `{owner}' and `{repo}' are gh's placeholders for the
         ;; repository it is run in, so DIR decides which one this is
         ;; here as much as anywhere else.  The branch is passed with
         ;; `-f' rather than `-F': one named for a ticket number would
         ;; otherwise be read as a number.
         :command (list rvb/github-executable "api" "graphql"
                        "-f" (concat "query=" rvb/github--pr-query)
                        "-F" "owner={owner}" "-F" "repo={repo}"
                        "-f" (concat "branch=" branch))
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((text (with-current-buffer (process-buffer proc) (buffer-string)))
                   (ok (zerop (process-exit-status proc))))
               (kill-buffer (process-buffer proc))
               (puthash key
                        (if ok (or (rvb/github--parse-pr text) 'none) 'unknown)
                        rvb/github--cache)
               (when refresh (funcall refresh))))))))
    (and (consp info) info)))

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
