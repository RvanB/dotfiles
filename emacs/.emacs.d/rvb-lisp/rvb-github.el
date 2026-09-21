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

(require 'cl-lib)
(require 'json)
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

A key is \"owner/repo#number\" for an issue or pull request,
`rvb/github--due-key' for \"which iteration is that issue in\", or
`rvb/github--pr-key' for \"the pull request for this branch\" -- three
questions, one cache of answers, so `rvb/github-refresh' clears them
all.

A value is `pending' while the first lookup is in flight, `unknown' if
the lookup failed, `none' if GitHub answered that there is nothing, or
a plist of what it said.")

(defvar rvb/github--cache-time (make-hash-table :test #'equal)
  "When each `rvb/github--cache' entry was answered, in `float-time'.")

(defvar rvb/github--pending (make-hash-table :test #'equal)
  "Keys whose lookup is in flight.

Separate from the cache because a re-ask keeps displaying the old
answer while the new one is on its way: the value has to stay put, so
`pending' cannot be it.")

(defcustom rvb/github-cache-ttl 300
  "Seconds an answer from GitHub is trusted before it is asked for again.

The cache is what makes a redraw free -- a status buffer looks every
reference up each time it is drawn -- but nothing here is told when
GitHub changes.  Without an expiry, an issue closed while Emacs is
running goes on reading as open until `rvb/github-refresh'.

A stale entry is displayed as it is while the new answer is fetched,
and a failed re-ask keeps the answer it had, so neither costs the
buffer the title it was already showing.

nil never expires anything."
  :type '(choice (const :tag "Never expire" nil) integer)
  :group 'rvb/github)

(defun rvb/github--ask-p (key)
  "Return non-nil if KEY should be looked up now.
Either nothing is known about it, or what is known has passed
`rvb/github-cache-ttl'.  A lookup already in flight is never asked
again."
  (and (not (gethash key rvb/github--pending))
       (let ((info (gethash key rvb/github--cache)))
         (or (null info)
             (and rvb/github-cache-ttl
                  (> (- (float-time)
                        (or (gethash key rvb/github--cache-time) 0))
                     rvb/github-cache-ttl))))))

(defun rvb/github--begin (key)
  "Note that a lookup of KEY has started."
  (puthash key t rvb/github--pending)
  (unless (gethash key rvb/github--cache)
    (puthash key 'pending rvb/github--cache)))

(defun rvb/github--finish (key value)
  "Record VALUE as the answer for KEY, and that its lookup has landed.

A failed re-ask keeps whatever GitHub last said rather than replacing
it with `unknown': the network is a worse witness than a five-minute
old answer, and the timestamp is stamped either way so the failure is
not retried on the next redraw."
  (remhash key rvb/github--pending)
  (let ((old (gethash key rvb/github--cache)))
    (unless (and (eq value 'unknown) (consp old))
      (puthash key value rvb/github--cache)))
  (puthash key (float-time) rvb/github--cache-time))

(defun rvb/github-forget (key)
  "Forget what was cached about KEY."
  (remhash key rvb/github--cache)
  (remhash key rvb/github--cache-time))

(defun rvb/github-expire-issue (key)
  "Treat what is known about issue KEY as stale, without forgetting it.
The next lookup asks GitHub again while the old answer is still shown,
where forgetting it would leave nothing to show in the meantime.  Its
due date is expired along with it."
  (dolist (k (list key (rvb/github--due-key key)))
    (when (gethash k rvb/github--cache)
      (puthash k 0 rvb/github--cache-time))))


;;; Rendering

(defun rvb/github-state-string (state)
  "Return a display string for STATE, one of \"open\", \"closed\", \"merged\".

The word alone, with no marker in front of it.  A marker earns its
place where the text has no colour to spare, and here it has: this is
rendered into body text, where the face carries the meaning and a glyph
in front of it only competes with the word."
  (pcase state
    ("open" (propertize "open" 'face 'rvb/github-open))
    ("merged" (propertize "merged" 'face 'rvb/github-merged))
    ("closed" (propertize "closed" 'face 'rvb/github-closed))
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

(defun rvb/github-assigned-issues (callback &optional limit)
  "Fetch the open issues assigned to you, then call CALLBACK with them.

Across every repository you can see, as `gh search' finds them -- up to
LIMIT, 100 by default.  CALLBACK receives a list of plists with :key
\(\"owner/repo#42\"), :repo, :number, :title, :body and :url, or nil
on failure.  Pull requests are left out; they are what a feature makes,
not what it is for."
  (rvb/github--run
   (list "search" "issues" "--assignee=@me" "--state=open"
         "--limit" (number-to-string (or limit 100))
         "--json" "repository,number,title,body,url")
   nil
   (lambda (text)
     (funcall
      callback
      (when text
        (condition-case nil
            (mapcar (lambda (issue)
                      (let ((repo (alist-get 'nameWithOwner
                                             (alist-get 'repository issue)))
                            (number (alist-get 'number issue)))
                        (list :key (format "%s#%s" repo number)
                              :repo repo
                              :number number
                              :title (alist-get 'title issue)
                              :body (alist-get 'body issue)
                              :url (alist-get 'url issue))))
                    (json-parse-string text :object-type 'alist
                                       :array-type 'list :null-object nil))
          (error (message "Could not read GitHub's list of your issues")
                 nil)))))
   "Finding your assigned issues"))

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
       (when result (rvb/github-forget key))
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

(defun rvb/github-fetch-pull-request (dir branch callback)
  "Ask GitHub now about BRANCH's pull request in DIR's repository.

Unlike `rvb/github-pull-request', never answered from the cache: for a
caller about to act on the answer rather than display it.  CALLBACK
receives the same plist -- :number, :url, :title, :state -- or nil when
there is none or the lookup failed.  The cache is updated as well."
  (if (not (file-directory-p dir))
      (funcall callback nil)
    (let ((default-directory (file-name-as-directory dir))
          (key (rvb/github--pr-key dir branch)))
      (rvb/github--run
       (list "api" "graphql"
             "-f" (concat "query=" rvb/github--pr-query)
             "-F" "owner={owner}" "-F" "repo={repo}"
             "-f" (concat "branch=" branch))
       nil
       (lambda (text)
         (let ((pr (and text (rvb/github--parse-pr text))))
           (when text (rvb/github--finish key (or pr 'none)))
           (funcall callback pr)))
       (format "Finding the pull request for %s" branch)))))

(defun rvb/github-pull-request-expired-p (dir branch)
  "Return non-nil if what is known about BRANCH's pull request is stale.

The pull-request counterpart of `rvb/github-issue-expired-p': an answer
in hand that has passed `rvb/github-cache-ttl'.  That includes GitHub
having said there is none, since somebody opening one elsewhere is one
of the changes worth noticing.  Never true of a branch nobody has asked
about yet, nor of one whose lookup is in flight."
  (let ((key (rvb/github--pr-key dir branch)))
    (and (gethash key rvb/github--cache)
         (rvb/github--ask-p key))))

(defun rvb/github-forget-pull-request (dir branch)
  "Forget what was cached about BRANCH's pull request in DIR."
  (rvb/github-forget (rvb/github--pr-key dir branch)))

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
               (rvb/github--ask-p key))
      (rvb/github--begin key)
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
               (rvb/github--finish
                key (if ok (or (rvb/github--parse-pr text) 'none) 'unknown))
               (when refresh (funcall refresh))))))))
    (and (consp info) info)))

;;; When an issue is due
;;
;; GitHub has no due date on an issue.  What a team actually plans
;; against is the iteration field on a project board, and an iteration
;; is a start date and a length -- so "due" here means the last day of
;; the iteration the issue was put in.
;;
;; This is a second request rather than more fields on the one that
;; fetches the title, because reading a project board needs the
;; `read:project' scope and the title does not: folded together, a
;; token without that scope would lose the titles too.  Apart it
;; degrades to no due dates at all, which is what a team not using
;; iterations should see anyway.

(defconst rvb/github--iteration-query "
query($owner:String!,$repo:String!,$number:Int!){
  repository(owner:$owner,name:$repo){
    issue(number:$number){
      projectItems(first:10){
        nodes{
          fieldValues(first:50){
            nodes{
              ... on ProjectV2ItemFieldIterationValue{
                title startDate duration
              }
            }
          }
        }
      }
    }
  }
}"
  "GraphQL asking which iterations an issue is scheduled in.

Every field value of the issue's project items, of which only the
iteration ones answer anything: the inline fragment leaves the rest as
empty objects, which is cheaper than asking what type each one is.")

(defun rvb/github--due-key (key)
  "Return the cache key for the iteration lookup of issue KEY."
  (concat key "@iteration"))

(defun rvb/github--iteration-end (start duration)
  "Return the last day of an iteration as a \"YYYY-MM-DD\" string.
START is its first day and DURATION its length in days -- the two
things GitHub records about an iteration -- so the day it ends on is
the last one it covers, not the one after."
  (when (and (stringp start) (numberp duration) (> duration 0))
    (pcase-let ((`(,_ ,_ ,_ ,day ,month ,year . ,_) (parse-time-string start)))
      (when (and day month year)
        ;; Midday, so that a day's arithmetic cannot be undone by an
        ;; hour of daylight saving.
        (format-time-string "%Y-%m-%d"
                            (encode-time 0 0 12 (+ day duration -1) month year))))))

(defun rvb/github--parse-iteration (text)
  "Return the iteration in GraphQL answer TEXT as a plist, or nil.

The one ending soonest, when an issue is on more than one board: what
a due date is for is knowing when the work is wanted, and the earliest
claim on it is the one that decides that."
  (condition-case nil
      (let* ((json (json-parse-string text :object-type 'alist
                                      :null-object nil :false-object nil))
             (items (alist-get 'nodes
                               (alist-get 'projectItems
                                          (alist-get 'issue
                                                     (alist-get 'repository
                                                                (alist-get 'data json))))))
             best)
        (dotimes (i (length items))
          (let ((values (alist-get 'nodes (alist-get 'fieldValues (aref items i)))))
            (dotimes (j (length values))
              (let* ((value (aref values j))
                     (end (rvb/github--iteration-end
                           (alist-get 'startDate value)
                           (alist-get 'duration value))))
                (when (and end (or (null best) (string< end (plist-get best :due))))
                  (setq best (list :iteration (alist-get 'title value)
                                   :due end)))))))
        best)
    (error nil)))

(defcustom rvb/github-fetch-iterations t
  "Whether to ask which iteration an issue is scheduled in.

The one lookup here that needs more than the `repo' scope: reading a
project board wants `read:project' as well.  Set this to nil for a
team that does not plan in iterations, or to decline the extra scope
-- everything else goes on working, and due dates are simply not
shown."
  :type 'boolean
  :group 'rvb/github)

(defvar rvb/github--scope-warned nil
  "Whether the missing `read:project' scope has been reported already.")

(defun rvb/github--note-missing-scope (text)
  "Say how to grant `read:project' if that is what TEXT complains about.

A warning rather than a message, and once per session: the lookup
fails silently by design, so without something that stays on screen
the only symptom is a due date that never appears and no way to find
out why."
  (when (and (not rvb/github--scope-warned)
             (string-match-p "read:project" (or text "")))
    (setq rvb/github--scope-warned t)
    (display-warning
     'rvb/github
     (format "Due dates are off: the %s token cannot read project boards.

Grant it the scope:

    %s auth refresh -s read:project

Or set `rvb/github-fetch-iterations' to nil to stop asking.  Nothing
else here needs that scope; titles and states are unaffected."
             rvb/github-executable rvb/github-executable)
     :warning)))

(defun rvb/github-issue-due (key &optional refresh)
  "Return when issue KEY is due, from the iteration it is scheduled in.

A plist of :iteration, the iteration's name, and :due, the last day it
covers as a \"YYYY-MM-DD\" string.  Nil covers both \"it is in no
iteration\" and \"nobody has asked yet\": a caller shows a due date or
does not, and draws again if that turns out to be wrong.  REFRESH is
called when a pending lookup lands.

Cached and expired like every other lookup here -- see
`rvb/github-cache-ttl' -- so moving an issue to the next sprint shows
up without restarting Emacs."
  (let* ((parts (rvb/github--key-parts key))
         (cache-key (and parts (rvb/github--due-key key)))
         (info (and cache-key (gethash cache-key rvb/github--cache))))
    (when (and parts
               rvb/github-fetch-titles
               rvb/github-fetch-iterations
               (executable-find rvb/github-executable)
               (rvb/github--ask-p cache-key))
      (rvb/github--begin cache-key)
      (let ((out (generate-new-buffer " *rvb-github*"))
            (owner-repo (split-string (car parts) "/")))
        (make-process
         :name "rvb-github-iteration"
         :buffer out
         :noquery t
         :connection-type 'pipe
         :stderr out
         :command (list rvb/github-executable "api" "graphql"
                        "-f" (concat "query=" rvb/github--iteration-query)
                        "-f" (concat "owner=" (car owner-repo))
                        "-f" (concat "repo=" (cadr owner-repo))
                        "-F" (concat "number=" (cdr parts)))
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((text (with-current-buffer (process-buffer proc) (buffer-string)))
                   (ok (zerop (process-exit-status proc))))
               (kill-buffer (process-buffer proc))
               (unless ok (rvb/github--note-missing-scope text))
               (rvb/github--finish
                cache-key
                (if ok (or (rvb/github--parse-iteration text) 'none) 'unknown))
               (when refresh (funcall refresh))))))))
    (and (consp info) info)))

(defun rvb/github-issue-expired-p (key)
  "Return non-nil if what is known about issue KEY has gone stale.

An answer already in hand that has passed `rvb/github-cache-ttl' --
its title and state, or the iteration it is due in.  For a caller that
redraws on a timer and wants asking GitHub again to be part of that:
nothing on disk changes when an issue is closed in a browser, so
without this a list of issues never notices.

Never true of a reference nobody has looked up yet, which the redraw
itself will fetch, nor of one whose lookup is in flight."
  (cl-some (lambda (k)
             (and (consp (gethash k rvb/github--cache))
                  (rvb/github--ask-p k)))
           (list key (rvb/github--due-key key))))

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
             (rvb/github--ask-p key))
    (rvb/github--begin key)
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
             (rvb/github--finish
              key
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
                                   state))))))
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

(declare-function org-link-set-parameters "ol" (type &rest parameters))

;; Registered when Org's link library loads, not before: requiring it
;; here would load much of Org at startup.
(with-eval-after-load 'ol
  (org-link-set-parameters "https" :activate-func #'rvb/github-activate)
  (org-link-set-parameters "http" :activate-func #'rvb/github-activate))

(defun rvb/github-refresh ()
  "Forget every cached issue title and look them up again."
  (interactive)
  (clrhash rvb/github--cache)
  (clrhash rvb/github--cache-time)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (font-lock-flush))))
  (message "GitHub references refreshed"))

(provide 'rvb-github)
;;; rvb-github.el ends here
