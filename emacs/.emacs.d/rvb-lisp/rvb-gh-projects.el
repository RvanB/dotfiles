;;; rvb-gh-projects.el --- GitHub Projects todos via gh -*- lexical-binding: t; -*-

;;; Commentary:
;; Sync GitHub Projects v2 stories into normal Org agenda files.
;; GitHub stays the source of truth for story metadata; Org remains the
;; day-to-day agenda surface where local TODOs can live beside GitHub stories.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defun rvb-gh-project--split-list (value)
  "Return VALUE split on commas and whitespace."
  (when (and value (not (string-empty-p value)))
    (split-string value "[,[:space:]]+" t)))

(defun rvb-gh-project--parse-source-token (token)
  "Parse TOKEN as OWNER:PROJECT[,PROJECT]."
  (unless (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" token)
    (user-error "Project source must look like OWNER:PROJECT[,PROJECT]: %s"
                token))
  (let ((owner (string-trim (match-string 1 token)))
        (projects (rvb-gh-project--split-list (match-string 2 token))))
    (unless (and (not (string-empty-p owner)) projects)
      (user-error "Project source must include an owner and project numbers: %s"
                  token))
    (cons owner projects)))

(defun rvb-gh-project--parse-sources (value)
  "Parse VALUE as whitespace-separated OWNER:PROJECT[,PROJECT] entries."
  (when (and value (not (string-empty-p value)))
    (mapcar #'rvb-gh-project--parse-source-token
            (split-string value "[[:space:]]+" t))))

(defgroup rvb-gh-projects nil
  "Sync GitHub Project stories into Org agenda todos."
  :group 'tools
  :prefix "rvb-gh-project-")

(defcustom rvb-gh-project-gh-executable "gh"
  "GitHub CLI executable."
  :type 'string)

(defcustom rvb-gh-project-owner (getenv "GH_PROJECT_OWNER")
  "Default GitHub Project owner login.
Use an organization login, a user login, or @me."
  :type '(choice (const :tag "Prompt" nil) string))

(defcustom rvb-gh-project-numbers
  (rvb-gh-project--split-list (getenv "GH_PROJECTS"))
  "Default GitHub Project numbers."
  :type '(repeat string))

(defcustom rvb-gh-project-sources
  (rvb-gh-project--parse-sources (getenv "GH_PROJECT_SOURCES"))
  "Default GitHub Project owner/project-number combinations.
Each entry is (OWNER . PROJECTS), where PROJECTS is a list of project
numbers.  `GH_PROJECT_SOURCES' may use OWNER:PROJECT[,PROJECT] entries
separated by whitespace, for example: acme:12,14 @me:2."
  :type '(alist :key-type (string :tag "Owner")
                :value-type (repeat :tag "Project numbers" string)))

(defcustom rvb-gh-project-iteration-value
  (or (getenv "GH_PROJECT_ITERATION") "@current")
  "Default iteration filter value.
Use @current, @next, @previous, an iteration title, or an iteration id."
  :type '(choice (const :tag "All iterations" nil)
                 (const "@current")
                 (const "@next")
                 (const "@previous")
                 string))

(defcustom rvb-gh-project-status-field
  (or (getenv "GH_PROJECT_STATUS_FIELD") "Status")
  "Project single-select field stored on synced todos."
  :type 'string)

(defcustom rvb-gh-project-state
  (or (getenv "GH_PROJECT_STATE") "open")
  "Default issue state used when fetching stories.
Use nil or \"all\" to omit the state filter."
  :type '(choice (const :tag "All states" nil)
                 (const "open")
                 (const "closed")
                 string))

(defcustom rvb-gh-project-gh-status
  (getenv "GH_PROJECT_STATUS")
  "Default GitHub Project status filter used when fetching stories.
Use nil or \"all\" to omit the status filter.  Prefix a value with \"-\" to
exclude it, for example \"-Done\"."
  :type '(choice (const :tag "All statuses" nil)
                 (const "Todo")
                 (const "In Progress")
                 (const "Done")
                 (const "-Done")
                 string))

(defcustom rvb-gh-project-assignee
  (or (getenv "GH_PROJECT_ASSIGNEE") "@me")
  "Default assignee used when fetching stories.
Use nil or \"all\" to omit the assignee filter."
  :type '(choice (const :tag "All assignees" nil)
                 (const "@me")
                 string))

(defcustom rvb-gh-project-item-limit
  500
  "Maximum number of project items to fetch per project."
  :type 'integer)

(defcustom rvb-gh-project-sync-target-file nil
  "Org file where new GitHub stories are inserted.
Existing GitHub-backed headings are updated wherever they already live in
`org-agenda-files'.  This file is only used for stories that do not have an
existing heading.

When nil, use work.org in `org-directory'.  Relative paths are resolved inside
`org-directory'."
  :type '(choice (const :tag "work.org in org-directory" nil)
                 file))

(defvaralias 'rvb-gh-project-org-file 'rvb-gh-project-sync-target-file)

(defcustom rvb-gh-project-sync-target-heading "GitHub Stories"
  "Top-level heading under which new GitHub stories are inserted."
  :type 'string)

(defcustom rvb-gh-project-sync-missing-action 'delete
  "What to do with managed GitHub story headings missing from the latest sync.
Only headings with `GH_MANAGED' set are affected.

nil keeps stale headings untouched, `done' marks them done, and `delete'
removes the subtree."
  :type '(choice (const :tag "Keep" nil)
                 (const :tag "Mark DONE" done)
                 (const :tag "Delete" delete)))

(defcustom rvb-gh-project-sync-todo-state t
  "When non-nil, TODO state changes on GitHub-backed headings update GitHub.
Changing a heading to a done keyword closes the issue.  Changing it from a done
keyword back to a todo keyword reopens the issue."
  :type 'boolean)

(defcustom rvb-gh-project-sync-deadline-from-iteration t
  "When non-nil, set Org deadlines from GitHub Project iteration end dates.
The deadline is only written when the synced project item includes enough
iteration metadata to determine the iteration's final day."
  :type 'boolean)

(defcustom rvb-gh-project-close-reason "completed"
  "Reason to pass to `gh issue close'.
Use nil or an empty string to omit the reason."
  :type '(choice (const :tag "No reason" nil)
                 (const "completed")
                 (const "not planned")
                 string))

(defcustom rvb-gh-project-close-comment nil
  "Optional comment to add when closing an issue from Org."
  :type '(choice (const :tag "No comment" nil) string))

(defcustom rvb-gh-project-reopen-comment nil
  "Optional comment to add when reopening an issue from Org."
  :type '(choice (const :tag "No comment" nil) string))

(defcustom rvb-gh-project-hide-property-drawers t
  "When non-nil, hide property drawers in Org buffers touched by this package."
  :type 'boolean)

(defvar rvb-gh-project--syncing-todo-state nil)

(defconst rvb-gh-project--description-heading "Description")

(defvar rvb-gh-project--iteration-deadline-cache (make-hash-table :test 'equal))

(defun rvb-gh-project--save-custom-variable (variable value)
  "Set VARIABLE to VALUE and persist it with Customize."
  (customize-save-variable variable value)
  value)

(cl-defstruct rvb-gh-project-entry
  key repo issue managed marker)

(defun rvb-gh-project--aget (key alist)
  "Return KEY from ALIST using `assq'."
  (alist-get key alist nil nil #'eq))

(defun rvb-gh-project--read-file-string (file)
  "Return FILE contents as a string."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun rvb-gh-project--run (&rest args)
  "Run gh ARGS and return stdout as a trimmed string."
  (let ((errfile (make-temp-file "rvb-gh-projects-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'process-file
                               rvb-gh-project-gh-executable
                               nil
                               (list t errfile)
                               nil
                               args)))
            (let ((stdout (string-trim-right (buffer-string)))
                  (stderr (string-trim
                           (or (rvb-gh-project--read-file-string errfile)
                               ""))))
              (unless (and (integerp status) (zerop status))
                (error "gh %s failed%s%s"
                       (string-join args " ")
                       (if (string-empty-p stderr) "" ": ")
                       stderr))
              stdout)))
      (ignore-errors (delete-file errfile)))))

(defun rvb-gh-project--run-json (&rest args)
  "Run gh ARGS and parse stdout as JSON."
  (let ((output (apply #'rvb-gh-project--run args)))
    (if (string-empty-p output)
        nil
      (json-parse-string output
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil))))

(defun rvb-gh-project--read-owner ()
  "Read a GitHub Project owner."
  (let ((default (or rvb-gh-project-owner "@me")))
    (read-string (format "Project owner (%s): " default) nil nil default)))

(defun rvb-gh-project--read-project-list ()
  "Read one or more GitHub Project numbers."
  (let* ((default (string-join (or rvb-gh-project-numbers '()) " "))
         (prompt (if (string-empty-p default)
                     "Project numbers: "
                   (format "Project numbers (%s): " default)))
         (input (read-string prompt nil nil default))
         (projects (rvb-gh-project--split-list input)))
    (unless projects
      (user-error "No project numbers configured"))
    projects))

(defun rvb-gh-project--source-projects (source)
  "Return the project numbers from SOURCE."
  (let ((projects (cdr source)))
    (cond
     ((and (listp projects)
           (= (length projects) 1)
           (listp (car projects)))
      (car projects))
     ((listp projects) projects)
     (projects (list projects)))))

(defun rvb-gh-project--normalize-source (source)
  "Normalize SOURCE to (OWNER . PROJECTS), with PROJECTS as strings."
  (let* ((owner (car source))
         (projects (mapcar (lambda (project)
                             (format "%s" project))
                           (rvb-gh-project--source-projects source))))
    (when (and owner projects)
      (cons (format "%s" owner) projects))))

(defun rvb-gh-project--normalize-sources (sources)
  "Normalize SOURCES to an alist of owners and project number lists."
  (delq nil (mapcar #'rvb-gh-project--normalize-source sources)))

(defun rvb-gh-project--configured-sources ()
  "Return configured project sources, falling back to owner/projects."
  (or (rvb-gh-project--normalize-sources rvb-gh-project-sources)
      (when (and rvb-gh-project-owner rvb-gh-project-numbers)
        (list (cons rvb-gh-project-owner
                    (mapcar (lambda (project)
                              (format "%s" project))
                            rvb-gh-project-numbers))))))

(defun rvb-gh-project--format-sources (sources)
  "Return SOURCES formatted for prompts and org metadata."
  (string-join
   (mapcar (lambda (source)
             (format "%s:%s"
                     (car source)
                     (string-join (cdr source) ",")))
           (rvb-gh-project--normalize-sources sources))
   " "))

(defun rvb-gh-project--read-source-list ()
  "Read one or more GitHub Project owner/project combinations."
  (let* ((default (rvb-gh-project--format-sources
                   (rvb-gh-project--configured-sources)))
         (prompt (if (string-empty-p default)
                     "Project sources (OWNER:PROJECT[,PROJECT] ...): "
                   (format "Project sources (%s): " default)))
         (input (read-string prompt nil nil default))
         (sources (rvb-gh-project--parse-sources input)))
    (unless sources
      (user-error "No project sources configured"))
    sources))

(defun rvb-gh-project--quote-query-token (value)
  "Return VALUE quoted when needed for GitHub Projects search syntax."
  (let ((value (format "%s" value)))
    (if (string-match-p "[[:space:]\":]" value)
        (format "\"%s\""
                (replace-regexp-in-string
                 "\""
                 "\\\\\""
                 (replace-regexp-in-string "\\\\" "\\\\\\\\" value nil t)
                 nil
                 t))
      value)))

(defun rvb-gh-project--query-field-term (field value)
  "Return a GitHub Projects query term for FIELD set to VALUE."
  (format "%s:%s"
          (rvb-gh-project--quote-query-token field)
          (rvb-gh-project--quote-query-token value)))

(defun rvb-gh-project--query-status-term (status)
  "Return a GitHub Projects query term for STATUS."
  (unless (rvb-gh-project--all-value-p status)
    (let ((status (string-trim (format "%s" status))))
      (if (and (> (length status) 1)
               (string-prefix-p "-" status))
          (format "-%s"
                  (rvb-gh-project--query-field-term
                   "status"
                   (string-remove-prefix "-" status)))
        (rvb-gh-project--query-field-term "status" status)))))

(defun rvb-gh-project-query (&optional iteration-field)
  "Return the default Projects search query for assigned stories.
When ITERATION-FIELD is non-nil, include the configured iteration value using
that project field name.  Iteration field names are discovered per project."
  (string-join
   (delq nil
         (list (unless (rvb-gh-project--all-value-p rvb-gh-project-assignee)
                 (format "assignee:%s" rvb-gh-project-assignee))
               "is:issue"
               (unless (rvb-gh-project--all-value-p rvb-gh-project-state)
                 (format "is:%s" rvb-gh-project-state))
               (rvb-gh-project--query-status-term rvb-gh-project-gh-status)
               (unless (rvb-gh-project--all-value-p
                        rvb-gh-project-iteration-value)
                 (when iteration-field
                   (rvb-gh-project--query-field-term
                    iteration-field
                    rvb-gh-project-iteration-value)))))
   " "))

(defun rvb-gh-project--one-line (value)
  "Return VALUE as a single-line string."
  (string-trim
   (replace-regexp-in-string "[\n\r\t ]+" " " (or value ""))))

(defun rvb-gh-project--field-value-title (value)
  "Return a display title for project field VALUE."
  (cond
   ((stringp value) value)
   ((listp value)
    (or (rvb-gh-project--aget 'title value)
        (rvb-gh-project--aget 'name value)
        (rvb-gh-project--aget 'text value)
        ""))
   (t "")))

(defun rvb-gh-project--parse-iso-date (value)
  "Return VALUE as a Gregorian (MONTH DAY YEAR) date, or nil."
  (when (and (stringp value)
             (string-match
              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
              value))
    (list (string-to-number (match-string 2 value))
          (string-to-number (match-string 3 value))
          (string-to-number (match-string 1 value)))))

(defun rvb-gh-project--format-gregorian-date (date)
  "Return Gregorian DATE as YYYY-MM-DD."
  (pcase-let ((`(,month ,day ,year) date))
    (format "%04d-%02d-%02d" year month day)))

(defun rvb-gh-project--date-add-days (date days)
  "Return ISO DATE plus DAYS days, preserving calendar dates."
  (when-let ((gregorian (rvb-gh-project--parse-iso-date date)))
    (rvb-gh-project--format-gregorian-date
     (calendar-gregorian-from-absolute
      (+ (calendar-absolute-from-gregorian gregorian) days)))))

(defun rvb-gh-project--number-value (value)
  "Return VALUE as a number, or nil when it is not numeric."
  (cond
   ((numberp value) value)
   ((and (stringp value)
         (string-match-p "\\`[0-9]+\\'" value))
    (string-to-number value))))

(defun rvb-gh-project--iteration-deadline (value)
  "Return the final date of iteration VALUE as YYYY-MM-DD, or nil."
  (when (listp value)
    (or (rvb-gh-project--aget 'endDate value)
        (rvb-gh-project--aget 'end-date value)
        (when-let* ((start-date (or (rvb-gh-project--aget 'startDate value)
                                    (rvb-gh-project--aget 'start-date value)))
                    (duration (rvb-gh-project--number-value
                               (rvb-gh-project--aget 'duration value))))
          (rvb-gh-project--date-add-days start-date (max 0 (1- duration)))))))

(defun rvb-gh-project--iteration-lookup-keys (value)
  "Return possible lookup keys for iteration VALUE."
  (delete-dups
   (delq nil
         (cond
          ((stringp value) (list value))
          ((listp value)
           (mapcar (lambda (key)
                     (when-let ((value (rvb-gh-project--aget key value)))
                       (format "%s" value)))
                   '(id title name text)))
          (t nil)))))

(defun rvb-gh-project--lookup-iteration-deadline (value deadlines)
  "Return the deadline for iteration VALUE from DEADLINES."
  (seq-some (lambda (key)
              (gethash key deadlines))
            (rvb-gh-project--iteration-lookup-keys value)))

(defun rvb-gh-project--iteration-deadline-for-value (value deadlines)
  "Return iteration deadline for VALUE, using DEADLINES as a fallback."
  (or (rvb-gh-project--iteration-deadline value)
      (and deadlines
           (rvb-gh-project--lookup-iteration-deadline value deadlines))))

(defconst rvb-gh-project--viewer-project-query
  "query($number: Int!) {
     viewer {
       projectV2(number: $number) {
         ...ProjectFields
       }
     }
   }
   fragment ProjectFields on ProjectV2 {
     fields(first: 100) {
       nodes {
         __typename
         ... on ProjectV2IterationField {
           id
           name
           configuration {
             iterations { id title startDate duration }
             completedIterations { id title startDate duration }
           }
         }
       }
     }
   }")

(defconst rvb-gh-project--owner-project-query
  "query($owner: String!, $number: Int!) {
     organization(login: $owner) {
       projectV2(number: $number) {
         ...ProjectFields
       }
     }
     user(login: $owner) {
       projectV2(number: $number) {
         ...ProjectFields
       }
     }
   }
   fragment ProjectFields on ProjectV2 {
     fields(first: 100) {
       nodes {
         __typename
         ... on ProjectV2IterationField {
           id
           name
           configuration {
             iterations { id title startDate duration }
             completedIterations { id title startDate duration }
           }
         }
       }
     }
   }")

(defun rvb-gh-project--project-graphql (owner project)
  "Return GraphQL data for OWNER PROJECT."
  (if (string= owner "@me")
      (rvb-gh-project--run-json
       "api" "graphql"
       "-f" (concat "query=" rvb-gh-project--viewer-project-query)
       "-F" (format "number=%s" project))
    (rvb-gh-project--run-json
     "api" "graphql"
     "-f" (concat "query=" rvb-gh-project--owner-project-query)
     "-F" (format "owner=%s" owner)
     "-F" (format "number=%s" project))))

(defun rvb-gh-project--project-node-from-graphql (data)
  "Return the project node from GraphQL DATA."
  (let* ((data (rvb-gh-project--aget 'data data))
         (viewer (rvb-gh-project--aget 'viewer data))
         (organization (rvb-gh-project--aget 'organization data))
         (user (rvb-gh-project--aget 'user data)))
    (or (and viewer (rvb-gh-project--aget 'projectV2 viewer))
        (and organization (rvb-gh-project--aget 'projectV2 organization))
        (and user (rvb-gh-project--aget 'projectV2 user)))))

(defun rvb-gh-project--project-field-nodes (project)
  "Return field nodes from PROJECT."
  (when-let ((fields (rvb-gh-project--aget 'fields project)))
    (rvb-gh-project--aget 'nodes fields)))

(defun rvb-gh-project--project-node (owner project)
  "Return the GitHub Project node for OWNER PROJECT."
  (when-let ((data (rvb-gh-project--project-graphql owner project)))
    (rvb-gh-project--project-node-from-graphql data)))

(defun rvb-gh-project--project-iteration-field (project)
  "Return the iteration field from PROJECT."
  (seq-find (lambda (field)
              (and (listp field)
                   (or (string= (or (rvb-gh-project--aget '__typename field)
                                     "")
                                "ProjectV2IterationField")
                       (rvb-gh-project--aget 'configuration field))))
            (rvb-gh-project--project-field-nodes project)))

(defun rvb-gh-project--project-iteration-field-name (project)
  "Return the discovered iteration field name from PROJECT."
  (when-let ((field (rvb-gh-project--project-iteration-field project)))
    (rvb-gh-project--aget 'name field)))

(defun rvb-gh-project--iteration-field-values (field)
  "Return all configured iteration values from FIELD."
  (when-let ((configuration (rvb-gh-project--aget 'configuration field)))
    (append (rvb-gh-project--aget 'iterations configuration)
            (rvb-gh-project--aget 'completedIterations configuration))))

(defun rvb-gh-project--put-iteration-deadline (deadlines iteration)
  "Record ITERATION's deadline in DEADLINES."
  (when-let ((deadline (rvb-gh-project--iteration-deadline iteration)))
    (dolist (key (rvb-gh-project--iteration-lookup-keys iteration))
      (puthash key deadline deadlines))))

(defun rvb-gh-project--iteration-deadlines-from-field (field)
  "Return a hash table of iteration ids/titles to deadlines from FIELD."
  (let ((deadlines (make-hash-table :test 'equal)))
    (dolist (iteration (rvb-gh-project--iteration-field-values field))
      (rvb-gh-project--put-iteration-deadline deadlines iteration))
    deadlines))

(defun rvb-gh-project--project-iteration-deadlines (owner project)
  "Return a hash table of iteration ids/titles to deadlines for OWNER PROJECT."
  (let ((cache-key (list owner project)))
    (or (gethash cache-key rvb-gh-project--iteration-deadline-cache)
        (puthash
         cache-key
         (condition-case err
             (if-let* ((project-node
                        (rvb-gh-project--project-node owner project))
                       (field
                        (rvb-gh-project--project-iteration-field
                         project-node)))
                 (rvb-gh-project--iteration-deadlines-from-field field)
               (make-hash-table :test 'equal))
           (error
            (message "Could not fetch iteration deadlines for %s:%s: %s"
                     owner
                     project
                     (error-message-string err))
            (make-hash-table :test 'equal)))
         rvb-gh-project--iteration-deadline-cache))))

(defun rvb-gh-project--item-field (item field)
  "Return project ITEM FIELD value."
  (when (and field (not (string-empty-p field)))
    (let ((field (downcase field)))
      (cdr (seq-find
            (lambda (entry)
              (string= (downcase
                        (format "%s" (car entry)))
                       field))
            item)))))

(defun rvb-gh-project--content-body (content)
  "Return issue body text from project item CONTENT."
  (or (rvb-gh-project--aget 'body content)
      (rvb-gh-project--aget 'bodyText content)
      (rvb-gh-project--aget 'description content)
      ""))

(defun rvb-gh-project--normalize-story
    (item owner project &optional deadlines iteration-field)
  "Normalize a project ITEM from OWNER PROJECT into an alist."
  (let* ((content (rvb-gh-project--aget 'content item))
         (type (rvb-gh-project--aget 'type content)))
    (when (string= type "Issue")
      (let* ((iteration-value (rvb-gh-project--item-field
                               item
                               iteration-field))
             (iteration (rvb-gh-project--field-value-title
                         iteration-value))
             (status (rvb-gh-project--field-value-title
                      (rvb-gh-project--item-field
                       item rvb-gh-project-status-field))))
        `((owner . ,owner)
          (project . ,project)
          (repo . ,(rvb-gh-project--aget 'repository content))
          (number . ,(rvb-gh-project--aget 'number content))
          (title . ,(rvb-gh-project--aget 'title content))
          (body . ,(rvb-gh-project--content-body content))
          (url . ,(rvb-gh-project--aget 'url content))
          (status . ,status)
          (iteration . ,iteration)
          (deadline . ,(rvb-gh-project--iteration-deadline-for-value
                        iteration-value
                        deadlines)))))))

(defun rvb-gh-project--key (repo issue)
  "Return a stable key for REPO ISSUE."
  (format "%s#%s" repo issue))

(defun rvb-gh-project--story-key (story)
  "Return a stable key for STORY."
  (rvb-gh-project--key
   (or (rvb-gh-project--aget 'repo story) "")
   (or (rvb-gh-project--aget 'number story) "")))

(defun rvb-gh-project--dedupe-stories (stories)
  "Return STORIES deduplicated by repository and issue number."
  (let ((seen (make-hash-table :test 'equal))
        deduped)
    (dolist (story stories)
      (let ((key (rvb-gh-project--story-key story)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push story deduped))))
    (nreverse deduped)))

(defun rvb-gh-project--sort-stories (stories)
  "Return STORIES sorted by owner, project, repository, and issue number."
  (sort (copy-sequence stories)
        (lambda (left right)
          (string< (format "%s#%s#%s#%08d"
                           (or (rvb-gh-project--aget 'owner left) "")
                           (or (rvb-gh-project--aget 'project left) "")
                           (or (rvb-gh-project--aget 'repo left) "")
                           (string-to-number
                            (format "%s" (or (rvb-gh-project--aget 'number left) 0))))
                   (format "%s#%s#%s#%08d"
                           (or (rvb-gh-project--aget 'owner right) "")
                           (or (rvb-gh-project--aget 'project right) "")
                           (or (rvb-gh-project--aget 'repo right) "")
                           (string-to-number
                            (format "%s" (or (rvb-gh-project--aget 'number right) 0))))))))

(defun rvb-gh-project--item-list-args (owner project query)
  "Return gh arguments for listing PROJECT items owned by OWNER using QUERY."
  (let ((args (list "project" "item-list" project
                    "--owner" owner
                    "--limit" (number-to-string rvb-gh-project-item-limit)
                    "--format" "json")))
    (if (string-empty-p (string-trim (or query "")))
        args
      (append args (list "--query" query)))))

(defun rvb-gh-project--project-query-warning
    (owner project query iteration-field-name)
  "Warn when QUERY cannot include an iteration filter for OWNER PROJECT."
  (when (and (not query)
             (not iteration-field-name)
             (not (rvb-gh-project--all-value-p
                   rvb-gh-project-iteration-value)))
    (message "Could not discover an iteration field for %s:%s; omitting iteration filter"
             owner
             project)))

(defun rvb-gh-project-fetch-stories (owner projects &optional query)
  "Fetch assigned stories from OWNER PROJECTS using Projects QUERY."
  (let (stories)
    (dolist (project projects)
      (let* ((project-node
              (condition-case err
                  (rvb-gh-project--project-node owner project)
                (error
                 (message "Could not fetch project fields for %s:%s: %s"
                          owner
                          project
                          (error-message-string err))
                 nil)))
             (iteration-field
              (rvb-gh-project--project-iteration-field project-node))
             (iteration-field-name
              (and iteration-field
                   (rvb-gh-project--aget 'name iteration-field)))
             (project-query (or query
                                (rvb-gh-project-query iteration-field-name)))
             (data (apply #'rvb-gh-project--run-json
                          (rvb-gh-project--item-list-args
                           owner
                           project
                           project-query)))
             (items (rvb-gh-project--aget 'items data))
             (deadlines
              (and rvb-gh-project-sync-deadline-from-iteration
                   (or (and iteration-field
                            (rvb-gh-project--iteration-deadlines-from-field
                             iteration-field))
                       (rvb-gh-project--project-iteration-deadlines
                        owner
                        project)))))
        (rvb-gh-project--project-query-warning
         owner
         project
         query
         iteration-field-name)
        (dolist (item items)
          (when-let ((story (rvb-gh-project--normalize-story
                             item
                             owner
                             project
                             deadlines
                             iteration-field-name)))
            (push story stories)))))
    (nreverse stories)))

(defun rvb-gh-project-fetch-stories-from-sources (sources &optional query)
  "Fetch assigned stories from project SOURCES using Projects QUERY."
  (let (stories)
    (dolist (source (rvb-gh-project--normalize-sources sources))
      (setq stories
            (append stories
                    (rvb-gh-project-fetch-stories
                     (car source)
                     (cdr source)
                     query))))
    (rvb-gh-project--sort-stories
     (rvb-gh-project--dedupe-stories stories))))

(defun rvb-gh-project--issue-ref (story)
  "Return compact issue reference for STORY."
  (format "%s#%s"
          (or (rvb-gh-project--aget 'repo story) "")
          (or (rvb-gh-project--aget 'number story) "")))

(defun rvb-gh-project--safe-property-value (value)
  "Return VALUE as a single-line Org property value."
  (replace-regexp-in-string "\n" " " (format "%s" (or value ""))))

(defun rvb-gh-project--story-deadline (story)
  "Return STORY deadline date as YYYY-MM-DD, or nil."
  (let ((deadline (rvb-gh-project--aget 'deadline story)))
    (when (and (stringp deadline)
               (rvb-gh-project--parse-iso-date deadline))
      deadline)))

(defun rvb-gh-project--sync-story-deadline (story)
  "Set the current Org heading deadline from STORY when configured."
  (when (and rvb-gh-project-sync-deadline-from-iteration
             (rvb-gh-project--story-deadline story))
    (save-excursion
      (org-back-to-heading t)
      (let ((inhibit-message t))
        (org-deadline nil (rvb-gh-project--story-deadline story))))))

(defun rvb-gh-project--custom-id (story)
  "Return a stable CUSTOM_ID for STORY."
  (replace-regexp-in-string
   "-+\\'" ""
   (replace-regexp-in-string
    "[^[:alnum:]_-]+" "-"
    (downcase
     (format "gh-%s-%s-%s-%s"
             (or (rvb-gh-project--aget 'owner story) "")
             (or (rvb-gh-project--aget 'project story) "")
             (or (rvb-gh-project--aget 'repo story) "")
             (or (rvb-gh-project--aget 'number story) ""))))))

(defun rvb-gh-project--story-title (story)
  "Return the Org headline text for STORY, without the TODO keyword."
  (format "%s %s"
          (rvb-gh-project--issue-ref story)
          (rvb-gh-project--one-line (rvb-gh-project--aget 'title story))))

(defun rvb-gh-project--story-body (story)
  "Return STORY body text normalized for Org insertion."
  (string-trim-right
   (replace-regexp-in-string
    "\r\n?" "\n"
    (format "%s" (or (rvb-gh-project--aget 'body story) "")))))

(defun rvb-gh-project--org-src-safe-line (line)
  "Return LINE escaped when it could close an Org source block."
  (if (string-match-p "\\`[[:space:]]*#\\+end_src\\b" line)
      (concat "," line)
    line))

(defun rvb-gh-project--insert-markdown-source-block (text)
  "Insert TEXT as an Org Markdown source block."
  (insert "#+begin_src markdown\n")
  (dolist (line (split-string text "\n"))
    (insert (rvb-gh-project--org-src-safe-line line) "\n"))
  (insert "#+end_src\n"))

(defun rvb-gh-project--insert-story-description-content (story)
  "Insert the managed description content for STORY."
  (let ((body (rvb-gh-project--story-body story)))
    (unless (string-empty-p body)
      (rvb-gh-project--insert-markdown-source-block body))))

(defun rvb-gh-project--insert-story-description-subtree (story level)
  "Insert STORY description as an Org subtree at LEVEL."
  (insert (make-string level ?*) " "
          rvb-gh-project--description-heading
          "\n")
  (rvb-gh-project--insert-story-description-content story))

(defun rvb-gh-project--direct-child-heading-position (title)
  "Return point of the direct child heading TITLE under current Org heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((parent-level (org-outline-level))
          (end (save-excursion
                 (org-end-of-subtree t t)
                 (point)))
          found)
      (forward-line 1)
      (while (and (not found)
                  (re-search-forward org-heading-regexp end t))
        (beginning-of-line)
        (let ((level (org-outline-level))
              (heading (nth 4 (org-heading-components))))
          (cond
           ((<= level parent-level)
            (goto-char end))
           ((and (= level (1+ parent-level))
                 (string= heading title))
            (setq found (point)))))
        (unless found
          (forward-line 1)))
      found)))

(defun rvb-gh-project--upsert-story-description (story)
  "Update or insert the managed Description child subtree for STORY."
  (org-back-to-heading t)
  (let* ((parent-level (org-outline-level))
         (description-pos
          (rvb-gh-project--direct-child-heading-position
           rvb-gh-project--description-heading)))
    (if description-pos
        (save-excursion
          (goto-char description-pos)
          (forward-line 1)
          (delete-region (point)
                         (save-excursion
                           (org-end-of-subtree t t)
                           (point)))
          (rvb-gh-project--insert-story-description-content story))
      (let ((notes-pos (rvb-gh-project--direct-child-heading-position "Notes")))
        (goto-char (or notes-pos
                       (save-excursion
                         (org-end-of-subtree t t)
                         (point))))
        (unless (bolp)
          (insert "\n"))
        (rvb-gh-project--insert-story-description-subtree
         story
         (1+ parent-level))))))

(defun rvb-gh-project--entry-body-end ()
  "Return the end of the current entry body before child headings."
  (save-excursion
    (org-back-to-heading t)
    (let ((parent-level (org-outline-level))
          (subtree-end (save-excursion
                         (org-end-of-subtree t t)
                         (point))))
      (forward-line 1)
      (catch 'end
        (while (re-search-forward org-heading-regexp subtree-end t)
          (beginning-of-line)
          (when (> (org-outline-level) parent-level)
            (throw 'end (point)))
          (forward-line 1))
        subtree-end))))

(defun rvb-gh-project--delete-open-github-link ()
  "Delete the generated Open on GitHub link from the current entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((limit (rvb-gh-project--entry-body-end)))
      (forward-line 1)
      (when (re-search-forward
             "^\\[\\[[^\n]+\\]\\[Open on GitHub\\]\\]\n"
             limit
             t)
        (let ((begin (match-beginning 0)))
          (while (and (< (point) limit)
                      (looking-at-p "[ \t]*\n"))
            (forward-line 1))
          (delete-region begin (point)))))))

(defun rvb-gh-project--put-story-properties (story)
  "Write STORY metadata into the current Org heading."
  (org-entry-put nil "CUSTOM_ID" (rvb-gh-project--custom-id story))
  (org-entry-put nil "CATEGORY"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'repo story)))
  (org-entry-put nil "GH_MANAGED" "t")
  (org-entry-put nil "GH_URL"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'url story)))
  (org-entry-put nil "GH_REPO"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'repo story)))
  (org-entry-put nil "GH_ISSUE"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'number story)))
  (org-entry-put nil "GH_OWNER"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'owner story)))
  (org-entry-put nil "GH_PROJECT"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'project story)))
  (org-entry-put nil "GH_STATUS"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'status story)))
  (org-entry-put nil "GH_ITERATION"
                 (rvb-gh-project--safe-property-value
                  (rvb-gh-project--aget 'iteration story)))
  (org-entry-delete nil "GH_ITERATION_END")
  (rvb-gh-project--delete-open-github-link)
  (rvb-gh-project--sync-story-deadline story))

(defun rvb-gh-project--insert-story-org (story level)
  "Insert STORY as an Org TODO subtree at LEVEL."
  (insert (make-string level ?*) " TODO "
          (rvb-gh-project--story-title story)
          "\n")
  (rvb-gh-project--put-story-properties story)
  (rvb-gh-project--insert-story-description-subtree story (1+ level))
  (insert (make-string (1+ level) ?*) " Notes\n\n"))

(defun rvb-gh-project--agenda-files ()
  "Return expanded Org agenda files, expanding agenda directories."
  (delete-dups
   (cl-mapcan
    (lambda (entry)
      (let ((path (expand-file-name entry)))
        (cond
         ((file-directory-p path)
          (directory-files-recursively path "\\.org\\'"))
         ((file-exists-p path)
          (list path)))))
    org-agenda-files)))

(defun rvb-gh-project--target-file ()
  "Return the file used for newly inserted GitHub stories."
  (let ((target (or rvb-gh-project-sync-target-file "work.org")))
    (expand-file-name target org-directory)))

(defun rvb-gh-project--agenda-files-for-sync ()
  "Return files to scan when syncing."
  (delete-dups
   (cons (rvb-gh-project--target-file)
         (rvb-gh-project--agenda-files))))

(defun rvb-gh-project--truthy-string-p (value)
  "Return non-nil when VALUE represents true."
  (and value
       (member (downcase value) '("t" "true" "yes" "1"))))

(defun rvb-gh-project--scan-org-entries ()
  "Return a hash table of existing GitHub-backed Org headings."
  (let ((entries (make-hash-table :test 'equal)))
    (dolist (file (rvb-gh-project--agenda-files-for-sync))
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (org-map-entries
            (lambda ()
              (let ((repo (or (org-entry-get nil "GH_REPO")
                              (org-entry-get nil "REPO")))
                    (issue (or (org-entry-get nil "GH_ISSUE")
                               (org-entry-get nil "ISSUE")))
                    (managed (org-entry-get nil "GH_MANAGED")))
                (when (and repo issue
                           (not (string-empty-p repo))
                           (not (string-empty-p issue)))
                  (let ((key (rvb-gh-project--key repo issue)))
                    (unless (gethash key entries)
                      (puthash key
                               (make-rvb-gh-project-entry
                                :key key
                                :repo repo
                                :issue issue
                                :managed (rvb-gh-project--truthy-string-p managed)
                                :marker (copy-marker (point-marker)))
                               entries))))))
            nil
            'file)))))
    entries))

(defun rvb-gh-project--goto-target-heading ()
  "Go to the configured target heading, creating it if necessary.
Return the heading level."
  (org-with-wide-buffer
   (goto-char (point-min))
   (unless (re-search-forward "\\S-" nil t)
     (insert "#+title: GitHub Stories\n\n"))
   (catch 'found
     (goto-char (point-min))
     (org-map-entries
      (lambda ()
        (when (string= (nth 4 (org-heading-components))
                       rvb-gh-project-sync-target-heading)
          (throw 'found (car (org-heading-components)))))
      nil
      'file)
     (goto-char (point-max))
     (unless (bolp)
       (insert "\n"))
     (insert "* " rvb-gh-project-sync-target-heading "\n")
     (forward-line -1)
     1)))

(defun rvb-gh-project--insert-story-in-target (story)
  "Insert STORY under the configured target heading.
Return the buffer that was modified."
  (let ((file (rvb-gh-project--target-file)))
    (make-directory (file-name-directory file) t)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "#+title: GitHub Stories\n\n")))
    (with-current-buffer (find-file-noselect file)
      (org-mode)
      (let ((level (rvb-gh-project--goto-target-heading)))
        (org-end-of-subtree t t)
        (unless (bolp)
          (insert "\n"))
        (rvb-gh-project--insert-story-org story (1+ level)))
      (current-buffer))))

(defun rvb-gh-project--ensure-target-file ()
  "Ensure the target file and target heading exist.
Return the buffer that was visited."
  (let ((file (rvb-gh-project--target-file)))
    (make-directory (file-name-directory file) t)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "#+title: GitHub Stories\n\n")))
    (with-current-buffer (find-file-noselect file)
      (org-mode)
      (rvb-gh-project--goto-target-heading)
      (current-buffer))))

(defun rvb-gh-project--update-story-entry (entry story)
  "Update existing Org ENTRY from STORY.
Return the buffer that was modified."
  (let ((marker (rvb-gh-project-entry-marker entry))
        (rvb-gh-project--syncing-todo-state t))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (unless (equal (org-get-todo-state) "TODO")
          (org-todo "TODO"))
        (org-edit-headline (rvb-gh-project--story-title story))
        (rvb-gh-project--put-story-properties story)
        (rvb-gh-project--upsert-story-description story))
      (current-buffer))))

(defun rvb-gh-project--delete-entry-subtree (entry)
  "Delete the subtree for ENTRY and return the modified buffer."
  (let ((marker (rvb-gh-project-entry-marker entry)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (let ((begin (point))
              (end (save-excursion
                     (org-end-of-subtree t t)
                     (point))))
          (delete-region begin end)))
      (current-buffer))))

(defun rvb-gh-project--mark-entry-done (entry)
  "Mark ENTRY done and return the modified buffer."
  (let ((marker (rvb-gh-project-entry-marker entry))
        (rvb-gh-project--syncing-todo-state t))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (org-back-to-heading t)
        (unless (member (org-get-todo-state) org-done-keywords)
          (org-todo "DONE")))
      (current-buffer))))

(defun rvb-gh-project--handle-missing-entry (entry)
  "Handle synced Org ENTRY that is no longer in the GitHub result set.
Return the buffer that was modified, or nil."
  (when (rvb-gh-project-entry-managed entry)
    (pcase rvb-gh-project-sync-missing-action
      ('delete (rvb-gh-project--delete-entry-subtree entry))
      ('done (rvb-gh-project--mark-entry-done entry))
      (_ nil))))

(defun rvb-gh-project--save-buffers (buffers)
  "Save each live file buffer in BUFFERS."
  (dolist (buffer (delete-dups (delq nil buffers)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and buffer-file-name (buffer-modified-p))
          (save-buffer))))))

;;;###autoload
(defun rvb-gh-project-sync-org (&optional sources query)
  "Sync assigned GitHub Project stories into existing Org agenda files.
Existing headings are matched by `GH_REPO' and `GH_ISSUE'.  Missing stories are
inserted under `rvb-gh-project-sync-target-heading' in
`rvb-gh-project-sync-target-file'."
  (interactive)
  (let* ((sources (rvb-gh-project--normalize-sources
                   (or sources
                       (rvb-gh-project--configured-sources)
                       (rvb-gh-project--read-source-list))))
         (query query)
         (story-keys (make-hash-table :test 'equal))
         (existing (rvb-gh-project--scan-org-entries))
         touched
         (updated 0)
         (inserted 0)
         (removed 0))
    (clrhash rvb-gh-project--iteration-deadline-cache)
    (let ((stories (rvb-gh-project-fetch-stories-from-sources sources query)))
      (push (rvb-gh-project--ensure-target-file) touched)
      (dolist (story stories)
        (let* ((key (rvb-gh-project--story-key story))
               (entry (gethash key existing)))
          (puthash key t story-keys)
          (if entry
              (progn
                (push (rvb-gh-project--update-story-entry entry story) touched)
                (cl-incf updated))
            (push (rvb-gh-project--insert-story-in-target story) touched)
            (cl-incf inserted))))
      (maphash
       (lambda (key entry)
         (unless (gethash key story-keys)
           (when-let ((buffer (rvb-gh-project--handle-missing-entry entry)))
             (push buffer touched)
             (cl-incf removed))))
       existing))
    (rvb-gh-project--save-buffers touched)
    (rvb-gh-project-enable-agenda-file)
    (message "GitHub stories synced: %d updated, %d inserted, %d %s"
             updated
             inserted
             removed
             (pcase rvb-gh-project-sync-missing-action
               ('delete "deleted")
               ('done "marked done")
               (_ "left stale")))))

;;;###autoload
(defun rvb-gh-project-open-org-file ()
  "Open the file where new GitHub Project todos are inserted."
  (interactive)
  (find-file (rvb-gh-project--target-file)))

;;;###autoload
(defun rvb-gh-project-configure (owner projects)
  "Set GitHub Project OWNER and PROJECTS."
  (interactive
   (list (rvb-gh-project--read-owner)
         (rvb-gh-project--read-project-list)))
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-owner owner)
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-numbers projects)
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-sources
                                        (list (cons owner projects)))
  (message "GitHub Projects configured: owner=%s projects=%s"
           owner
           (string-join projects ", ")))

;;;###autoload
(defun rvb-gh-project-configure-sources (sources)
  "Set GitHub Project SOURCES."
  (interactive
   (list (rvb-gh-project--read-source-list)))
  (rvb-gh-project--save-custom-variable
   'rvb-gh-project-sources
   (rvb-gh-project--normalize-sources sources))
  (when rvb-gh-project-sources
    (rvb-gh-project--save-custom-variable
     'rvb-gh-project-owner
     (caar rvb-gh-project-sources))
    (rvb-gh-project--save-custom-variable
     'rvb-gh-project-numbers
     (cdar rvb-gh-project-sources)))
  (message "GitHub Project sources configured: %s"
           (rvb-gh-project--format-sources rvb-gh-project-sources)))

(defun rvb-gh-project--all-value-p (value)
  "Return non-nil when VALUE means no filter."
  (or (null value)
      (and (stringp value)
           (let ((value (string-trim value)))
             (or (string-empty-p value)
                 (string= (downcase value) "all"))))))

(defun rvb-gh-project--empty-value-p (value)
  "Return non-nil when VALUE is nil or an empty string."
  (or (null value)
      (and (stringp value)
           (string-empty-p (string-trim value)))))

(defun rvb-gh-project--setting-value-string (value &optional unset)
  "Return VALUE as a compact display string.
Use UNSET when VALUE is nil or an empty string."
  (cond
   ((rvb-gh-project--empty-value-p value) (or unset "unset"))
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t (format "%s" value))))

(defun rvb-gh-project--setting-description (label value &optional unset)
  "Return a Transient description for LABEL showing VALUE."
  (format "%-16s %s"
          (concat label ":")
          (propertize
           (rvb-gh-project--setting-value-string value unset)
           'face 'transient-value)))

(defun rvb-gh-project--sources-label ()
  "Return the configured sources for display."
  (let ((sources (rvb-gh-project--format-sources
                  (rvb-gh-project--configured-sources))))
    (unless (string-empty-p sources)
      sources)))

(defun rvb-gh-project--iteration-value-label ()
  "Return the configured iteration value for display."
  (if (rvb-gh-project--all-value-p rvb-gh-project-iteration-value)
      "all"
    rvb-gh-project-iteration-value))

(defun rvb-gh-project--assignee-label ()
  "Return the configured assignee filter for display."
  (if (rvb-gh-project--all-value-p rvb-gh-project-assignee)
      "all"
    rvb-gh-project-assignee))

(defun rvb-gh-project--state-label ()
  "Return the configured issue state for display."
  (if (rvb-gh-project--all-value-p rvb-gh-project-state)
      "all"
    rvb-gh-project-state))

(defun rvb-gh-project--gh-status-label ()
  "Return the configured GitHub Project status filter for display."
  (if (rvb-gh-project--all-value-p rvb-gh-project-gh-status)
      "all"
    rvb-gh-project-gh-status))

(defun rvb-gh-project--missing-action-label ()
  "Return the configured missing-heading action for display."
  (pcase rvb-gh-project-sync-missing-action
    ('delete "delete stale")
    ('done "mark DONE")
    (_ "keep stale")))

(defun rvb-gh-project--deadline-label ()
  "Return the configured deadline sync behavior for display."
  (if rvb-gh-project-sync-deadline-from-iteration
      "iteration end"
    "off"))

(defun rvb-gh-project--describe-sources ()
  "Describe the configured project sources for Transient."
  (rvb-gh-project--setting-description
   "sources"
   (rvb-gh-project--sources-label)
   "unset"))

(defun rvb-gh-project--describe-iteration-value ()
  "Describe the configured iteration value for Transient."
  (rvb-gh-project--setting-description
   "iteration"
   (rvb-gh-project--iteration-value-label)
   "all"))

(defun rvb-gh-project--describe-assignee ()
  "Describe the configured assignee filter for Transient."
  (rvb-gh-project--setting-description
   "assignee"
   (rvb-gh-project--assignee-label)
   "all"))

(defun rvb-gh-project--describe-gh-status ()
  "Describe the configured GitHub Project status filter for Transient."
  (rvb-gh-project--setting-description
   "gh status"
   (rvb-gh-project--gh-status-label)
   "all"))

(defun rvb-gh-project--describe-state ()
  "Describe the configured issue state for Transient."
  (rvb-gh-project--setting-description
   "state"
   (rvb-gh-project--state-label)
   "all"))

(defun rvb-gh-project--describe-target-file ()
  "Describe the configured Org target file for Transient."
  (rvb-gh-project--setting-description
   "target file"
   (abbreviate-file-name (rvb-gh-project--target-file))))

(defun rvb-gh-project--describe-target-heading ()
  "Describe the configured Org target heading for Transient."
  (rvb-gh-project--setting-description
   "heading"
   rvb-gh-project-sync-target-heading
   "unset"))

(defun rvb-gh-project--describe-missing-action ()
  "Describe the configured missing-heading action for Transient."
  (rvb-gh-project--setting-description
   "stale headings"
   (rvb-gh-project--missing-action-label)))

(defun rvb-gh-project--describe-deadline-sync ()
  "Describe the configured deadline sync behavior for Transient."
  (rvb-gh-project--setting-description
   "deadlines"
   (rvb-gh-project--deadline-label)))

(defun rvb-gh-project--read-iteration-value ()
  "Read an iteration value, returning nil for all iterations."
  (let* ((current (rvb-gh-project--iteration-value-label))
         (value (completing-read
                 (format "Iteration value (%s): " current)
                 '("@current" "@next" "@previous" "all")
                 nil
                 nil
                 nil
                 nil
                 current)))
    (unless (rvb-gh-project--all-value-p value)
      value)))

(defun rvb-gh-project--read-assignee ()
  "Read an assignee filter, returning nil for all assignees."
  (let* ((current (rvb-gh-project--assignee-label))
         (value (completing-read
                 (format "Assignee (%s): " current)
                 '("@me" "all")
                 nil
                 nil
                 nil
                 nil
                 current)))
    (unless (rvb-gh-project--all-value-p value)
      value)))

(defun rvb-gh-project--read-state ()
  "Read an issue state, returning nil for all states."
  (let ((value (completing-read
                (format "Issue state (%s): " (rvb-gh-project--state-label))
                '("open" "closed" "all")
                nil
                t
                nil
                nil
                (rvb-gh-project--state-label))))
    (unless (rvb-gh-project--all-value-p value)
      value)))

(defun rvb-gh-project--read-gh-status ()
  "Read a GitHub Project status filter, returning nil for all statuses."
  (let* ((current (rvb-gh-project--gh-status-label))
         (value (completing-read
                 (format "GitHub Project status (%s): " current)
                 '("all" "Todo" "In Progress" "Done" "-Done")
                 nil
                 nil
                 nil
                 nil
                 current)))
    (unless (rvb-gh-project--all-value-p value)
      value)))

(defun rvb-gh-project--read-missing-action ()
  "Read the action for stale GitHub-backed Org headings."
  (let* ((choices '(("keep stale" . nil)
                    ("mark DONE" . done)
                    ("delete stale" . delete)))
         (selected (completing-read
                    (format "When a synced story is missing (%s): "
                            (rvb-gh-project--missing-action-label))
                    (mapcar #'car choices)
                    nil
                    t
                    nil
                    nil
                    (rvb-gh-project--missing-action-label))))
    (cdr (assoc selected choices))))

(defun rvb-gh-project--read-deadline-sync ()
  "Read whether to sync deadlines from iteration end dates."
  (string= (completing-read
            (format "Sync deadlines from iterations (%s): "
                    (rvb-gh-project--deadline-label))
            '("iteration end" "off")
            nil
            t
            nil
            nil
            (rvb-gh-project--deadline-label))
           "iteration end"))

(defun rvb-gh-project--setting-message ()
  "Return a one-line summary of GitHub project sync settings."
  (format "sources=%s assignee=%s state=%s gh_status=%s iteration=%s(auto field) target=%s heading=%s missing=%s deadlines=%s"
          (or (rvb-gh-project--sources-label) "unset")
          (rvb-gh-project--assignee-label)
          (rvb-gh-project--state-label)
          (rvb-gh-project--gh-status-label)
          (rvb-gh-project--iteration-value-label)
          (abbreviate-file-name (rvb-gh-project--target-file))
          rvb-gh-project-sync-target-heading
          (rvb-gh-project--missing-action-label)
          (rvb-gh-project--deadline-label)))

(defun rvb-gh-project-show-settings ()
  "Show current GitHub project sync settings."
  (interactive)
  (message "%s" (rvb-gh-project--setting-message)))

(defun rvb-gh-project-set-iteration-value (value)
  "Set the GitHub Project iteration VALUE used in sync queries."
  (interactive
   (list (rvb-gh-project--read-iteration-value)))
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-iteration-value
                                        (unless (rvb-gh-project--all-value-p value)
                                          value))
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-assignee (assignee)
  "Set the GitHub Project ASSIGNEE filter used in sync queries."
  (interactive
   (list (rvb-gh-project--read-assignee)))
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-assignee
                                        (unless (rvb-gh-project--all-value-p
                                                 assignee)
                                          assignee))
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-iteration-current ()
  "Sync the current configured iteration."
  (interactive)
  (rvb-gh-project-set-iteration-value "@current"))

(defun rvb-gh-project-set-iteration-next ()
  "Sync the next configured iteration."
  (interactive)
  (rvb-gh-project-set-iteration-value "@next"))

(defun rvb-gh-project-set-iteration-previous ()
  "Sync the previous configured iteration."
  (interactive)
  (rvb-gh-project-set-iteration-value "@previous"))

(defun rvb-gh-project-set-iteration-all ()
  "Sync all iterations by omitting the iteration filter."
  (interactive)
  (rvb-gh-project-set-iteration-value nil))

(defun rvb-gh-project-set-state (state)
  "Set the GitHub issue STATE used in sync queries."
  (interactive
   (list (rvb-gh-project--read-state)))
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-state
                                        (unless (rvb-gh-project--all-value-p state)
                                          state))
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-state-open ()
  "Sync open GitHub issues."
  (interactive)
  (rvb-gh-project-set-state "open"))

(defun rvb-gh-project-set-state-closed ()
  "Sync closed GitHub issues."
  (interactive)
  (rvb-gh-project-set-state "closed"))

(defun rvb-gh-project-set-state-all ()
  "Sync all GitHub issue states."
  (interactive)
  (rvb-gh-project-set-state nil))

(defun rvb-gh-project-set-gh-status (status)
  "Set the GitHub Project STATUS filter used in sync queries."
  (interactive
   (list (rvb-gh-project--read-gh-status)))
  (rvb-gh-project--save-custom-variable
   'rvb-gh-project-gh-status
   (unless (rvb-gh-project--all-value-p status)
     status))
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-target-file (file)
  "Set the Org FILE where new GitHub stories are inserted."
  (interactive
   (list (read-file-name "GitHub story target file: "
                         org-directory
                         (rvb-gh-project--target-file))))
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-sync-target-file file)
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-target-heading (heading)
  "Set the Org target HEADING where new GitHub stories are inserted."
  (interactive
   (list (read-string "GitHub story target heading: "
                      rvb-gh-project-sync-target-heading)))
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-sync-target-heading
                                        heading)
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-missing-action (action)
  "Set ACTION for managed Org headings missing from the latest sync."
  (interactive
   (list (rvb-gh-project--read-missing-action)))
  (rvb-gh-project--save-custom-variable 'rvb-gh-project-sync-missing-action
                                        action)
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-deadline-sync (enabled)
  "Set whether synced stories get iteration-end deadlines.
When ENABLED is non-nil, deadlines are set from GitHub Project iteration
metadata."
  (interactive
   (list (rvb-gh-project--read-deadline-sync)))
  (rvb-gh-project--save-custom-variable
   'rvb-gh-project-sync-deadline-from-iteration
   enabled)
  (rvb-gh-project-show-settings))

(defun rvb-gh-project-set-missing-keep ()
  "Keep managed Org headings missing from the latest GitHub sync."
  (interactive)
  (rvb-gh-project-set-missing-action nil))

(defun rvb-gh-project-set-missing-done ()
  "Mark managed Org headings done when missing from the latest GitHub sync."
  (interactive)
  (rvb-gh-project-set-missing-action 'done))

(defun rvb-gh-project-set-missing-delete ()
  "Delete managed Org headings missing from the latest GitHub sync."
  (interactive)
  (rvb-gh-project-set-missing-action 'delete))

;;;###autoload
(transient-define-prefix rvb-gh-project-menu ()
  "Configure and sync GitHub Project stories."
  ["Run"
   ("s" "sync now" rvb-gh-project-sync-org)
   ("f" "open target file" rvb-gh-project-open-org-file)
   ("?" "show current settings" rvb-gh-project-show-settings)]
  ["Query"
   ("S" rvb-gh-project--describe-sources
    rvb-gh-project-configure-sources :transient t)
   ("a" rvb-gh-project--describe-assignee
    rvb-gh-project-set-assignee :transient t)
   ("i" rvb-gh-project--describe-iteration-value
    rvb-gh-project-set-iteration-value :transient t)
   ("g" rvb-gh-project--describe-gh-status
    rvb-gh-project-set-gh-status :transient t)
   ("x" rvb-gh-project--describe-state
    rvb-gh-project-set-state :transient t)]
  ["Org"
   ("t" rvb-gh-project--describe-target-file
    rvb-gh-project-set-target-file :transient t)
   ("h" rvb-gh-project--describe-target-heading
    rvb-gh-project-set-target-heading :transient t)
   ("D" rvb-gh-project--describe-deadline-sync
    rvb-gh-project-set-deadline-sync :transient t)
   ("m" rvb-gh-project--describe-missing-action
    rvb-gh-project-set-missing-action :transient t)])

;;;###autoload
(defun rvb-gh-project-enable-agenda-file ()
  "Ensure `rvb-gh-project-sync-target-file' is covered by `org-agenda-files'."
  (interactive)
  (unless (seq-some
           (lambda (entry)
             (let ((path (expand-file-name entry)))
               (or (string= path (rvb-gh-project--target-file))
                   (and (file-directory-p path)
                        (file-in-directory-p
                         (rvb-gh-project--target-file)
                         path)))))
           org-agenda-files)
    (add-to-list 'org-agenda-files (rvb-gh-project--target-file))))

(defun rvb-gh-project--current-entry-ref ()
  "Return (REPO ISSUE) for the current GitHub-backed Org entry."
  (let ((repo (or (org-entry-get nil "GH_REPO")
                  (org-entry-get nil "REPO")))
        (issue (or (org-entry-get nil "GH_ISSUE")
                   (org-entry-get nil "ISSUE"))))
    (when (and repo issue
               (not (string-empty-p repo))
               (not (string-empty-p issue)))
      (list repo issue))))

(defun rvb-gh-project--run-issue-state-command (repo issue action)
  "Run gh issue ACTION for ISSUE in REPO."
  (let ((args (list "issue" action issue "--repo" repo)))
    (cond
     ((and (string= action "close")
           rvb-gh-project-close-reason
           (not (string-empty-p rvb-gh-project-close-reason)))
      (setq args (append args (list "--reason" rvb-gh-project-close-reason))))
     ((and (string= action "reopen")
           rvb-gh-project-reopen-comment
           (not (string-empty-p rvb-gh-project-reopen-comment)))
      (setq args (append args (list "--comment" rvb-gh-project-reopen-comment)))))
    (when (and (string= action "close")
               rvb-gh-project-close-comment
               (not (string-empty-p rvb-gh-project-close-comment)))
      (setq args (append args (list "--comment" rvb-gh-project-close-comment))))
    (apply #'rvb-gh-project--run args)))

(defun rvb-gh-project--revert-todo-state (state)
  "Revert the current heading to TODO STATE without syncing to GitHub."
  (when state
    (let ((rvb-gh-project--syncing-todo-state t))
      (org-todo state))))

(defun rvb-gh-project--sync-todo-state-to-github ()
  "Close or reopen GitHub issues when Org TODO state changes."
  (when (and rvb-gh-project-sync-todo-state
             (not rvb-gh-project--syncing-todo-state)
             (derived-mode-p 'org-mode))
    (let* ((new-state org-state)
           (old-state org-last-state)
           (new-done (member new-state org-done-keywords))
           (old-done (member old-state org-done-keywords))
           (action (cond
                    ((and new-done (not old-done)) "close")
                    ((and old-done (not new-done)) "reopen")))
           (ref (and action (rvb-gh-project--current-entry-ref))))
      (when (and action ref)
        (condition-case err
            (progn
              (rvb-gh-project--run-issue-state-command
               (car ref)
               (cadr ref)
               action)
              (message "%s GitHub issue %s#%s"
                       (capitalize action)
                       (car ref)
                       (cadr ref)))
          (error
           (rvb-gh-project--revert-todo-state old-state)
           (user-error "Failed to %s GitHub issue %s#%s: %s"
                       action
                       (car ref)
                       (cadr ref)
                       (error-message-string err))))))))

(defun rvb-gh-project--setup-org-buffer ()
  "Set presentation defaults for Org buffers touched by this package."
  (when rvb-gh-project-hide-property-drawers
    (setq-local org-hide-drawer-startup t)
    (add-hook 'org-cycle-hook #'org-cycle-hide-drawers nil t)
    (save-excursion
      (goto-char (point-min))
      (org-cycle-hide-drawers 'all))))

(add-hook 'org-mode-hook #'rvb-gh-project--setup-org-buffer)
(add-hook 'org-after-todo-state-change-hook
          #'rvb-gh-project--sync-todo-state-to-github)

(provide 'rvb-gh-projects)

;;; rvb-gh-projects.el ends here
