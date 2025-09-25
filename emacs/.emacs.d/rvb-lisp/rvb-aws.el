;;; rvb-aws.el --- Pick an AWS profile, SSO login, set env  -*- lexical-binding: t; -*-

(require 'subr-x)

(defun aws--read-lines (file)
  "Return FILE as a list of lines, or nil if FILE doesn't exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t))))

(defun aws--collect-profiles ()
  "Return profile names from ~/.aws/config that are declared as [profile NAME].
Does NOT include [default] or any other section types."
  (let* ((home (or (getenv "HOME") "~"))
         (config (expand-file-name ".aws/config" home))
         (rx-profile "^\\[profile[[:space:]]+\\([^]]+\\)\\]$")
         (names '()))
    (dolist (line (aws--read-lines config))
      (when (string-match rx-profile line)
        (push (string-trim (match-string 1 line)) names)))
    (let ((uniq (delete-dups (nreverse names))))
      (if uniq
          (sort uniq #'string<)
        (user-error "No [profile NAME] sections found in %s" config)))))

(defun aws--call-to-string (program &rest args)
  "Run PROGRAM with ARGS, returning (EXIT-CODE . OUTPUT-STRING)."
  (with-temp-buffer
    (let ((code (apply #'process-file program nil (current-buffer) nil args)))
      (cons code (string-trim-right (buffer-string))))))

(defun aws--ensure-aws-cli ()
  (or (executable-find "aws")
      (user-error "Could not find `aws` on PATH. Please install AWS CLI v2")))

(defun aws--get-region (profile)
  "Get region configured for PROFILE, or nil."
  (when (= 0 (car (aws--call-to-string "aws" "configure" "get" "region" "--profile" profile)))
    (cdr (aws--call-to-string "aws" "configure" "get" "region" "--profile" profile))))

(defun aws--parse-export-env (export-text)
  "Parse lines like `export KEY=VALUE` into an alist."
  (let (pairs)
    (dolist (line (split-string export-text "\n" t))
      (when (string-match "^export\\s-+\\([A-Za-z0-9_]+\\)=\\(.*\\)$" line)
        (let ((k (match-string 1 line))
              (v (match-string 2 line)))
          ;; Remove surrounding quotes if present
          (when (and (>= (length v) 2)
                     (eq (aref v 0) ?\")
                     (eq (aref v (1- (length v))) ?\"))
            (setq v (substring v 1 -1)))
          (push (cons k v) pairs))))
    (nreverse pairs)))

(defun aws--set-env-from-alist (alist)
  "Set environment variables in Emacs from ALIST of (KEY . VAL)."
  (dolist (kv alist)
    (setenv (car kv) (cdr kv))))

;;;###autoload
(defun aws-sso-login (&optional profile)
  "Prompt for an AWS PROFILE, run SSO login, then set temp credentials in Emacs."
  (interactive)
  (aws--ensure-aws-cli)
  (let* ((profiles (aws--collect-profiles))
         (choice (or profile
                     (if profiles
                         (completing-read "AWS profile: " profiles nil t nil nil
                                          (car profiles))
                       (user-error "No AWS profiles found in ~/.aws/config or ~/.aws/credentials")))))
    ;; 1) SSO login
    (let* ((buf (get-buffer-create "*AWS SSO Login*"))
           (exit (with-current-buffer buf
                   (erase-buffer)
                   (let ((code (process-file "aws" nil buf t "sso" "login" "--profile" choice)))
                     (display-buffer buf)
                     code))))
      (when (not (= exit 0))
        (user-error "aws sso login failed for profile '%s' (see *AWS SSO Login*)" choice)))

    ;; 2) Export temporary creds
    (let* ((res (aws--call-to-string "aws" "configure" "export-credentials"
                                     "--profile" choice "--format" "env"))
           (code (car res))
           (out  (cdr res)))
      (when (not (= code 0))
        (user-error "`aws configure export-credentials` failed for '%s':\n%s" choice out))
      (let* ((env-pairs (aws--parse-export-env out))
             ;; Fill region if export didn't include it.
             (region (or (cdr (assoc "AWS_REGION" env-pairs))
                         (aws--get-region choice))))
        (aws--set-env-from-alist env-pairs)
        (when region
          (setenv "AWS_REGION" region)
          ;; Some tools read AWS_DEFAULT_REGION
          (setenv "AWS_DEFAULT_REGION" region))
        ;; Also set AWS_PROFILE for convenience.
        (setenv "AWS_PROFILE" choice)

        (message "Logged in as profile '%s'. Env set: AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN%s"
                 choice
                 (if region ", AWS_REGION/AWS_DEFAULT_REGION" ""))))))

(provide 'rvb-aws)
;;; rvb-aws.el ends here
