;;; rvb-project.el --- Project Dashboard -*- lexical-binding: t; -*-

(defvar-local rvb/project-root nil
  "The root directory of the project for this dashboard buffer.")

(defun rvb/project-dashboard (&optional project-dir)
  "Open a project dashboard showing GitHub info and project files.
If PROJECT-DIR is provided, use that as the project root.
Otherwise, use the current project or prompt for one."
  (interactive)
  (let* ((root (or project-dir
                   (when-let ((proj (project-current)))
                     (project-root proj))
                   (project-prompt-project-dir)))
         (buffer-name (format "*Project: %s*" (file-name-nondirectory (directory-file-name root))))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq rvb/project-root root)
        (setq default-directory root)

        ;; Get GitHub repo info first
        (let* ((default-directory root)
               (project-name (file-name-nondirectory (directory-file-name root)))
               (repo-info (when (executable-find "gh")
                           (shell-command-to-string "gh repo view --json url -q '.url' 2>/dev/null")))
               (repo-url (when (and repo-info (not (string-empty-p (string-trim repo-info))))
                          (string-trim repo-info))))

          ;; Project name header (clickable if GitHub repo)
          (insert "\n\n")
          (if repo-url
              (insert-text-button project-name
                                 'action (lambda (_) (browse-url repo-url))
                                 'follow-link t
                                 'help-echo (format "Open %s in browser" repo-url)
                                 'face '(:height 2.0 :weight bold :foreground "#5fafd7" :underline t))
            (insert (propertize project-name
                               'face '(:height 2.0 :weight bold :foreground "#5fafd7"))))
          (insert "\n")

          ;; Sync status right under project name
          (when (file-exists-p (expand-file-name ".git" root))
            (let* ((remote-info (shell-command-to-string "git rev-list --left-right --count @{upstream}...HEAD 2>/dev/null"))
                   (parts (split-string (string-trim remote-info) "\t")))
              (when (= (length parts) 2)
                (let ((behind (string-to-number (nth 0 parts)))
                      (ahead (string-to-number (nth 1 parts))))
                  (insert "  ")
                  (cond
                   ((and (= behind 0) (= ahead 0))
                    (insert (propertize "Up to date" 'face '(:foreground "#87af87"))))
                   ((and (> behind 0) (= ahead 0))
                    (insert (propertize (format "%d behind" behind) 'face '(:foreground "#d75f5f"))))
                   ((and (= behind 0) (> ahead 0))
                    (insert (propertize (format "%d ahead" ahead) 'face '(:foreground "#5fafd7"))))
                   ((and (> behind 0) (> ahead 0))
                    (insert (propertize (format "%d behind" behind) 'face '(:foreground "#d75f5f"))
                            (propertize ", " 'face 'default)
                            (propertize (format "%d ahead" ahead) 'face '(:foreground "#5fafd7")))))
                  (insert "\n"))))))) ; closes let (behind/ahead), when (length check), let* (remote-info), when (file-exists), let* (project-name/repo-info)

        (insert "\n\n")

        ;; GitHub section
        (when (executable-find "gh")
          (let ((default-directory root))
            ;; Check if this is a GitHub repository
            (let ((repo-check (shell-command-to-string "gh repo view --json nameWithOwner -q '.nameWithOwner' 2>/dev/null")))
              (when (string-match-p "^[a-zA-Z0-9-]+/[a-zA-Z0-9-_]+$" (string-trim repo-check))
                ;; Show open issues
                (insert (propertize "Issues\n" 'face '(:height 1.2 :weight bold :foreground "#5fafd7")))
                (let* ((issues-json (shell-command-to-string "gh issue list --limit 5 --json number,title,url 2>/dev/null"))
                       (issues (unless (string-empty-p (string-trim issues-json))
                                (append (json-parse-string issues-json :object-type 'alist) nil))))
                  (if (or (not issues) (= (length issues) 0))
                      (insert "  No open issues\n")
                    (dolist (issue issues)
                      (let ((number (alist-get 'number issue))
                            (title (alist-get 'title issue))
                            (url (alist-get 'url issue)))
                        (insert "  • ")
                        (insert-text-button (format "#%d %s" number title)
                                           'action (lambda (_) (browse-url url))
                                           'follow-link t
                                           'help-echo (format "Open %s" url)
                                           'face '(:foreground "default" :underline t))
                        (insert "\n")))
                    (insert "\n")))

                ;; Show open PRs
                (insert (propertize "Pull Requests\n" 'face '(:height 1.2 :weight bold :foreground "#5fafd7")))
                (let* ((prs-json (shell-command-to-string "gh pr list --limit 5 --json number,title,url 2>/dev/null"))
                       (prs (unless (string-empty-p (string-trim prs-json))
                             (append (json-parse-string prs-json :object-type 'alist) nil))))
                  (if (or (not prs) (= (length prs) 0))
                      (insert "  No open pull requests\n")
                    (dolist (pr prs)
                      (let ((number (alist-get 'number pr))
                            (title (alist-get 'title pr))
                            (url (alist-get 'url pr)))
                        (insert "  • ")
                        (insert-text-button (format "#%d %s" number title)
                                           'action (lambda (_) (browse-url url))
                                           'follow-link t
                                           'help-echo (format "Open %s" url)
                                           'face '(:foreground "default" :underline t))
                        (insert "\n")))
                    (insert "\n"))))))
          (insert "\n"))

        ;; Keybindings help
        (insert "\n")
        (insert (propertize "Keybindings\n" 'face '(:height 1.2 :weight bold :foreground "#5fafd7")))
        (insert "  " (propertize "g" 'face '(:foreground "#87af87" :weight bold)) " - Open Magit\n")
        (insert "  " (propertize "f" 'face '(:foreground "#87af87" :weight bold)) " - Find file\n")
        (insert "  " (propertize "d" 'face '(:foreground "#87af87" :weight bold)) " - Open Dired\n")
        (insert "  " (propertize "q" 'face '(:foreground "#d75f5f" :weight bold)) " - Quit\n")
        (insert "\n")

      ;; Set up keybindings before special-mode to capture root in closures
      (let ((project-root root))
        (setq-local rvb/project-root root)

        ;; Set up the major mode
        (special-mode)
        (use-local-map (copy-keymap special-mode-map))

        ;; Keybindings with captured project-root
        (local-set-key (kbd "g") (lambda ()
                                   (interactive)
                                   (let ((default-directory project-root))
                                     (delete-other-windows)
                                     (magit-status project-root))))
        (local-set-key (kbd "f") (lambda ()
                                   (interactive)
                                   (let ((default-directory project-root))
                                     (call-interactively #'project-find-file))))
        (local-set-key (kbd "d") (lambda ()
                                   (interactive)
                                   (dired project-root)))
        (local-set-key (kbd "q") #'quit-window))
      (goto-char (point-min))

    ;; Display dashboard and let magit create its own split
    (delete-other-windows)
    (switch-to-buffer buf))
    (magit-status root)))

;; Integrate with project-switch-project
(with-eval-after-load 'project
  ;; Set dashboard as the default project switch action
  ;; The first command in the list is the default
  (setq project-switch-commands
        '((rvb/project-dashboard "Dashboard" "d")
          (project-find-file "Find file" "f")
          (project-find-regexp "Find regexp" "g")
          (project-find-dir "Find directory" "D")
          (project-vc-dir "VC-Dir" "v")
          (project-shell "Shell" "s"))))

(provide 'rvb-project)
