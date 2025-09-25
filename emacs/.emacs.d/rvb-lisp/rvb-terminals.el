(use-package eat
  :ensure t
  :config
  )

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defgroup my/eshell-prompt nil
  "TRAMP-aware eshell prompt."
  :group 'eshell)

(defcustom my/eshell-dir-max 40
  "Max characters for displayed directory; longer paths are shortened."
  :type 'integer)

;; simple rolling counter (per eshell buffer)
(defvar-local my/eshell-cmd-count 0)
(defun my/eshell--bump-count () (setq my/eshell-cmd-count (1+ my/eshell-cmd-count)))
(add-hook 'eshell-pre-command-hook #'my/eshell--bump-count)

(defun my/eshell--shorten (s &optional max)
  "Shorten path S to MAX chars by collapsing middle components."
  (let* ((max (or max my/eshell-dir-max))
         (len (length s)))
    (if (<= len max) s
      ;; keep start/end, collapse middle
      (let* ((keep (/ (- max 3) 2))
             (head (substring s 0 keep))
             (tail (substring s (- len keep))))
        (concat head "..." tail)))))

(defun my/eshell--remote-parts (dir)
  "Return plist with TRAMP parts for DIR or nil if local.
Keys: :method :user :host :local"
  (when-let* ((vec (when (file-remote-p dir)
                     (tramp-dissect-file-name dir)))
              ;; In multi-hop, use the *target* hop (last).
              (hop (or (ignore-errors (tramp-file-name-hop vec)) vec)))
    `(:method ,(or (tramp-file-name-method hop) "")
      :user   ,(or (tramp-file-name-user   hop) "")
      :host   ,(or (tramp-file-name-host   hop) "")
      :local  ,(or (tramp-file-name-localname vec) ""))))

(defun my/eshell--face (sym)
  "Pick a face by semantic role SYM."
  (pcase sym
    ('count   'shadow)
    ('time    'font-lock-constant-face)
    ('local   'success)
    ('remote  'warning)
    ('root    'error)
    ('dir     'eshell-ls-directory)
    ('symbol  'font-lock-keyword-face)
    (_        'default)))

(defun my/eshell--prompt ()
  (let* ((dir default-directory)
         (remote (my/eshell--remote-parts dir))
         (time (format-time-string "%H:%M"))
         (user (or (plist-get remote :user) (user-login-name)))
         (host (or (plist-get remote :host) (system-name)))
         (method (plist-get remote :method))
         (localname (if remote (plist-get remote :local) (abbreviate-file-name (eshell/pwd))))
         (dir-disp (my/eshell--shorten (abbreviate-file-name localname)))
         (is-root (string= (or (plist-get remote :user) (user-login-name)) "root"))
         (prompt-char (if is-root "#" ">"))
         (uh (if remote
                 (format "[%s] %s@%s" method user host)
               (format "%s@%s" user host)))
         (uh-face (cond (is-root (my/eshell--face 'root))
                        (remote (my/eshell--face 'remote))
                        (t      (my/eshell--face 'local)))))
    (concat
     ;; Line 1
     (propertize (format "%3d" eshell-last-command-status) 'face (my/eshell--face 'count))
     " "
     (propertize time 'face (my/eshell--face 'time))
     " "
     (propertize uh   'face uh-face)
     " "
     (propertize dir-disp 'face (my/eshell--face 'dir))
     "\n"
     ;; Line 2: actual prompt
     (propertize (format " %s " prompt-char) 'face (my/eshell--face 'symbol)))))

(setq eshell-prompt-function #'my/eshell--prompt)
(setq eshell-highlight-prompt t)

;; Because our prompt starts at the beginning of the second line with a space,
;; keep the regexp simple and robust (properties are ignored).
(setq eshell-prompt-regexp "^ [#>] ")

;; Optional: don't let eshell add extra newlines around prompts
(setq eshell-echo-input nil)



(provide 'rvb-terminals)
