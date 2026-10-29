;; (use-package eat
;;   :ensure t
;;   )

(use-package vterm
  :ensure t
  :defer t
  :init
  (setq vterm-max-scrollback 100000)
  ;; Vterm renders RGB colors, but GUI Emacs often starts without the
  ;; terminal's COLORTERM variable.  Codex and Claude use it to decide
  ;; whether to send the same full-color UI they send to WezTerm/Ghostty.
  ;; Claude's fullscreen renderer bypasses vterm's scrollback.
  (setq vterm-environment
        (append '("COLORTERM=truecolor"
                  "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1")
                (seq-remove
                 (lambda (entry)
                   (or (string-prefix-p "COLORTERM=" entry)
                       (string-prefix-p "CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=" entry)))
                 (and (boundp 'vterm-environment) vterm-environment))))
  :config
  (advice-add 'vterm--filter :before #'rvb/vterm-answer-color-queries)
  )

(defvar-local rvb/vterm-color-query-tail ""
  "End of the preceding vterm output chunk, for split OSC queries.")

(defconst rvb/vterm-color-query-regexp
  (rx "\e]1" (or "0" "1") ";?" (or "\a" "\e\\"))
  "OSC 10 and 11 queries, terminated by BEL or ST.")

(defun rvb/vterm-color-values (color frame)
  "Return 16-bit RGB values for COLOR on FRAME without palette rounding."
  (if (and (stringp color)
           (string-match-p (rx string-start "#" (= 6 hex) string-end) color))
      (mapcar (lambda (start)
                (* 257 (string-to-number (substring color start (+ start 2)) 16)))
              '(1 3 5))
    (color-values color frame)))

(defun rvb/vterm-answer-color-queries (process input)
  "Answer foreground and background color queries from PROCESS's INPUT."
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((prior-length (length rvb/vterm-color-query-tail))
               (combined (concat rvb/vterm-color-query-tail input))
               (window (get-buffer-window buffer t))
               (frame (and window (window-frame window)))
               (start 0))
          (while (string-match rvb/vterm-color-query-regexp combined start)
            (let ((end (match-end 0))
                  (slot (aref combined (+ (match-beginning 0) 3))))
              (when (> end prior-length)
                (when-let* ((color (if (eq slot ?0)
                                       (face-foreground 'default frame t)
                                     (face-background 'default frame t)))
                            (rgb (rvb/vterm-color-values color frame)))
                  (process-send-string
                   process
                   (format "\e]1%c;rgb:%04x/%04x/%04x\e\\"
                           slot (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))))
              (setq start end)))
          (setq rvb/vterm-color-query-tail
                (substring combined (max 0 (- (length combined) 16)))))))))

(use-package multi-vterm
  :ensure t
  :defer t
  :init
  )

;; https://mocompute.codeberg.page/item/2024/2024-09-03-emacs-project-vterm.html
(defun my-project-shell ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (if (comint-check-proc shell-buffer)
            (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
          (vterm shell-buffer))
      (vterm (generate-new-buffer-name default-project-shell-name)))))

(advice-add 'project-shell :override #'my-project-shell)

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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
    ('status  'shadow)
    ('time    'default)
    ('local   'success)
    ('remote  'warning)
    ('root    'error)
    ('dir     'eshell-ls-directory)
    ('symbol  'default)
    ('prompt  'shadow)
    ('pipe    'shadow)
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
     (propertize "┌─" 'face (my/eshell--face 'pipe))
     (propertize (format "%3d" eshell-last-command-status) 'face (my/eshell--face 'status))
     " "
     (propertize time 'face (my/eshell--face 'time))
     " "
     (propertize uh   'face uh-face)
     " "
     (propertize dir-disp 'face (my/eshell--face 'dir))
     "\n"
     ;; Line 2: actual prompt with curved pipe connection
     (propertize "└─" 'face (my/eshell--face 'pipe))
     (propertize prompt-char 'face (my/eshell--face 'prompt))
     " ")))

(setq eshell-prompt-function #'my/eshell--prompt)
(setq eshell-highlight-prompt t)

;; Because our prompt now starts with curved pipe + prompt char + space,
;; keep the regexp simple and robust (properties are ignored).
(setq eshell-prompt-regexp "^└─[#>] ")

;; Optional: don't let eshell add extra newlines around prompts
(setq eshell-echo-input nil)

;; Aweshell - a better eshell
;; (use-package aweshell
;;   :vc (:url "https://github.com/manateelazycat/aweshell.git"
;; 	    :rev :newest
;; 	    :branch "master")
;;   :config
;;   (setq eshell-prompt-function #'my/eshell--prompt)
;;   (setq eshell-highlight-prompt nil)

;;   ;; Because our prompt now starts with curved pipe + prompt char + space,
;;   ;; keep the regexp simple and robust (properties are ignored).
;;   (setq eshell-prompt-regexp "^└─[#>] ")
;;   )

;;; Mana Terminal Launcher
(defcustom rvb/mana-terminal-backend 'vterm
  "Terminal backend to use for launching mana.
Valid values are 'term, 'ansi-term, 'vterm"
  :type '(choice (const :tag "term" term)
                 (const :tag "ansi-term" ansi-term)
                 (const :tag "vterm" vterm))
  :group 'rvb-terminals)

(defcustom rvb/mana-executable "mana"
  "Path to the mana executable."
  :type 'string
  :group 'rvb-terminals)

(defun rvb/launch-mana ()
  "Launch mana in the configured terminal backend."
  (interactive)
  (pcase rvb/mana-terminal-backend
    ('term
     (let ((term-buffer (make-term "mana" rvb/mana-executable)))
       (set-buffer term-buffer)
       (term-mode)
       (term-char-mode)
       (switch-to-buffer term-buffer)))

    ('ansi-term
     (ansi-term rvb/mana-executable "mana"))

    ('vterm
     (if (require 'vterm nil t)
         (let ((vterm-shell rvb/mana-executable))
           (vterm "*mana*"))
       (user-error "vterm is not available. Please install it or choose a different backend")))

    (_
     (user-error "Unknown terminal backend: %s" rvb/mana-terminal-backend))))

(provide 'rvb-terminals)
