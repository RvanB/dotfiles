;;; inkpot-theme.el --- conversion of Vim's inkpot colorscheme

;; Implementation of the original Vim <SID>M and <SID>X functions
(defun inkpot--m (a)
  "Map a urxvt cube number to an xterm-256 cube number."
  (nth a '(0 1 3 5)))

(defun inkpot--x (a)
  "Map a urxvt colour to an xterm-256 colour."
  (cond
   ((= a 8) 237)
   ((< a 16) a)
   ((> a 79) (+ 232 (* 3 (- a 80))))
   (t
    (let* ((b (- a 16))
           (x (% b 4))
           (y (% (/ b 4) 4))
           (z (/ b 16)))
      (+ 16 
         (inkpot--m x) 
         (* 6 (inkpot--m y)) 
         (* 36 (inkpot--m z)))))))

;; xterm-256 color palette as hex values
(defconst inkpot--xterm-colors
  [;; 0-15: standard colors
   "#000000" "#800000" "#008000" "#808000" "#000080" "#800080" "#008080" "#c0c0c0"
   "#808080" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#ff00ff" "#00ffff" "#ffffff"
   ;; 16-231: 6x6x6 color cube
   "#000000" "#00005f" "#000087" "#0000af" "#0000d7" "#0000ff"
   "#005f00" "#005f5f" "#005f87" "#005faf" "#005fd7" "#005fff"
   "#008700" "#00875f" "#008787" "#0087af" "#0087d7" "#0087ff"
   "#00af00" "#00af5f" "#00af87" "#00afaf" "#00afd7" "#00afff"
   "#00d700" "#00d75f" "#00d787" "#00d7af" "#00d7d7" "#00d7ff"
   "#00ff00" "#00ff5f" "#00ff87" "#00ffaf" "#00ffd7" "#00ffff"
   "#5f0000" "#5f005f" "#5f0087" "#5f00af" "#5f00d7" "#5f00ff"
   "#5f5f00" "#5f5f5f" "#5f5f87" "#5f5faf" "#5f5fd7" "#5f5fff"
   "#5f8700" "#5f875f" "#5f8787" "#5f87af" "#5f87d7" "#5f87ff"
   "#5faf00" "#5faf5f" "#5faf87" "#5fafaf" "#5fafd7" "#5fafff"
   "#5fd700" "#5fd75f" "#5fd787" "#5fd7af" "#5fd7d7" "#5fd7ff"
   "#5fff00" "#5fff5f" "#5fff87" "#5fffaf" "#5fffd7" "#5fffff"
   "#870000" "#87005f" "#870087" "#8700af" "#8700d7" "#8700ff"
   "#875f00" "#875f5f" "#875f87" "#875faf" "#875fd7" "#875fff"
   "#878700" "#87875f" "#878787" "#8787af" "#8787d7" "#8787ff"
   "#87af00" "#87af5f" "#87af87" "#87afaf" "#87afd7" "#87afff"
   "#87d700" "#87d75f" "#87d787" "#87d7af" "#87d7d7" "#87d7ff"
   "#87ff00" "#87ff5f" "#87ff87" "#87ffaf" "#87ffd7" "#87ffff"
   "#af0000" "#af005f" "#af0087" "#af00af" "#af00d7" "#af00ff"
   "#af5f00" "#af5f5f" "#af5f87" "#af5faf" "#af5fd7" "#af5fff"
   "#af8700" "#af875f" "#af8787" "#af87af" "#af87d7" "#af87ff"
   "#afaf00" "#afaf5f" "#afaf87" "#afafaf" "#afafd7" "#afafff"
   "#afd700" "#afd75f" "#afd787" "#afd7af" "#afd7d7" "#afd7ff"
   "#afff00" "#afff5f" "#afff87" "#afffaf" "#afffd7" "#afffff"
   "#d70000" "#d7005f" "#d70087" "#d700af" "#d700d7" "#d700ff"
   "#d75f00" "#d75f5f" "#d75f87" "#d75faf" "#d75fd7" "#d75fff"
   "#d78700" "#d7875f" "#d78787" "#d787af" "#d787d7" "#d787ff"
   "#d7af00" "#d7af5f" "#d7af87" "#d7afaf" "#d7afd7" "#d7afff"
   "#d7d700" "#d7d75f" "#d7d787" "#d7d7af" "#d7d7d7" "#d7d7ff"
   "#d7ff00" "#d7ff5f" "#d7ff87" "#d7ffaf" "#d7ffd7" "#d7ffff"
   "#ff0000" "#ff005f" "#ff0087" "#ff00af" "#ff00d7" "#ff00ff"
   "#ff5f00" "#ff5f5f" "#ff5f87" "#ff5faf" "#ff5fd7" "#ff5fff"
   "#ff8700" "#ff875f" "#ff8787" "#ff87af" "#ff87d7" "#ff87ff"
   "#ffaf00" "#ffaf5f" "#ffaf87" "#ffafaf" "#ffafd7" "#ffafff"
   "#ffd700" "#ffd75f" "#ffd787" "#ffd7af" "#ffd7d7" "#ffd7ff"
   "#ffff00" "#ffff5f" "#ffff87" "#ffffaf" "#ffffd7" "#ffffff"
   ;; 232-255: grayscale ramp
   "#080808" "#121212" "#1c1c1c" "#262626" "#303030" "#3a3a3a"
   "#444444" "#4e4e4e" "#585858" "#606060" "#666666" "#767676"
   "#808080" "#8a8a8a" "#949494" "#9e9e9e" "#a8a8a8" "#b2b2b2"
   "#bcbcbc" "#c6c6c6" "#d0d0d0" "#dadada" "#e4e4e4" "#eeeeee"])

(defun inkpot-display-get-color-hex ()
  "Display the hex color strings returned by `inkpot--get-color` for indices 0 to 87."
  (interactive)
  (let ((buffer (get-buffer-create "*Inkpot Get-Color Hex*")))
    (with-current-buffer buffer
      (erase-buffer)
      (dotimes (i 88)
        (insert (format "%s\n" (inkpot--get-color i))))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))


(defun inkpot--get-color (vim-color)
  "Get hex color for a Vim color number using the <SID>X function."
  (aref inkpot--xterm-colors (inkpot--x vim-color)))

(deftheme inkpot
  "Exact conversion of Vim's inkpot colorscheme.")

;; Colors computed using the actual <SID>X function
(let ((class '((class color) (min-colors 89))))

  (custom-theme-set-faces
   'inkpot

   `(highlight-numbers-number ((,class (:foreground ,(inkpot--get-color 69)))))

   ;; Basic faces - using computed colors from <SID>X function
   `(default ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 0)))))
   `(cursor ((,class (:background ,(inkpot--get-color 87)))))
   `(region ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 38)))))
   `(highlight ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 38)))))
   `(hl-line ((,class (:background ,(inkpot--get-color 81)))))
   `(fringe ((,class (:foreground ,(inkpot--get-color 39) :background ,(inkpot--get-color 0)))))
   `(vertical-border ((,class (:foreground ,(inkpot--get-color 84) :background ,(inkpot--get-color 81)))))

   ;; Font lock - exact syntax highlighting from original
   `(font-lock-function-call-face ((,class (:inherit default))))
   `(font-lock-builtin-face ((,class (:foreground ,(inkpot--get-color 27)))))
   `(font-lock-comment-face ((,class (:foreground ,(inkpot--get-color 52) :slant italic))))
   ;; `(font-lock-constant-face ((,class (:foreground ,(inkpot--get-color 25)))))
   `(font-lock-constant-face ((,class (:inherit default))))
   `(font-lock-function-name-face ((,class (:foreground ,(inkpot--get-color 53)))))
   `(font-lock-keyword-face ((,class (:foreground ,(inkpot--get-color 27)))))
   `(font-lock-string-face ((,class (:foreground ,(inkpot--get-color 73) :background ,(inkpot--get-color 81)))))
   `(font-lock-type-face ((,class (:foreground ,(inkpot--get-color 71)))))
   `(font-lock-variable-name-face ((,class (:inherit default))))
   ;; `(font-lock-warning-face ((,class (:foreground ,(inkpot--get-color 16) :background ,(inkpot--get-color 68) :weight bold))))
   `(font-lock-doc-face ((,class (:foreground ,(inkpot--get-color 73) :background ,(inkpot--get-color 81)))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(inkpot--get-color 25)))))

   ;; ;; Mode line - exact colors
   `(mode-line ((,class (:foreground "#bebebe" :background "#5e2a13" :weight bold))))
   `(mode-line-inactive ((,class (:foreground ,(inkpot--get-color 84) :background ,(inkpot--get-color 81)))))
   `(mode-line-buffer-id ((,class (:foreground ,(inkpot--get-color 79) :weight bold))))
   
   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,(inkpot--get-color 52) :weight bold))))

   ;; Search - exact mappings
   `(isearch ((,class (:foreground ,(inkpot--get-color 0) :background ,(inkpot--get-color 73) :weight bold))))
   `(lazy-highlight ((,class (:foreground ,(inkpot--get-color 0) :background ,(inkpot--get-color 52)))))
   `(match ((,class (:background ,(inkpot--get-color 52)))))

   ;; Delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,(inkpot--get-color 55)))))
   `(rainbow-delimiters-depth-2-face ((,class (:inherit default))))
   `(rainbow-delimiters-depth-3-face ((,class (:inherit default))))
   `(rainbow-delimiters-depth-4-face ((,class (:inherit default))))
   `(rainbow-delimiters-depth-5-face ((,class (:inherit default))))
   `(rainbow-delimiters-depth-6-face ((,class (:inherit default))))
   `(rainbow-delimiters-depth-7-face ((,class (:inherit default))))
   `(rainbow-delimiters-depth-8-face ((,class (:inherit default))))
   `(rainbow-delimiters-depth-9-face ((,class (:inherit default))))

   ;; Error messages - exact colors
   ;; `(error ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 32)))))
   ;; `(warning ((,class (:foreground ,(inkpot--get-color 16) :background ,(inkpot--get-color 68) :weight bold))))
   ;; `(success ((,class (:foreground ,(inkpot--get-color 38) :weight bold))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,(inkpot--get-color 39) :background ,(inkpot--get-color 0)))))
   ;; `(line-number-current-line ((,class (:foreground ,(inkpot--get-color 85) :background ,(inkpot--get-color 0) :weight bold))))

   ;; Magit
   `(magit-branch-local ((,class (:foreground ,(inkpot--get-color 27)))))
   `(magit-branch-remote ((,class (:foreground ,(inkpot--get-color 55)))))
   `(magit-section-heading ((,class (:foreground ,(inkpot--get-color 27)))))
   `(magit-diff-added ((,class (:foreground ,(inkpot--get-color 25)))))
   `(magit-diff-added-highlight ((,class (:inherit magit-diff-added))))
   `(magit-diff-removed ((,class (:foreground ,(inkpot--get-color 69)))))
   `(magit-diff-removed-highlight ((,class (:inherit magit-diff-removed))))
   `(magit-diff-context ((,class (:inherit default))))
   `(magit-diff-context-highlight ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 80)))))
   `(git-commit-summary ((,class (:inherit default))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,(inkpot--get-color 27)))))
   `(dired-header ((,class (:foreground ,(inkpot--get-color 52) :weight bold))))
   ;; `(dired-ignored ((,class (:foreground ,(inkpot--get-color 39)))))
   `(dired-flagged ((,class (:foreground ,(inkpot--get-color 48)))))
   `(dired-marked ((,class (:foreground ,(inkpot--get-color 55)))))

   ;; Other
   ;; `(c-annotation-face ((,class (:foreground "#ff0000"))))
   

   ;; Diff mode - exact diff colors from original
   ;; `(diff-added ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 20)))))
   ;; `(diff-removed ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 32)))))
   ;; `(diff-changed ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 17)))))
   ;; `(diff-header ((,class (:foreground ,(inkpot--get-color 79) :background ,(inkpot--get-color 34)))))

   ;; Folding - exact colors  
   ;; `(outline-1 ((,class (:foreground ,(inkpot--get-color 27) :weight bold))))
   ;; `(outline-2 ((,class (:foreground ,(inkpot--get-color 71) :weight bold))))
   ;; `(outline-3 ((,class (:foreground ,(inkpot--get-color 53) :weight bold))))
   ;; `(outline-4 ((,class (:foreground ,(inkpot--get-color 25) :weight bold))))

   ;; ;; Org mode
   ;; `(org-level-1 ((,class (:foreground ,(inkpot--get-color 27) :weight bold))))
   ;; `(org-level-2 ((,class (:foreground ,(inkpot--get-color 71) :weight bold))))
   ;; `(org-level-3 ((,class (:foreground ,(inkpot--get-color 53) :weight bold))))
   ;; `(org-level-4 ((,class (:foreground ,(inkpot--get-color 25) :weight bold))))
   ;; `(org-todo ((,class (:foreground ,(inkpot--get-color 16) :background ,(inkpot--get-color 57) :weight bold))))
   ;; `(org-done ((,class (:foreground ,(inkpot--get-color 28) :weight bold))))
   ;; `(org-link ((,class (:foreground ,(inkpot--get-color 77) :weight bold))))

   ;; Special characters and elements
   ;; `(font-lock-regexp-grouping-backslash ((,class (:foreground ,(inkpot--get-color 55)))))
   ;; `(font-lock-regexp-grouping-construct ((,class (:foreground ,(inkpot--get-color 55)))))

   ;; Compilation
   `(compilation-error ((,class (:foreground ,(inkpot--get-color 79)))))
   `(compilation-warning ((,class (:foreground ,(inkpot--get-color 79)))))
   `(compilation-info ((,class (:foreground ,(inkpot--get-color 79)))))

   `(show-paren-match ((,class (:background ,(inkpot--get-color 14) :foreground ,(inkpot--get-color 79)))))
   ;; `(show-paren-mismatch ((,class (:background ,(inkpot--get-color 48) :foreground ,(inkpot--get-color 16)))))

   ;; Whitespace
   ;; `(whitespace-tab ((,class (:foreground ,(inkpot--get-color 55) :weight bold))))
   ;; `(whitespace-space ((,class (:foreground ,(inkpot--get-color 55) :weight bold))))
   ;; `(whitespace-trailing ((,class (:background ,(inkpot--get-color 32)))))

   ;; Spell checking (Vim 7.0+ colors)
   ;; `(flyspell-incorrect ((,class (:background ,(inkpot--get-color 32)))))
   ;; `(flyspell-duplicate ((,class (:background ,(inkpot--get-color 33)))))

   ;; Visual selection
   ;; `(secondary-selection ((,class (:background ,(inkpot--get-color 38)))))

   ;; Popup menu (completion) - Vim 7.0+ colors
   ;; `(popup-face ((,class (:foreground ,(inkpot--get-color 87) :background ,(inkpot--get-color 82)))))
   ;; `(popup-menu-selection-face ((,class (:foreground ,(inkpot--get-color 87) :background ,(inkpot--get-color 38) :weight bold))))

   ;; Company (if available)
   ;; `(company-tooltip ((,class (:foreground ,(inkpot--get-color 87) :background ,(inkpot--get-color 82)))))
   ;; `(company-tooltip-selection ((,class (:foreground ,(inkpot--get-color 87) :background ,(inkpot--get-color 38) :weight bold))))
   ;; `(company-tooltip-common ((,class (:foreground ,(inkpot--get-color 87) :background ,(inkpot--get-color 39) :weight bold))))
   ;; `(company-scrollbar-bg ((,class (:foreground ,(inkpot--get-color 87) :background ,(inkpot--get-color 39) :weight bold))))
   ;; `(company-scrollbar-fg ((,class (:foreground ,(inkpot--get-color 87) :background ,(inkpot--get-color 39) :weight bold))))
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'inkpot)

;;; inkpot-theme.el ends here
