;;; inkpot-theme.el --- conversion of Vim's inkpot colorscheme with direct hex colors

;; Direct hex color definitions extracted from the correct inkpot color mappings
;; These can now be fine-tuned for warmer tints and less contrast

(deftheme inkpot
  "Exact conversion of Vim's inkpot colorscheme with direct hex colors for customization.")

;; Color definitions - using correct mappings from your 88-color file
;; You can now modify these hex values directly for warmer tints and less contrast
(let ((class '((class color) (min-colors 89)))
      ;; Core colors used in the theme (correctly mapped)
      (bg-primary "#000000")        ; inkpot color 0 → line 1
      (fg-primary "#ffffff")        ; inkpot color 79 → line 80
      (bg-secondary "#262626")      ; inkpot color 81 → line 82
      (fg-secondary "#808080")      ; inkpot color 84 → line 85
      (bg-highlight "#5f5faf")      ; inkpot color 38 → line 39
      (cursor-color "#dadada")      ; inkpot color 87 → line 88
      (fringe-color "#5f5fff")      ; inkpot color 39 → line 40
      
      ;; Syntax highlighting colors (correctly mapped)
      (keyword-color "#00afff")     ; inkpot color 27 → line 28
      (function-color "#af5f5f")    ; inkpot color 53 → line 54
      (string-color "#ffaf5f")      ; inkpot color 73 → line 74
      (string-bg "#262626")         ; inkpot color 81 → line 82 (string background)
      (type-color "#00af5f")        ; inkpot color 25 → line 26
      (number-color "#ff5f5f")      ; inkpot color 69 → line 70
      (search-bg "#ffaf5f")         ; inkpot color 73 → line 74
      (search-lazy-bg "#af5f00")    ; inkpot color 52 → line 53
      
      ;; Git/Magit colors (correctly mapped)
      (git-branch-local "#00afff")  ; inkpot color 27 → line 28
      (git-branch-remote "#af5fff") ; inkpot color 55 → line 56
      (git-added "#00af5f")         ; inkpot color 25 → line 26
      (git-removed "#ff5f5f")       ; inkpot color 69 → line 70
      (git-context-bg "#080808")    ; inkpot color 80 → line 81
      
      ;; UI colors (correctly mapped)
      (minibuffer-prompt "#af5f00") ; inkpot color 52 → line 53
      (project-dir "#ffafff")       ; inkpot color 75 → line 76
      (success-color "#00af5f")     ; inkpot color 25 → line 26
      (paren-match-bg "#00ffff")    ; inkpot color 14 → line 15
      
      ;; Terminal colors
      (term-red "#ff0000")
      (term-red-bright "#ff5f5f")
      (term-green "#00af5f")
      (term-green-bright "#00ff5f")
      (term-yellow "#f0a050")       ; custom color from original
      (term-yellow-bright "#ffaf5f")
      (term-blue "#00afff")
      (term-blue-bright "#5fcfff")  ; custom color from original
      (term-magenta "#af5f5f")
      (term-magenta-bright "#af5faf")
      (term-cyan "#5fffaf")
      (term-cyan-bright "#afffaf")
      (term-black "#000000")
      (term-gray "#595959")         ; custom color from original
      (term-white "#ffffff")
      
      ;; Additional specific colors (correctly mapped)
      (line-number-color "#262626") ; inkpot color 81 → line 82
      (line-number-current "#9e9e9e") ; inkpot color 84 → line 85
      (dired-flagged-color "#ff5f00") ; inkpot color 68 → line 69
      )

  (custom-theme-set-faces
   'inkpot

   `(highlight-numbers-number ((,class (:foreground ,number-color))))

   ;; Basic faces
   `(default ((,class (:foreground ,fg-primary :background ,bg-primary))))
   `(cursor ((,class (:background ,cursor-color))))
   `(region ((,class (:foreground ,fg-primary :background ,bg-highlight))))
   `(highlight ((,class (:foreground ,fg-primary :background ,bg-highlight))))
   `(hl-line ((,class (:background ,bg-secondary))))
   `(fringe ((,class (:foreground ,fringe-color :background ,bg-primary))))
   `(vertical-border ((,class (:foreground ,fg-secondary :background ,bg-secondary))))

   ;; Font lock - syntax highlighting
   `(font-lock-function-call-face ((,class (:foreground ,function-color))))
   `(font-lock-builtin-face ((,class (:foreground ,keyword-color))))
   `(font-lock-comment-face ((,class (:foreground ,fg-secondary :slant italic))))
   `(font-lock-constant-face ((,class (:inherit default))))
   `(font-lock-function-name-face ((,class (:foreground ,function-color))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword-color))))
   `(font-lock-string-face ((,class (:foreground ,string-color :background ,string-bg))))
   `(font-lock-type-face ((,class (:foreground ,type-color))))
   `(font-lock-variable-name-face ((,class (:inherit default))))
   `(font-lock-doc-face ((,class (:foreground ,string-color :background ,string-bg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,type-color))))

   ;; Mode line
   `(mode-line ((,class (:foreground ,fg-primary :background ,bg-secondary :weight bold))))
   `(mode-line-active ((,class (:inherit mode-line))))

   `(mode-line-inactive ((,class (:foreground ,fg-secondary :background ,bg-secondary))))
   `(mode-line-buffer-id ((,class (:foreground ,fg-primary :weight bold))))

   `(doom-modeline-project-dir ((,class (:foreground ,project-dir))))

   ;; Eat terminal colors
   `(eat-term-color-0  ((,class (:foreground ,term-black))))
   `(eat-term-color-8  ((,class (:foreground ,term-gray))))
   `(eat-term-color-1  ((,class (:foreground ,term-red))))
   `(eat-term-color-9  ((,class (:foreground ,term-red-bright))))
   `(eat-term-color-2  ((,class (:foreground ,term-green))))
   `(eat-term-color-10 ((,class (:foreground ,term-green-bright))))
   `(eat-term-color-3  ((,class (:foreground ,term-yellow))))
   `(eat-term-color-11 ((,class (:foreground ,term-yellow-bright))))
   `(eat-term-color-4  ((,class (:foreground ,term-blue))))
   `(eat-term-color-12 ((,class (:foreground ,term-blue-bright))))
   `(eat-term-color-5  ((,class (:foreground ,term-magenta))))
   `(eat-term-color-13 ((,class (:foreground ,term-magenta-bright))))
   `(eat-term-color-6  ((,class (:foreground ,term-cyan))))
   `(eat-term-color-14 ((,class (:foreground ,term-cyan-bright))))
   `(eat-term-color-7  ((,class (:foreground ,term-white))))
   `(eat-term-color-15 ((,class (:foreground ,term-white))))
   
   ;; Minibuffer
   `(minibuffer-prompt ((,class (:foreground ,minibuffer-prompt :weight bold))))

   ;; Consult
   `(consult-file ((,class (:inherit default))))

   ;; Search
   `(isearch ((,class (:foreground ,bg-primary :background ,search-bg :weight bold))))
   `(lazy-highlight ((,class (:foreground ,bg-primary :background ,search-lazy-bg))))
   `(match ((,class (:background ,search-lazy-bg))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,line-number-color :background ,bg-primary))))
   `(line-number-current-line ((,class (:foreground ,line-number-current :background ,bg-primary :weight bold))))

   ;; Magit
   `(magit-branch-local ((,class (:foreground ,git-branch-local))))
   `(magit-branch-remote ((,class (:foreground ,git-branch-remote))))
   `(magit-section-heading ((,class (:foreground ,git-branch-local))))
   `(magit-diff-added ((,class (:foreground ,git-added))))
   `(magit-diff-added-highlight ((,class (:inherit magit-diff-added))))
   `(magit-diff-removed ((,class (:foreground ,git-removed))))
   `(magit-diff-removed-highlight ((,class (:inherit magit-diff-removed))))
   `(magit-diff-context ((,class (:inherit default))))
   `(magit-diff-context-highlight ((,class (:foreground ,fg-primary :background ,git-context-bg))))
   `(git-commit-summary ((,class (:inherit default))))

   ;; Dired and Eshell
   `(dired-directory ((,class (:foreground ,git-branch-local))))
   `(eshell-ls-directory ((,class (:foreground ,git-branch-local))))
   `(dired-header ((,class (:foreground ,minibuffer-prompt :weight bold))))
   `(dired-flagged ((,class (:foreground ,dired-flagged-color))))
   `(dired-marked ((,class (:foreground ,git-branch-remote))))

   ;; Success face (for eshell prompt)
   `(success ((,class (:foreground ,success-color))))

   ;; Compilation
   `(compilation-error ((,class (:foreground ,fg-primary))))
   `(compilation-warning ((,class (:foreground ,fg-primary))))
   `(compilation-info ((,class (:foreground ,fg-primary))))

   `(show-paren-match ((,class (:background ,paren-match-bg :foreground ,fg-primary))))
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'inkpot)

;;; inkpot-theme.el ends here
