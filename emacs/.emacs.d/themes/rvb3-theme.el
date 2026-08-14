;;; rvb3-theme.el --- Black, white, and one red  -*- lexical-binding: t; -*-

;;; Commentary:
;; A near-monochrome light theme laid out the way a printed page is:
;; black ink on warm paper, three greys for everything secondary, and a
;; single spot red.
;;
;; The red is a second ink, not a syntax colour.  Printing one costs a
;; whole extra pass, so it is spent only where a page would spend it:
;; the cursor, the search match you are on, a document's title, a TODO,
;; and things that are actually wrong.  Nothing routine is red, which
;; is what keeps it meaning something.
;;
;; Everything else is distinguished the way type is -- weight, slant
;; and grey value rather than hue.  Comments recede into grey, strings
;; sit a shade off the body text, keywords hold the line in bold.

;;; Code:

(deftheme rvb3
  "Black and white with a single spot red, laid out like a printed page.")

(let ((class '((class color) (min-colors 89)))
      (paper     "#ffffff")   ; the page
      (wash      "#f1efe9")   ; a tint of it: blocks, current line, popups
      (wash-deep "#e6e3da")   ; selection
      (rule      "#dcd8ce")   ; hairlines: borders, fringes, line numbers
      (grey      "#8c887c")   ; secondary text
      (soft      "#54514a")   ; text a shade off the body
      (ink       "#17150f")   ; the body
      (red       "#c8102e")   ; the spot colour
      (red-deep  "#8f0b20")   ; the spot on a tint
      (red-wash  "#f8e6e5"))  ; the spot as a tint
  (custom-theme-set-faces
   'rvb3

   ;; The page.
   `(default ((,class (:foreground ,ink :background ,paper))))
   ;; A red cursor: one mark on the page, always where you are looking.
   `(cursor ((,class (:background ,red))))
   `(fringe ((,class (:foreground ,rule :background ,paper))))
   `(region ((,class (:background ,wash-deep :distant-foreground ,ink :extend t))))
   `(secondary-selection ((,class (:background ,wash :extend t))))
   `(highlight ((,class (:background ,wash))))
   `(hl-line ((,class (:background ,wash))))
   `(vertical-border ((,class (:foreground ,rule))))
   `(window-divider ((,class (:foreground ,rule))))
   `(window-divider-first-pixel ((,class (:foreground ,rule))))
   `(window-divider-last-pixel ((,class (:foreground ,rule))))
   `(shadow ((,class (:foreground ,grey))))
   `(line-number ((,class (:foreground ,rule :background ,paper))))
   `(line-number-current-line ((,class (:foreground ,grey :background ,paper))))
   ;; Ruled off rather than boxed in: the mode line is the same paper,
   ;; separated by a hairline.
   `(mode-line ((,class (:foreground ,ink :background ,paper
                                     :box nil :overline ,rule))))
   `(mode-line-active ((,class (:inherit mode-line))))
   `(mode-line-inactive ((,class (:foreground ,grey :background ,paper
                                              :box nil :overline ,rule))))
   `(mode-line-buffer-id ((,class (:foreground ,ink :weight bold))))
   `(header-line ((,class (:foreground ,ink :background ,paper
                                       :box nil :underline ,rule))))
   `(minibuffer-prompt ((,class (:foreground ,ink :weight bold))))
   `(tooltip ((,class (:foreground ,ink :background ,wash))))

   ;; Syntax, by weight and grey value.
   `(font-lock-comment-face ((,class (:foreground ,grey :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face ((,class (:foreground ,grey :slant italic))))
   `(font-lock-string-face ((,class (:foreground ,soft))))
   `(font-lock-regexp-face ((,class (:foreground ,soft))))
   `(font-lock-keyword-face ((,class (:foreground ,ink :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,ink :weight bold))))
   `(font-lock-function-call-face ((,class (:inherit default))))
   `(font-lock-type-face ((,class (:foreground ,ink :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,soft :weight bold))))
   `(font-lock-number-face ((,class (:foreground ,soft))))
   `(font-lock-builtin-face ((,class (:foreground ,soft :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,grey :weight bold))))
   `(font-lock-variable-name-face ((,class (:inherit default))))
   `(font-lock-variable-use-face ((,class (:inherit default))))
   `(font-lock-property-name-face ((,class (:foreground ,soft))))
   `(font-lock-property-use-face ((,class (:foreground ,soft))))
   `(font-lock-escape-face ((,class (:foreground ,grey))))
   `(font-lock-operator-face ((,class (:inherit default))))
   `(font-lock-negation-char-face ((,class (:foreground ,grey))))
   `(font-lock-bracket-face ((,class (:foreground ,grey))))
   `(font-lock-delimiter-face ((,class (:foreground ,grey))))
   `(font-lock-punctuation-face ((,class (:foreground ,grey))))
   `(font-lock-misc-punctuation-face ((,class (:inherit font-lock-punctuation-face))))
   ;; Wrong is one of the few things worth the second ink.
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))

   ;; Search: the match you are on is red, the rest are marked in grey.
   `(isearch ((,class (:foreground ,paper :background ,red :weight bold))))
   `(isearch-fail ((,class (:foreground ,red :background ,red-wash))))
   `(lazy-highlight ((,class (:foreground ,ink :background ,wash-deep))))
   `(match ((,class (:foreground ,ink :background ,wash-deep))))
   `(show-paren-match ((,class (:foreground ,ink :background ,wash-deep :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,paper :background ,red :weight bold))))
   ;; Underlined, the way a printed reference is.
   `(link ((,class (:foreground ,ink :underline t))))
   `(link-visited ((,class (:foreground ,soft :underline t))))
   `(button ((,class (:foreground ,ink :underline t))))
   `(custom-button ((,class (:foreground ,ink :background ,wash
                                         :box (:line-width 1 :color ,rule)))))
   `(custom-button-mouse ((,class (:foreground ,ink :background ,wash-deep
                                               :box (:line-width 1 :color ,grey)))))

   ;; Diagnostics.
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,ink :weight bold))))
   `(success ((,class (:foreground ,ink))))
   `(flymake-error ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,grey)))))
   `(flymake-note ((,class (:underline (:style wave :color ,rule)))))
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,grey)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,rule)))))
   `(compilation-error ((,class (:foreground ,red :weight bold))))
   `(compilation-warning ((,class (:foreground ,ink :weight bold))))
   `(compilation-info ((,class (:foreground ,soft))))
   `(eglot-highlight-symbol-face ((,class (:background ,wash :weight bold))))
   `(eglot-mode-line ((,class (:foreground ,ink))))
   `(eglot-inlay-hint-face ((,class (:foreground ,grey :height 0.8))))
   `(eglot-type-hint-face ((,class (:inherit eglot-inlay-hint-face :slant italic))))
   `(eglot-parameter-hint-face ((,class (:inherit eglot-inlay-hint-face))))

   ;; Completion.
   `(company-tooltip ((,class (:foreground ,ink :background ,wash))))
   `(company-tooltip-selection ((,class (:foreground ,ink :background ,wash-deep))))
   `(company-tooltip-common ((,class (:foreground ,ink :weight bold))))
   `(vertico-current ((,class (:background ,wash-deep :extend t))))
   `(completions-common-part ((,class (:foreground ,ink :weight bold))))
   ;; Matched fragments differ by weight and slant, not by hue.
   `(orderless-match-face-0 ((,class (:foreground ,ink :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,ink :weight bold :slant italic))))
   `(orderless-match-face-2 ((,class (:foreground ,soft :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,soft :weight bold :slant italic))))
   `(marginalia-documentation ((,class (:foreground ,grey :slant italic))))

   ;; Files, diffs, and Magit.  A deletion is the one diff worth the
   ;; second ink; an addition reads as a tint, the way a paste-up does.
   `(dired-directory ((,class (:foreground ,ink :weight bold))))
   `(dired-header ((,class (:foreground ,ink :weight bold :underline ,rule))))
   `(dired-flagged ((,class (:foreground ,red :weight bold))))
   `(dired-marked ((,class (:foreground ,ink :weight bold))))
   `(diff-added ((,class (:foreground ,ink :background ,wash :extend t))))
   `(diff-removed ((,class (:foreground ,red-deep :background ,red-wash :extend t))))
   `(diff-changed ((,class (:foreground ,ink :background ,wash :extend t))))
   `(diff-refine-added ((,class (:foreground ,ink :background ,wash-deep :weight bold))))
   `(diff-refine-removed ((,class (:foreground ,red-deep :background ,red-wash :weight bold))))
   `(diff-context ((,class (:foreground ,soft))))
   `(diff-header ((,class (:foreground ,ink :weight bold))))
   `(diff-file-header ((,class (:foreground ,ink :weight bold :underline ,rule))))
   `(diff-hunk-header ((,class (:foreground ,grey))))
   `(diff-hl-insert ((,class (:foreground ,grey :background ,paper))))
   `(diff-hl-delete ((,class (:foreground ,red :background ,paper))))
   `(diff-hl-change ((,class (:foreground ,soft :background ,paper))))
   `(magit-branch-local ((,class (:foreground ,ink :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,soft :slant italic))))
   `(magit-branch-current ((,class (:foreground ,ink :weight bold :box (:line-width 1 :color ,rule)))))
   `(magit-section-heading ((,class (:foreground ,ink :weight bold))))
   `(magit-section-highlight ((,class (:background ,wash :extend t))))
   `(magit-section-heading-selection ((,class (:foreground ,red))))
   `(magit-diff-added ((,class (:foreground ,ink :background ,wash :extend t))))
   `(magit-diff-added-highlight ((,class (:foreground ,ink :background ,wash-deep :extend t))))
   `(magit-diff-removed ((,class (:foreground ,red-deep :background ,red-wash :extend t))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red-deep :background ,red-wash :weight bold :extend t))))
   `(magit-diff-context ((,class (:foreground ,soft :extend t))))
   `(magit-diff-context-highlight ((,class (:foreground ,soft :background ,wash :extend t))))
   `(magit-diff-hunk-heading ((,class (:foreground ,grey :background ,paper :extend t))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,ink :background ,wash :extend t))))
   `(magit-diff-file-heading ((,class (:foreground ,ink :weight bold))))
   `(magit-hash ((,class (:foreground ,grey))))
   `(magit-dimmed ((,class (:foreground ,grey))))
   `(magit-log-author ((,class (:foreground ,soft))))
   `(magit-log-date ((,class (:foreground ,grey))))
   `(magit-tag ((,class (:foreground ,ink :slant italic))))

   ;; Org.  Rubrication: the title takes the red, once per document,
   ;; the way a printed one would.  Headings are unbolded, so a rule
   ;; under each one carries the weight instead -- the levels then read
   ;; as grey values against a line rather than as sizes.
   `(org-document-title ((,class (:foreground ,red :weight normal))))
   `(org-document-info ((,class (:foreground ,soft))))
   `(org-document-info-keyword ((,class (:foreground ,rule))))
   `(org-level-1 ((,class (:foreground ,ink :weight normal :underline t))))
   `(org-level-2 ((,class (:foreground ,ink :weight normal :underline nil))))
   `(org-level-3 ((,class (:foreground ,soft :weight normal :underline nil))))
   `(org-level-4 ((,class (:foreground ,soft :weight normal :underline nil))))
   `(org-level-5 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-level-6 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-level-7 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-level-8 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,grey :weight normal))))
   `(org-headline-done ((,class (:foreground ,grey))))
   `(org-checkbox ((,class (:foreground ,ink :weight bold))))
   `(org-block ((,class (:background ,wash :extend t))))
   `(org-block-begin-line ((,class (:foreground ,grey :background ,wash :slant italic :extend t))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))
   `(org-code ((,class (:foreground ,soft))))
   `(org-verbatim ((,class (:foreground ,soft))))
   `(org-quote ((,class (:foreground ,soft :slant italic :extend t))))
   `(org-table ((,class (:foreground ,soft))))
   `(org-date ((,class (:foreground ,grey :underline t))))
   `(org-tag ((,class (:foreground ,grey :weight normal))))
   `(org-special-keyword ((,class (:foreground ,rule))))
   `(org-drawer ((,class (:foreground ,rule))))
   `(org-meta-line ((,class (:foreground ,rule))))
   `(org-ellipsis ((,class (:foreground ,grey :underline nil))))
   `(org-link ((,class (:foreground ,ink :underline t))))
   `(org-footnote ((,class (:foreground ,grey :underline t))))
   `(org-modern-label ((,class (:foreground ,ink :background ,wash))))

   ;; Markdown keeps its heading sizes, since nothing else marks them.
   `(markdown-header-face-1 ((,class (:foreground ,ink :weight bold :height 1.3))))
   `(markdown-header-face-2 ((,class (:foreground ,ink :weight bold :height 1.2))))
   `(markdown-header-face-3 ((,class (:foreground ,soft :weight bold :height 1.1))))
   `(markdown-code-face ((,class (:background ,wash :extend t))))
   `(markdown-inline-code-face ((,class (:foreground ,soft :background ,wash))))
   `(markdown-blockquote-face ((,class (:foreground ,soft :slant italic))))
   `(markdown-link-face ((,class (:foreground ,ink :underline t))))
   `(markdown-url-face ((,class (:foreground ,grey))))

   ;; Sh
   `(sh-heredoc ((,class (:foreground ,soft))))
   `(sh-quoted-exec ((,class (:foreground ,soft))))

   ;; Tabs and the rest.
   `(tab-bar ((,class (:foreground ,grey :background ,paper))))
   `(tab-bar-tab ((,class (:foreground ,ink :background ,paper :weight bold
                                       :box (:line-width 1 :color ,rule)))))
   `(tab-bar-tab-inactive ((,class (:foreground ,grey :background ,paper))))
   `(tab-line ((,class (:foreground ,grey :background ,paper))))
   `(tab-line-tab-current ((,class (:foreground ,ink :background ,paper :weight bold))))
   `(tab-line-tab-inactive ((,class (:foreground ,grey :background ,paper))))
   `(widget-field ((,class (:background ,wash :box (:line-width 1 :color ,rule)))))
   `(trailing-whitespace ((,class (:background ,red-wash))))
   `(whitespace-trailing ((,class (:background ,red-wash))))
   `(fill-column-indicator ((,class (:foreground ,rule))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rvb3)

;;; rvb3-theme.el ends here
