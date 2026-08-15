;;; rvb3-theme.el --- A one-bit digital page  -*- lexical-binding: t; -*-

;;; Commentary:
;; A near-monochrome light theme laid out the way a printed page is:
;; black ink on pale grey with structural rules and no chromatic colour.  Text
;; itself remains black.  Buffer selection uses a printed dot screen; menus and
;; strong states use solid reversed video.
;;
;; Everything else is distinguished with rules, underlines, and sparing weight
;; rather than slant or grey text.  Keywords and definitions carry the bold;
;; comments and strings remain the same solid ink as the body.

;;; Code:

(defface rvb3-selected-face
  '((t (:inherit default)))
  "Solid reversed video used for selections in the rvb3 theme."
  :group 'faces)

(defface rvb3-stipple-face
  '((t (:inherit default)))
  "Black-on-paper halftone used for in-buffer selections."
  :group 'faces)

(deftheme rvb3
  "A one-bit digital page of ink, paper, screens, and reversed video.")

(let ((class '((class color) (min-colors 89)))
      (graphic '((type graphic) (class color) (min-colors 89)))
      ;; A fully neutral scale gives the page the cool cast of an early digital
      ;; workstation while avoiding the glare of a pure-white background.
      (paper     "#ffffff")   ; the page
      ;; Neutral surfaces stay the colour of the page.  Rules, underlines, and
      ;; reversed video provide separation; there are no solid grey fills.
      (wash      "#ffffff")
      (wash-deep "#ffffff")
      (rule      "#b5b5b5")   ; non-text hairlines and borders
      (ink       "#171717")   ; the body
      (comment   "#666666")   ; comments recede without losing readability
      ;; Keep semantic role names below without weakening their text colour.
      ;; Classic Mac hierarchy comes from surrounding chrome, not pale type.
      (grey      "#171717")
      (soft      "#171717")
      (halftone  '(4 4 "\x01\x04\x01\x04")))
  (custom-theme-set-faces
   'rvb3

   ;; Menu selection follows the classic Mac convention: solid reversed video.
   `(rvb3-selected-face
     ((,class (:foreground ,paper :background ,ink))))
   ;; Graphical buffers use a printed dot screen with ordinary black text.
   ;; Terminals fall back to the same reversed video used by menu selections.
   `(rvb3-stipple-face
     ((,graphic (:foreground ,ink :background ,paper :stipple ,halftone))
      (,class (:foreground ,paper :background ,ink))))
   ;; The page.
   `(default ((,class (:foreground ,ink :background ,paper))))
   `(cursor ((,class (:background ,ink))))
   `(fringe ((,class (:foreground ,rule :background ,paper))))
   ;; Keep these attributes direct rather than inherited.  Magit diff faces
   ;; specify their own foregrounds, and could otherwise leave white addition
   ;; text on the region's pale background when a drag began inside the diff.
   `(region ((,graphic (:foreground ,ink :background ,paper
                                    :stipple ,halftone
                                    :distant-foreground ,ink))
             (,class (:foreground ,paper :background ,ink
                                   :distant-foreground ,paper))))
   `(secondary-selection
     ((,class (:inherit rvb3-stipple-face))))
   `(highlight ((,class (:inherit rvb3-selected-face))))
   `(hl-line ((,class (:background ,paper))))
   `(vertical-border ((,class (:foreground ,rule))))
   `(window-divider ((,class (:foreground ,rule))))
   `(window-divider-first-pixel ((,class (:foreground ,rule))))
   `(window-divider-last-pixel ((,class (:foreground ,rule))))
   `(shadow ((,class (:foreground ,ink :background ,wash))))
   `(line-number ((,class (:foreground ,grey :background ,paper))))
   `(line-number-current-line ((,class (:foreground ,ink :background ,wash))))
   ;; Ruled off rather than boxed in: the mode line is the same paper,
   ;; separated by a hairline.
   `(mode-line ((,class (:foreground ,ink :background ,paper
                                     :box nil :overline ,rule))))
   `(mode-line-active ((,class (:inherit mode-line))))
   `(mode-line-inactive ((,class (:foreground ,ink :background ,wash
                                              :box nil :overline ,rule))))
   `(mode-line-buffer-id ((,class (:foreground ,ink :weight bold))))
   `(header-line ((,class (:foreground ,paper :background ,ink :box nil))))
   `(rvb/ui-page-chrome-command
     ((,graphic (:inherit rvb3-stipple-face))
      (,class (:foreground ,ink :background ,paper))))
   `(rvb/ui-page-chrome-breadcrumb-highlight
     ((,class (:foreground ,ink :background ,paper))))
   `(minibuffer-prompt ((,class (:foreground ,ink))))
   `(tooltip ((,class (:foreground ,ink :background ,paper
                                  :box (:line-width 1 :color ,rule)))))

   ;; Syntax, by rules and grey value.
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-string-face ((,class (:foreground ,soft))))
   `(font-lock-regexp-face ((,class (:foreground ,soft))))
   `(font-lock-keyword-face ((,class (:foreground ,ink :weight bold))))
   `(font-lock-function-name-face ((,class (:foreground ,ink :weight bold))))
   `(font-lock-function-call-face ((,class (:inherit default))))
   `(font-lock-type-face ((,class (:foreground ,ink))))
   `(font-lock-constant-face ((,class (:foreground ,soft))))
   `(font-lock-number-face ((,class (:foreground ,soft))))
   `(font-lock-builtin-face ((,class (:foreground ,soft))))
   `(font-lock-preprocessor-face ((,class (:foreground ,grey :background ,wash))))
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
   ;; Warnings use the same unequivocal reversed video as other diagnostics.
   `(font-lock-warning-face ((,class (:inherit rvb3-selected-face))))

   ;; Search is navigation rather than alarm: matches use the same printed
   ;; screen as other in-buffer selections.  Failure uses reversed video.
   `(isearch ((,class (:inherit rvb3-stipple-face
                               :box (:line-width 1 :color ,ink)))))
   `(isearch-fail ((,class (:inherit rvb3-selected-face))))
   `(lazy-highlight ((,class (:inherit rvb3-stipple-face))))
   `(match ((,class (:inherit rvb3-stipple-face))))
   `(show-paren-match ((,class (:inherit rvb3-stipple-face))))
   `(show-paren-mismatch ((,class (:inherit rvb3-selected-face))))
   ;; Underlined, the way a printed reference is.
   `(link ((,class (:foreground ,ink :underline t))))
   `(link-visited ((,class (:foreground ,soft :underline t))))
   `(button ((,class (:foreground ,ink :underline t))))
   `(custom-button ((,class (:foreground ,ink :background ,wash
                                         :box (:line-width 1 :color ,rule)))))
   `(custom-button-mouse ((,class (:inherit rvb3-selected-face
                                           :box (:line-width 1 :color ,ink)))))

   ;; Diagnostics.
   `(error ((,class (:inherit rvb3-selected-face))))
   `(warning ((,class (:inherit rvb3-selected-face))))
   `(success ((,class (:foreground ,ink))))
   `(flymake-error ((,class (:inherit rvb3-selected-face))))
   `(flymake-warning ((,class (:inherit rvb3-selected-face))))
   `(flymake-note ((,class (:underline (:style wave :color ,rule)))))
   `(flycheck-error ((,class (:inherit rvb3-selected-face))))
   `(flycheck-warning ((,class (:inherit rvb3-selected-face))))
   `(flycheck-info ((,class (:underline (:style wave :color ,rule)))))
   `(compilation-error ((,class (:inherit rvb3-selected-face))))
   `(compilation-warning ((,class (:inherit rvb3-selected-face))))
   `(compilation-info ((,class (:foreground ,soft))))
   `(eglot-highlight-symbol-face ((,class (:inherit rvb3-stipple-face))))
   `(eglot-mode-line ((,class (:foreground ,ink))))
   `(eglot-inlay-hint-face ((,class (:foreground ,grey :height 0.8))))
   `(eglot-type-hint-face ((,class (:inherit eglot-inlay-hint-face))))
   `(eglot-parameter-hint-face ((,class (:inherit eglot-inlay-hint-face))))

   ;; Completion.
   `(company-tooltip ((,class (:foreground ,ink :background ,wash))))
   `(company-tooltip-selection ((,class (:inherit rvb3-selected-face))))
   `(corfu-default ((,class (:foreground ,ink :background ,paper))))
   `(corfu-current ((,class (:inherit rvb3-selected-face :extend t))))
   `(corfu-border ((,class (:background ,ink))))
   `(corfu-bar ((,class (:background ,ink))))
   `(corfu-annotations ((,class (:inherit nil))))
   `(corfu-deprecated ((,class (:inherit nil :strike-through t))))
   `(corfu-mouse ((,class (:inherit rvb3-selected-face))))
   `(corfu-quick1 ((,class (:foreground ,paper :background ,ink))))
   `(corfu-quick2 ((,class (:foreground ,ink :background ,paper :weight bold))))
   `(corfu-popupinfo ((,class (:foreground ,ink :background ,paper
                                          :box (:line-width 1 :color ,ink)))))
   ;; Match faces leave foreground and background unspecified.  They therefore
   ;; remain black on ordinary candidates and white on reversed selections.
   `(company-tooltip-common ((,class (:weight bold))))
   `(vertico-current ((,class (:inherit rvb3-selected-face :extend t))))
   `(completions-common-part ((,class (:weight bold))))
   `(orderless-match-face-0 ((,class (:weight bold))))
   `(orderless-match-face-1 ((,class (:weight bold))))
   `(orderless-match-face-2 ((,class (:weight bold))))
   `(orderless-match-face-3 ((,class (:weight bold))))
   ;; Marginalia's defaults inherit syntax faces with explicit foregrounds.
   ;; Leave file annotations colourless so reversed Vertico rows stay white
   ;; from the candidate through its owner and permission columns.
   `(marginalia-documentation ((,class (:inherit nil))))
   `(marginalia-file-name ((,class (:inherit nil))))
   `(marginalia-file-owner ((,class (:inherit nil))))
   `(marginalia-size ((,class (:inherit nil))))
   `(marginalia-date ((,class (:inherit nil))))
   `(marginalia-file-priv-no ((,class (:inherit nil))))
   `(marginalia-file-priv-dir ((,class (:inherit nil))))
   `(marginalia-file-priv-link ((,class (:inherit nil))))
   `(marginalia-file-priv-read ((,class (:inherit nil))))
   `(marginalia-file-priv-write ((,class (:inherit nil))))
   `(marginalia-file-priv-exec ((,class (:inherit nil))))
   `(marginalia-file-priv-other ((,class (:inherit nil))))
   `(marginalia-file-priv-rare ((,class (:inherit nil))))

   ;; Files, diffs, and Magit.  Additions are solid reversed ink; removals are
   ;; screened out.  The same monochrome grammar is used by Diff and Magit.
   `(dired-directory ((,class (:foreground ,ink :weight bold))))
   `(dired-header ((,class (:foreground ,ink :background ,wash))))
   `(dired-flagged ((,class (:inherit rvb3-selected-face))))
   `(dired-marked ((,class (:inherit rvb3-stipple-face))))
   `(diff-added ((,class (:foreground ,paper :background ,ink :extend t))))
   `(diff-removed ((,class (:inherit rvb3-stipple-face :extend t))))
   `(diff-changed ((,class (:foreground ,ink :background ,paper :extend t))))
   `(diff-refine-added
     ((,class (:foreground ,paper :background ,ink :weight bold))))
   `(diff-refine-removed
     ((,class (:inherit rvb3-stipple-face :weight bold))))
   `(diff-context ((,class (:foreground ,soft))))
   `(diff-header ((,class (:foreground ,ink :background ,wash))))
   `(diff-file-header ((,class (:foreground ,ink :background ,wash))))
   `(diff-hunk-header ((,class (:foreground ,grey))))
   `(diff-hl-insert ((,class (:foreground ,ink :background ,paper))))
   `(diff-hl-delete ((,class (:foreground ,ink :background ,paper))))
   `(diff-hl-change ((,class (:foreground ,ink :background ,paper))))

   ;; Ediff treats A as the removed/older side and B as the added/newer side.
   ;; A is screened, B is reversed, C is bold, and Ancestor stays plain.
   `(ediff-current-diff-A
     ((,class (:inherit rvb3-stipple-face :extend t))))
   `(ediff-current-diff-B
     ((,class (:inherit rvb3-selected-face :extend t))))
   `(ediff-current-diff-C
     ((,class (:foreground ,ink :background ,paper :weight bold :extend t))))
   `(ediff-current-diff-Ancestor
     ((,class (:foreground ,ink :background ,paper :extend t))))
   `(ediff-fine-diff-A
     ((,class (:inherit rvb3-stipple-face :weight bold))))
   `(ediff-fine-diff-B
     ((,class (:inherit rvb3-selected-face :weight bold))))
   `(ediff-fine-diff-C
     ((,class (:foreground ,ink :background ,paper :weight bold))))
   `(ediff-fine-diff-Ancestor
     ((,class (:foreground ,ink :background ,paper :weight bold))))
   `(ediff-even-diff-A
     ((,class (:inherit rvb3-stipple-face :extend t))))
   `(ediff-odd-diff-A
     ((,class (:inherit rvb3-stipple-face :extend t))))
   `(ediff-even-diff-B
     ((,class (:inherit rvb3-selected-face :extend t))))
   `(ediff-odd-diff-B
     ((,class (:inherit rvb3-selected-face :extend t))))
   `(ediff-even-diff-C
     ((,class (:foreground ,ink :background ,paper :weight bold :extend t))))
   `(ediff-odd-diff-C
     ((,class (:foreground ,ink :background ,paper :weight bold :extend t))))
   `(ediff-even-diff-Ancestor
     ((,class (:foreground ,ink :background ,paper :extend t))))
   `(ediff-odd-diff-Ancestor
     ((,class (:foreground ,ink :background ,paper :extend t))))

   `(magit-branch-local ((,class (:foreground ,ink))))
   `(magit-branch-remote ((,class (:foreground ,soft))))
   `(magit-branch-current ((,class (:inherit rvb3-selected-face))))
   `(magit-section-heading ((,class (:foreground ,ink :background ,wash))))
   ;; This face is layered over more specific diff faces.  Keep its colours
   ;; unspecified so it cannot turn white-on-black additions into white text
   ;; on the page background when point enters their section.
   `(magit-section-highlight
     ((,class (:foreground unspecified :background unspecified :extend t))))
   `(magit-section-heading-selection
     ((,class (:inherit rvb3-selected-face))))
   ;; Additions are fully placed ink; removals are screened out like a
   ;; paste-up deletion.  Highlighting does not add extra line decoration.
   `(magit-diff-added
     ((,class (:foreground ,paper :background ,ink :extend t))))
   `(magit-diff-added-highlight
     ((,class (:foreground ,paper :background ,ink :extend t))))
   `(magit-diff-removed
     ((,class (:inherit rvb3-stipple-face :extend t))))
   `(magit-diff-removed-highlight
     ((,class (:inherit rvb3-stipple-face :extend t))))
   `(magit-diff-context ((,class (:foreground ,soft :extend t))))
   `(magit-diff-context-highlight ((,class (:foreground ,soft :background ,wash :extend t))))
   `(magit-diff-hunk-heading ((,class (:foreground ,grey :background ,paper :extend t))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,ink :background ,wash :extend t))))
   `(magit-diff-lines-heading
     ((,class (:foreground ,paper :background ,ink :extend t))))
   `(magit-diff-lines-boundary
     ((,class (:foreground ,paper :background ,ink :extend t))))
   `(magit-diff-file-heading ((,class (:foreground ,ink :weight bold))))
   `(magit-hash ((,class (:foreground ,grey))))
   `(magit-dimmed ((,class (:foreground ,grey))))
   `(magit-log-author ((,class (:foreground ,soft))))
   `(magit-log-date ((,class (:foreground ,grey))))
   `(magit-tag ((,class (:foreground ,ink :box (:line-width 1 :color ,rule)))))

   ;; Org.  The title carries the typographic weight once per document.
   ;; Other headings stay plain, so a rule
   ;; under each one carries the weight instead -- the levels then read
   ;; as grey values against a line rather than as sizes.
   `(org-document-title ((,class (:foreground ,ink :weight bold))))
   `(org-document-info ((,class (:foreground ,soft))))
   `(org-document-info-keyword ((,class (:foreground ,grey))))
   `(org-level-1 ((,class (:foreground ,ink :weight normal :underline t))))
   `(org-level-2 ((,class (:foreground ,ink :weight normal :underline nil))))
   `(org-level-3 ((,class (:foreground ,soft :weight normal :underline nil))))
   `(org-level-4 ((,class (:foreground ,soft :weight normal :underline nil))))
   `(org-level-5 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-level-6 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-level-7 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-level-8 ((,class (:foreground ,grey :weight normal :underline nil))))
   `(org-todo ((,class (:inherit rvb3-selected-face))))
   `(org-done ((,class (:foreground ,ink :weight normal :strike-through t))))
   `(org-headline-done ((,class (:foreground ,ink :strike-through t))))
   `(org-checkbox ((,class (:foreground ,ink :background ,wash))))
   `(org-block ((,class (:background ,wash :extend t))))
   `(org-block-begin-line ((,class (:foreground ,grey :background ,wash :extend t))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))
   `(org-code ((,class (:foreground ,soft))))
   `(org-verbatim ((,class (:foreground ,soft))))
   `(org-quote ((,class (:foreground ,soft :background ,wash :extend t))))
   `(org-table ((,class (:foreground ,soft))))
   `(org-date ((,class (:foreground ,grey :underline t))))
   `(org-tag ((,class (:foreground ,grey :weight normal))))
   `(org-special-keyword ((,class (:foreground ,grey))))
   `(org-drawer ((,class (:foreground ,grey))))
   `(org-meta-line ((,class (:foreground ,grey))))
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
   `(markdown-blockquote-face ((,class (:foreground ,soft :background ,wash))))
   `(markdown-link-face ((,class (:foreground ,ink :underline t))))
   `(markdown-url-face ((,class (:foreground ,grey))))

   ;; Sh
   `(sh-heredoc ((,class (:foreground ,soft))))
   `(sh-quoted-exec ((,class (:foreground ,soft))))

   ;; Tabs and the rest.
   `(tab-bar ((,class (:foreground ,grey :background ,paper))))
   `(tab-bar-tab ((,class (:inherit rvb3-selected-face
                                   :box (:line-width 1 :color ,ink)))))
   `(tab-bar-tab-inactive ((,class (:foreground ,ink :background ,paper))))
   `(tab-line ((,class (:foreground ,grey :background ,paper))))
   `(tab-line-tab-current ((,class (:inherit rvb3-selected-face))))
   `(tab-line-tab-inactive ((,class (:foreground ,ink :background ,paper))))
   `(widget-field ((,class (:background ,wash :box (:line-width 1 :color ,rule)))))
   `(trailing-whitespace ((,class (:background ,ink))))
   `(whitespace-trailing ((,class (:background ,ink))))
   `(fill-column-indicator ((,class (:foreground ,rule))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rvb3)

;;; rvb3-theme.el ends here
