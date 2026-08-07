;;; rvb2-theme.el --- Muted screenshot-derived terminal theme -*- lexical-binding: t; -*-

;;; Commentary:
;; A standalone Emacs theme using colors sampled from an old terminal
;; screenshot and restrained, classic syntax highlighting.

;;; Code:

(deftheme rvb2
  "A muted, screenshot-derived terminal theme.")

(let ((class '((class color) (min-colors 89)))
      ;; Screenshot-derived terminal palette.
      (bg          "#010202")
      (bg-alt      "#27282a")
      (fg          "#eeeeed")
      (fg-dim      "#a7b2aa")
      (cyan        "#74b8b7")
      (cyan-bright "#77c5c3")
      (blue        "#667a8b")
      (red         "#971c2b")
      (red-bright  "#cb4c52")
      (magenta     "#b568a6")
      (yellow      "#fbf695")
      (ochre       "#998375")
      (green       "#d3e3b1")
      (string      "#f2f2d0")
      (selection   "#eeeeed"))
  (custom-theme-set-faces
   'rvb2

   ;; Editor chrome.
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,fg))))
   `(fringe ((,class (:foreground ,blue :background ,bg))))
   ;; Preserve syntax foregrounds inside the selection, as terminal Vim does.
   ;; `distant-foreground' keeps ordinary near-white text legible without
   ;; flattening colored font-lock faces to a single region foreground.
   `(region ((,class (:background ,selection :distant-foreground ,bg :extend t))))
   `(secondary-selection ((,class (:background ,bg-alt :extend t))))
   `(highlight ((,class (:background ,bg-alt))))
   `(hl-line ((,class (:background ,bg-alt))))
   `(vertical-border ((,class (:foreground ,bg-alt))))
   `(shadow ((,class (:foreground ,fg-dim))))
   `(line-number ((,class (:foreground ,blue :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,yellow :background ,bg :weight bold))))
   ;; `(mode-line ((,class (:foreground ,fg :background ,bg-alt :box nil))))
   ;; `(mode-line-active ((,class (:inherit mode-line))))
   ;; `(mode-line-inactive ((,class (:foreground ,fg-dim :background ,bg :box nil))))
   ;; `(mode-line-buffer-id ((,class (:foreground ,yellow :weight bold))))
   `(header-line ((,class (:foreground ,fg :background ,bg-alt :box nil))))
   `(minibuffer-prompt ((,class (:foreground ,cyan-bright :weight bold))))

   ;; Syntax roles: Comment=blue, Constant=dark red, Identifier=cyan,
   ;; PreProc=magenta, Special=ochre, Statement=yellow, and Type=green.
   `(font-lock-comment-face ((,class (:foreground ,blue))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-doc-face ((,class (:foreground ,blue))))
   `(font-lock-constant-face ((,class (:foreground ,red))))
   `(font-lock-number-face ((,class (:foreground ,red-bright))))
   `(font-lock-function-name-face ((,class (:foreground ,cyan))))
   `(font-lock-function-call-face ((,class (:foreground ,cyan))))
   ;; Tree-sitter exposes variables and operators much more aggressively than
   ;; classic Vim syntax.  Keep them neutral so ordinary expressions remain a
   ;; stable, readable foreground instead of becoming a cyan/yellow patchwork.
   `(font-lock-variable-name-face ((,class (:inherit default))))
   `(font-lock-variable-use-face ((,class (:inherit default))))
   `(font-lock-property-name-face ((,class (:foreground ,cyan))))
   `(font-lock-property-use-face ((,class (:foreground ,cyan))))
   `(font-lock-builtin-face ((,class (:foreground ,cyan))))
   `(font-lock-keyword-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,green :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-regexp-face ((,class (:foreground ,string))))
   `(font-lock-escape-face ((,class (:foreground ,ochre))))
   `(font-lock-operator-face ((,class (:inherit default))))
   `(font-lock-negation-char-face ((,class (:foreground ,ochre))))
   `(font-lock-warning-face ((,class (:foreground ,red-bright :weight bold))))
   `(font-lock-bracket-face ((,class (:foreground ,fg))))
   `(font-lock-delimiter-face ((,class (:foreground ,fg))))
   `(font-lock-punctuation-face ((,class (:foreground ,fg))))
   `(font-lock-misc-punctuation-face ((,class (:inherit font-lock-punctuation-face))))

   ;; Search and navigation.
   `(isearch ((,class (:foreground ,bg :background ,yellow :weight bold))))
   `(lazy-highlight ((,class (:foreground ,fg :background ,red))))
   `(match ((,class (:foreground ,bg :background ,green))))
   `(show-paren-match ((,class (:foreground ,bg :background ,cyan-bright :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,fg :background ,red-bright :weight bold))))
   `(link ((,class (:foreground ,cyan-bright :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))

   ;; Diagnostics and language tooling.
   `(error ((,class (:foreground ,red-bright :weight bold))))
   `(warning ((,class (:foreground ,ochre :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))
   `(flymake-error ((,class (:underline (:style wave :color ,red-bright)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,ochre)))))
   `(flymake-note ((,class (:underline (:style wave :color ,cyan)))))
   `(flycheck-error ((,class (:underline (:style wave :color ,red-bright)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,ochre)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,cyan)))))
   `(eglot-highlight-symbol-face ((,class (:background ,bg-alt :weight bold))))
   `(eglot-mode-line ((,class (:foreground ,green))))
   `(eglot-inlay-hint-face ((,class (:foreground ,fg-dim :height 0.8))))
   `(eglot-type-hint-face ((,class (:inherit eglot-inlay-hint-face :foreground ,green))))
   `(eglot-parameter-hint-face ((,class (:inherit eglot-inlay-hint-face :foreground ,cyan))))

   ;; Completion.
   `(company-tooltip ((,class (:foreground ,fg :background ,bg-alt))))
   `(company-tooltip-selection ((,class (:foreground ,bg :background ,selection))))
   `(company-tooltip-common ((,class (:foreground ,cyan-bright :weight bold))))
   `(vertico-current ((,class (:foreground ,bg :background ,selection))))
   `(orderless-match-face-0 ((,class (:foreground ,cyan-bright :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,yellow :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,green :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,magenta :weight bold))))

   ;; Files, diffs, and Magit.
   `(dired-directory ((,class (:foreground ,cyan :weight bold))))
   `(dired-header ((,class (:foreground ,yellow :weight bold))))
   `(dired-flagged ((,class (:foreground ,red-bright))))
   `(dired-marked ((,class (:foreground ,magenta))))
   `(diff-added ((,class (:foreground ,green))))
   `(diff-removed ((,class (:foreground ,red-bright))))
   `(diff-header ((,class (:foreground ,yellow :weight bold))))
   `(diff-file-header ((,class (:foreground ,cyan :weight bold))))
   `(magit-branch-local ((,class (:foreground ,cyan))))
   `(magit-branch-remote ((,class (:foreground ,magenta))))
   `(magit-section-heading ((,class (:foreground ,yellow :weight bold))))
   `(magit-diff-added ((,class (:foreground ,green))))
   `(magit-diff-removed ((,class (:foreground ,red-bright))))
   `(magit-diff-context-highlight ((,class (:background ,bg-alt))))

   ;; Org and Markdown.
   `(org-level-1 ((,class (:foreground ,yellow :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,cyan :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,green :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,magenta :weight bold))))
   `(org-todo ((,class (:foreground ,red-bright :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-block ((,class (:background ,bg-alt))))
   `(org-block-begin-line ((,class (:foreground ,fg-dim :background ,bg-alt :slant italic))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))
   `(markdown-header-face-1 ((,class (:foreground ,yellow :weight bold :height 1.3))))
   `(markdown-header-face-2 ((,class (:foreground ,cyan :weight bold :height 1.2))))
   `(markdown-header-face-3 ((,class (:foreground ,green :weight bold :height 1.1))))
   `(markdown-code-face ((,class (:background ,bg-alt))))
   `(markdown-inline-code-face ((,class (:foreground ,string :background ,bg-alt))))

   ;; Tabs and miscellaneous UI.
   `(tab-bar ((,class (:foreground ,fg-dim :background ,bg-alt))))
   `(tab-bar-tab ((,class (:foreground ,yellow :background ,bg :weight bold))))
   `(tab-bar-tab-inactive ((,class (:foreground ,fg-dim :background ,bg-alt))))
   `(tab-line ((,class (:foreground ,fg-dim :background ,bg-alt))))
   `(tab-line-tab-current ((,class (:foreground ,yellow :background ,bg :weight bold))))
   `(tab-line-tab-inactive ((,class (:foreground ,fg-dim :background ,bg-alt))))
   `(trailing-whitespace ((,class (:background ,red-bright))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rvb2)

;;; rvb2-theme.el ends here
