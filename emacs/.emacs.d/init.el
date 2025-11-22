;;; Set up package archives 
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents t))
(setq package-install-upgrade-built-in t)

;;; Load my modules from config directory
(add-to-list 'load-path (expand-file-name "rvb-lisp" user-emacs-directory))
(require 'rvb-settings)
(require 'rvb-ui)
(require 'rvb-completions)
(require 'rvb-langs)
(require 'rvb-editing)
(require 'rvb-movement)
(require 'rvb-tools)
(require 'rvb-terminals)
(require 'rvb-ai)
(require 'rvb-org)
(require 'rvb-bindings)
;; (require 'rvb-exwm)
(require 'rvb-aws)

;;; Project tabs
;; (setq tab-bar-format
;;       '(tab-bar-format-tabs
;;         tab-bar-separator
;;         tab-bar-format-align-right))
;; (tab-bar-mode)

;; (use-package otpp
;;   :ensure t
;;   :after project
;;   :init
;;   ;; Enable `otpp-mode` globally
;;   (otpp-mode 1)
;;   ;; If you want to advice the commands in `otpp-override-commands`
;;   ;; to be run in the current's tab (so, current project's) root directory
;;   (otpp-override-mode 1))




