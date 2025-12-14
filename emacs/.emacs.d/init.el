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
(require 'rvb-ai)
(require 'rvb-terminals)
(require 'rvb-org)
(require 'rvb-bindings)
(require 'rvb-aws)
