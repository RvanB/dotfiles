;;; -*- lexical-binding: t; -*-

;;; Set up package archives
;;
;; Set without loading package.el: the options take effect whenever it
;; does load, which at startup is never -- packages are activated from
;; the quickstart file (see early-init.el).
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-install-upgrade-built-in t)

;; `:ensure t' loads package.el to ask whether a package is installed,
;; on every start, for every package.  An activated package is plainly
;; installed, and so is one that ships with Emacs, like Eglot; only one
;; that is neither goes to package.el -- which then refreshes the
;; archives and installs it as usual.
(defun rvb/use-package-ensure (name args state &optional no-refresh)
  "Ensure NAME's packages in ARGS are installed, cheaply when they are.
A drop-in `use-package-ensure-function'."
  (unless (seq-every-p (lambda (ensure)
                      (let ((package (cond ((eq ensure t) name)
                                           ((consp ensure) (car ensure))
                                           (t ensure))))
                        (or (null ensure)
                            (memq package package-activated-list)
                            (locate-library (symbol-name package)))))
                    args)
    (use-package-ensure-elpa name args state no-refresh)))
(setq use-package-ensure-function #'rvb/use-package-ensure)

;; The same for `:vc': asking whether a package is installed loads
;; package.el, so a package already activated is not asked about.
(defun rvb/use-package-vc-installed-p (arg &rest _)
  "Return non-nil if the package `use-package-vc-install' is given is active.
ARG is its (NAME OPTIONS REVISION)."
  (memq (car arg) package-activated-list))
(advice-add 'use-package-vc-install :before-until #'rvb/use-package-vc-installed-p)

;;; Load my modules from config directory
(add-to-list 'load-path (expand-file-name "rvb-lisp" user-emacs-directory))
(require 'rvb-settings)
(require 'rvb-ui)
(require 'rvb-tabs)
(require 'rvb-completions)
(require 'rvb-langs)
(require 'rvb-editing)
(require 'rvb-movement)
(require 'rvb-projects)
(require 'rvb-tools)
(require 'rvb-features)
(require 'rvb-windows)
(require 'rvb-ai)
(require 'rvb-terminals)
(require 'rvb-org)
(require 'rvb-github)
(require 'rvb-bindings)
