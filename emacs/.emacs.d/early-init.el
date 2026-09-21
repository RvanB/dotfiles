;;; -*- lexical-binding: t; -*-

;;; Startup performance
;;
;; No garbage collection while starting: loading the config allocates a
;; lot and keeps nearly all of it, so collecting along the way only
;; costs pauses -- one landed inside Magit's load at over half a second.
;; The threshold comes down once Emacs is up; see rvb-settings.el.
(setq gc-cons-threshold most-positive-fixnum)

;; Activate packages from one precomputed file rather than by scanning
;; every package directory, which also spares loading package.el (and
;; the URL library it pulls in) at startup.  Emacs refreshes the file
;; itself after installing or deleting a package; after anything else
;; that changes elpa/, run `package-quickstart-refresh'.
(setq package-quickstart t)

;; Create the first frame the way init will leave it.  Every parameter
;; init changes on an existing frame is reapplied by
;; `frame-notice-user-settings' once init is done, which relaid out the
;; frame and cost half a second.  The tab bar is `tab-bar-mode' in
;; rvb-ui.el, the scroll bars `scroll-bar-mode'.
;;
;; No internal border: it is painted in the default background, so the
;; dark tab bar and header band stopped two pixels short of each edge,
;; leaving a white sliver down both sides.  The fringes still keep the
;; text off the edge of the window.
(dolist (parameter '((tab-bar-lines . 1) (vertical-scroll-bars) (tool-bar-lines . 0)
                     (internal-border-width . 0)))
  (add-to-list 'default-frame-alist parameter)
  (add-to-list 'initial-frame-alist parameter))
;; Changing the font, the tab bar or the like resizes the frame to keep
;; its text area the same size.  At startup there is no size to keep.
(setq frame-inhibit-implied-resize t)

;; Frame parameters that need to exist before the first macOS frame is created.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))
(add-to-list 'initial-frame-alist '(ns-transparent-titlebar . nil))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'initial-frame-alist '(ns-appearance . dark))
