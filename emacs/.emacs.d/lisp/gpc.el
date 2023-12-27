;;; gpc.el --- A general purpose cache facility.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; URL: https://github.com/mukuge/gpc.el
;; Keywords: lisp
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A general purpose cache facility for Emacs.

;;; Code:

(require 'cl-lib)
(require 'nalist)

;; Utility functions.

(defun gpc-util-hash-to-alist (hash-table)
  "Return an alist made from keys and values of HASH-TABLE."
  (let ((alist nil))
    (maphash #'(lambda (k v) (nalist-set k v alist)) hash-table)
    alist))

(defun gpc-util-alist-to-hash (alist)
  "Return a hash table made of keys and values of ALIST."
  (let ((ht (make-hash-table)))
    (nalist-map #'(lambda (k v) (puthash k v ht)) alist)
    ht))

;; Initilization functions.

(defmacro gpc-init (name spec-list)
  "Bind NAME to a general purpose cache specified in SPEC-LIST.

General purpose cache, or `gpc', is a cache facility which
enables you to store return values of fetch functions in a
variable for future reuse.

`gpc' uses two places to store information: One is the ordinary
variable binding , which keeps cached data, the other is the
symbol's property list, where the specification of the cache is
kept with the key `gpc-cache-spec'.

Aside from the cache spec mechanism, a gpc cache is just a named
association list, or `nalist'.  Some of its cache access
functions is actually aliases to the corresponding functions in
the 'nalist' library.'

A cache spec is implemented as a hash table whose key is a key of
a cache entry, and the value associated with each key is a
list, (initval fetchfn), which specifies the initial value and
fetch function of the cash entry."
  (declare (indent 1))
  `(prog1
       (nalist-init ,name nil)
     (gpc-set-spec ,name (gpc-util-alist-to-hash ,spec-list))))

(defalias 'gpc-make-local-variable 'nalist-make-local-variable)

(defalias 'gpc-make-variable-buffer-local 'nalist-make-variable-buffer-local)

(defmacro gpc-overwrite-with-initvals (cache)
  "Overwrite the whole cache content with initvals in the CACHE spec."
  `(maphash #'(lambda (k v)
                (nalist-set k (car v) ,cache))
            (gpc-get-spec ,cache)))

(cl-defmacro defcache (name buffer-local doc-string &rest spec-list)
  "Define NAME as a general purpose cache, and return a symbol.

This macro uses `defvar' internally, so the resulting symbol as a
variable is special, and DOC-STRING is stored in the symbol's
property list.

The resulting variable is initialized as an automatically
buffer-local variable if the value of BUFFER-LOCAL is
:buffer-local. Otherwise, as a global variable.

SPEC-LIST defines the specification of the cache: the initial
values and fetch functions.  See `gpc-init' for the detail."
  (declare (indent 2))
  `(prog1
       (defvar ,name nil ,doc-string)
     (gpc-set-spec ,name (gpc-util-alist-to-hash ',spec-list))
     (nalist-init ,name nil)
     (when (eq ,buffer-local :buffer-local)
       (gpc-make-variable-buffer-local ,name))))

;; Cache spec access functions

(defmacro gpc-set-spec (symbol hash-table)
  "Set HASH-TABLE in SYMBOL's property list as a cache spec.

HASH-TABLE should contain a cache spec following the spec
description format.  See `gpc-init' for the detail."
  `(put ',symbol 'gpc-cache-spec ,hash-table))

(defmacro gpc-get-spec (cache)
  "Return the spec of CACHE."
  `(get ',cache 'gpc-cache-spec))

(defmacro gpc-spec-set-entry (key initval fetchfn cache)
  "Set the CACHE spec entry whose key is KEY to have the value (INITVAL FETCHFN)."
  `(puthash ,key (list ,initval ,fetchfn) (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-entry (key cache)
  "Return a CACHE spec entry with KEY if exists, otherwise nil.

A CACHE spec entry is a list: (KEY initval fetchfn)."
  `(gethash ,key (gpc-get-spec ,cache)))

(defmacro gpc-spec-get-initval (key cache)
  "Get the initval of the CACHE spec entry with KEY."
  `(nth 0 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-get-fetchfn (key cache)
  "Get the fetch function of the CACHE spec entry with KEY."
  `(nth 1 (gpc-spec-get-entry ,key ,cache)))

(defmacro gpc-spec-map (function cache)
  "Call FUNCTION for all keys and values in CACHE.

The function should have three arguments, which are filled by
this macro with a key, its initval, and its fetchfn."
  `(maphash '(lambda (k v)
               (funcall ,function k (nth 0 v) (nth 1 v)))
            (gpc-get-spec ,cache)))

(defmacro gpc-spec-keyp (key cache)
  "Return t if KEY is a key in the CACHE's spec, otherwise nil."
  `(if (gpc-spec-get-entry ,key ,cache) t nil))

(defmacro gpc-pp-spec (cache)
  "Pretty print the CACHE spec, and return it."
  (let ((alist (cl-gensym "alist-")))
    `(let ((,alist (gpc-util-hash-to-alist
                    (gpc-get-spec ,cache))))
       (message (pp ,alist))
       ,alist)))

;; Cache content access functions

(defalias 'gpc-val 'nalist-get)

(defmacro gpc-fetch (key cache)
  "Fetch the value of KEY in CACHE by calling its fetch function.

It returns the fetched value."
  (let ((ekey (cl-gensym "key-")))
    `(let ((,ekey ,key))
       (if (gpc-locked-p ,cache)
           (gpc-val ,ekey ,cache)
         (nalist-set ,ekey (funcall (gpc-spec-get-fetchfn ,ekey ,cache)) ,cache)))))

(defmacro gpc-fetch-all (cache)
  "Fetch values for all keys in the CACHE spec."
  `(gpc-spec-map '(lambda (k v f)
                    (gpc-fetch k ,cache))
                 ,cache))

(cl-defmacro gpc-get (key cache &key (force nil))
  "Return the value of KEY in CACHE by calling the fetchfn if needed.

It uses fetchfn to get the value when FORCE is non-nil."
  (let ((ekey (cl-gensym "key-")))
    `(let ((,ekey ,key))
       (if ,force
           (gpc-fetch ,ekey ,cache)
         (if (gpc-pair-exist-p ,ekey ,cache)
             (gpc-val ,ekey ,cache)
           (gpc-fetch ,ekey ,cache))))))

(defalias 'gpc-set 'nalist-set)

(defalias 'gpc-remove 'nalist-remove)

(defalias 'gpc-clear 'nalist-clear)

(defalias 'gpc-pairs 'nalist-pairs)

(defalias 'gpc-keys 'nalist-keys)

(defalias 'gpc-values 'nalist-values)

(cl-defun gpc-pair-exist-p (key cache &key (testfn 'eq))
  "Return t if CACHE has an entry with KEY, otherwise nil."
  (nalist-get key cache :default nil :testfn testfn))

(defun gpc-pp (cache)
  "Pretty print and return the whole content of CACHE."
  (message (pp cache))
  cache)

(defmacro gpc-lock (cache)
  "Lock the values in CACHE, and return the lock list of CACHE.

After locking, `gpc-fetch' acts like `gpc-val'.  This gpc lock
feature is intended to be used with buffer-local variables.

The lock list of CACHE contains the buffers where CACHE is
locked."
  (let ((locked-bufs (cl-gensym "locked-bufs-")))
    `(progn
       (gpc-lock-gc ,cache)
       (let ((,locked-bufs (get ',cache 'gpc-locked-buffers))
             (buf (current-buffer)))
         (unless (member buf ,locked-bufs)
           (push buf ,locked-bufs)
           (put ',cache 'gpc-locked-buffers ,locked-bufs))))))

(defmacro gpc-unlock (cache)
  "Unlock CACHE."
  (let ((locked-bufs (cl-gensym "locked-bufs-")))
    `(let ((,locked-bufs (get ',cache 'gpc-locked-buffers))
           (buf (current-buffer)))
       (when (member buf ,locked-bufs)
         (put ',cache 'gpc-locked-buffers (remove buf ,locked-bufs))))))

(defmacro gpc-lock-clear (cache)
  "Set the lock list of CACHE nil."
  `(put ',cache 'gpc-locked-buffers nil))

(defmacro gpc-lock-gc (cache)
  "Remove killed buffers from the lock list of CACHE."
  (let ((locked-bufs (cl-gensym "locked-bufs-")))
    `(let ((,locked-bufs (get ',cache 'gpc-locked-buffers))
           (buf (current-buffer)))
       (put ',cache 'gpc-locked-buffers (cl-remove-if-not 'buffer-live-p ,locked-bufs)))))

(defmacro gpc-lock-pp (cache)
  "Pretty print the locked buffers for CACHE."
  `(message (pp (get ',cache 'gpc-locked-buffers))))

(defmacro gpc-get-lock-list (cache)
  "Return the lock list of CACHE."
  `(get ',cache 'gpc-locked-buffers))

(defmacro gpc-locked-p (cache)
  "Return t if CACHE is locked, otherwise nil."
  `(if (member (current-buffer)
               (get ',cache 'gpc-locked-buffers))
       t nil))

(defmacro gpc-copy (cache from-buffer to-buffer)
  "Copy the content of CACHE from FROM-BUFFER to TO-BUFFER.

Use this function when CACHE is buffer-local or automatically
buffer-local."
  (let ((efrom-buffer (cl-gensym "from-buffer-"))
        (eto-buffer (cl-gensym "to-buffer-"))
        (content (cl-gensym "content-")))
    `(let ((,efrom-buffer ,from-buffer)
           (,eto-buffer ,to-buffer)
           (,content))
       (save-excursion
         (let ((content nil))
           (set-buffer ,efrom-buffer)
           (setq ,content (copy-alist ,cache))
           (set-buffer ,eto-buffer)
           (setq ,cache ,content))))))

(provide 'gpc)
;;; gpc.el ends here
