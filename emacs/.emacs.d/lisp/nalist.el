;;; nalist.el --- API for named association lists.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: Lisp, tools
;; URL: https://github.com/mukuge/nalist.el
;; Package-Version: 0.1.5
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; API for named association lists.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun nalist-pairp (obj)
  "Return t if OBJ is a pair, otherwise nil.

A pair is a cons cell, regardless of what Lisp objects its car
and cdr point."
  (consp obj))

(defun nalist-proper-list-p (obj)
  "Return t if OBJ is a proper list, otherwise nil.

A proper list is a non circular cons chain whose last `cdr' points nil."
  (or
   (null obj)
   (and (consp obj)
        (cl-loop
         for nth-cdr = obj then (cdr nth-cdr)
         for 2nth-cdr = (cdr obj) then (cddr 2nth-cdr)
         when (eq nth-cdr 2nth-cdr) return nil
         ;; A circular list consists of finite positions: When we move
         ;; a pointer along its cons cell chain using `cdr', it goes
         ;; back to the same reference point in finite steps.
         ;; Moreover, it's periodic.  Let's assume the period is N
         ;; steps.  If we move two pointers so that one moves one step
         ;; at a time starting from the position 1, the other moves
         ;; two steps at a time starting from the position 2, the
         ;; former points the nth position, the latter points the 2nth
         ;; position after n iterations.  So, after N iterations, the
         ;; former and the latter point the same position on the
         ;; circular list because it's N-periodic.
         ;;
         when (null nth-cdr) return t
         ;; nth-cdr and 2nth-cdr advance different steps at a
         ;; time. That means there are some points where the nth-cdr
         ;; visit before the 2nth-cdr.  So, need to check nullness of
         ;; the nth-cdr.
         ;;
         when (null 2nth-cdr) return t
         when (not (consp 2nth-cdr)) return nil
         ;; Check for non circular lists if 2nth cdr position is a
         ;; terminus.
         ;;
         when (null (cdr 2nth-cdr)) return t
         when (not (consp (cdr 2nth-cdr))) return nil
         ;; Check for non circular lists if (2n+1)th cdr position is a
         ;; terminus.
         ;;
         when (null (cddr 2nth-cdr)) return t
         when (not (consp (cddr 2nth-cdr))) return nil)
        ;; Check for non circular list if (2n+2)th, or the next 2nth,
        ;; cdr position is a terminus.
        ;;
        ;; Given the above three positions are not a terminus,
        ;; iterate over to the next nth and 2nth cdr positions.
        )))

(defun nalist-nalist-p (obj)
  "Return t if OBJ is an (n)alist, otherwise nil.

An alist, or association list, is a proper list of pairs.  What
`car' and `cdr' of a pair in alist point is often called a key
and value respectively."
  (and (nalist-proper-list-p obj)
       (let ((res t))
         (mapc #'(lambda (pair)
                   (unless (nalist-pairp pair)
                     (setq res nil)))
               obj)
         res)))

(cl-defmacro nalist-init (name alist &key shallow)
  "Bind NAME to ALIST if SHALLOW is non-nil, otherwise to a deep-copy of ALIST."
  (let ((ealist (cl-gensym "alist-")))
    `(let ((,ealist ,alist))
       (unless (nalist-nalist-p ,ealist)
         (error "Invalid initial value `%s'" ,ealist))
       (if ,shallow
           (setq ,name ,ealist)
         (setq ,name (copy-alist ,ealist)))
       ',name)))

(defmacro nalist-make-local-variable (nalist)
  "Create a buffer-local binding in the current buffer for NALIST.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells."
  (let ((copy (cl-gensym "copy-")))
    `(let ((,copy (copy-alist ,nalist)))
       (make-local-variable ',nalist)
       (setq ,nalist ,copy)
       ',nalist)))

(defmacro nalist-make-variable-buffer-local (nalist)
  "Mark NALIST automatically buffer local.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells.

It also sets the default value of NALIST to nil to avoid the
buffer-local variables in other buffers share the cons cells
through it."
  (let ((copy (cl-gensym "copy-")))
    `(let ((,copy ,nalist))
       (setq ,nalist nil)
       (make-variable-buffer-local ',nalist)
       (setq ,nalist ,copy)
       ',nalist)))

(defmacro nalist-clear (nalist)
  "Set NALIST nil."
  `(setq ,nalist nil))

(cl-defmacro nalist--alist-set (key value nalist &key (testfn ''eq))
  "Find the pair with KEY in NALIST with TESTFN, and set its value to VALUE.

This is a compatibility macro for Emacs 25."
  (let ((before (cl-gensym "before-"))
        (after (cl-gensym "after-"))
        (pair (cl-gensym "pair-"))
        (pair-found (cl-gensym "pair-found-"))
        (ekey (cl-gensym "key-"))
        (evalue (cl-gensym "value-")))
    `(progn
       (cl-assert (nalist-nalist-p ,nalist) t)
       (let ((,ekey ,key)
             (,evalue ,value))
         (if (null ,nalist)
             (progn
               (setq ,nalist (list (cons ,ekey ,evalue)))
               ,value)
           (cl-do* ((,before nil)
                    (,after ,nalist (cdr ,after))
                    (,pair (car ,after) (car ,after))
                    (,pair-found nil))
               ((null ,after)
                (unless ,pair-found
                  (push (cons ,ekey ,evalue) ,before))
                (setq ,nalist ,before)
                ,evalue)
             (when (funcall ,testfn (car ,pair) ,ekey)
               (setq ,pair-found t)
               (setq ,pair (cons (car ,pair) ,evalue)))
             (push ,pair ,before)))))))

(cl-defmacro nalist-set (key value nalist &key (testfn ''eq))
  "Find the pair with KEY in NALIST with TESTFN, and set its value to VALUE.

This macro destructively changes the value of the pair with KEY
into VALUE if the pair with KEY already exists, otherwise add a
new pair with KEY and VALUE to NALIST.

NALIST needs to be a symbol without a quote to access the correct
binding in its context.

It returns VALUE."
  (let ((evalue (cl-gensym "value-")))
    (if (>= emacs-major-version 26)
        `(let ((,evalue ,value))
           (setf (alist-get ,key ,nalist nil nil ,testfn) ,evalue)
           ,evalue)
      `(let ((,evalue ,value))
         (nalist--alist-set ,key ,evalue ,nalist :testfn ,testfn)))))

(cl-defun nalist--alist-get (key nalist &key (default nil) (testfn 'eq))
  "Return the value associated with KEY in ALIST with using TESTFN.

This is a compatibility function for Emacs 25."
  (let ((list nalist)
        (res default))
    (while list
      (let ((pair (car list)))
        (when (funcall testfn key (car pair))
          (setq res (cdr pair))))
      (setq list (cdr list)))
    res))

(cl-defun nalist-get (key nalist &key default (testfn 'eq))
  "Return the value of KEY in NALIST if found with TESTFN, otherwise DEFAULT.

The key lookup is done with TESTFN if non-nil, otherwise with
`eq'."
  (if (>= emacs-major-version 26)
      (alist-get key nalist default nil testfn)
    (nalist--alist-get key nalist :default default :testfn testfn)))

(cl-defmacro nalist--remove (key nalist &key (default nil) (testfn ''eq))
  "Remove the pair with KEY from NALIST, and return the value of the pair.

This macro uses TESTFN to find the pair with the KEY. The default
value of TESTFN is `eq'.

This is a compatibility function for Emacs 25."
  (let ((before (cl-gensym "before-"))
        (after (cl-gensym "after-"))
        (pair (cl-gensym "pair-"))
        (pair-found (cl-gensym "pair-found-")))
    `(progn
       (cl-assert (nalist-nalist-p ,nalist) t)
       (if (null ,nalist) nil
         (cl-do* ((,before nil)
                  (,after ,nalist (cdr ,after))
                  (,pair (car ,after) (car ,after))
                  (,pair-found nil))
             ((null ,after)
              (setq ,nalist ,before)
              (if ,pair-found
                  (cdr ,pair-found)
                ,default))
           (if (funcall ,testfn (car ,pair) ,key)
               (setq ,pair-found ,pair)
             (push ,pair ,before)))))))

(cl-defmacro nalist-remove (key nalist &key (testfn ''eq))
  "Remove the pair with KEY from NALIST if found with TESTFN."
  (if (>= emacs-major-version 26)
      (let ((ekey (cl-gensym "key-"))
            (etestfn (cl-gensym "testfn-"))
            (value (cl-gensym "value-")))
        `(let ((,ekey ,key)
               (,etestfn ,testfn))
           (let ((,value (alist-get ,ekey ,nalist nil t ,etestfn)))
             (setf (alist-get ,ekey ,nalist nil t ,etestfn) nil)
             ,value)))
    `(nalist--remove ,key ,nalist :default nil :testfn ,testfn)))

(defun nalist-pairs (nalist)
  "Return a list consisting all the pairs in NALIST."
  (cl-assert (nalist-nalist-p nalist) t)
  (copy-alist nalist))

(defun nalist-keys (nalist)
  "Return a list consisting all the keys in NALIST."
  (cl-assert (nalist-nalist-p nalist) t)
  (mapcar 'car nalist))

(defun nalist-values (nalist)
  "Return a list consisting all the values in NALIST."
  (cl-assert (nalist-nalist-p nalist) t)
  (mapcar 'cdr nalist))

(cl-defmacro nalist-copy (nalist-old nalist-new &key shallow)
  "Copy and bind the content of NALIST-OLD to NALIST-NEW.

This macro uses shallow-copy if SHALLOW is non-nil, otherwise
uses deep-copy."
  `(progn
     (cl-assert (nalist-nalist-p ,nalist-old) t)
     (if ,shallow
         (setq ,nalist-new ,nalist-old)
       (setq ,nalist-new (copy-alist ,nalist-old)))
     ',nalist-new))

(defalias 'nalist-pop 'nalist-remove
  "Remove the pair with KEY from NALIST, and return the value of the pair.

This macro uses TESTFN to find the pair with the KEY. The default
value of TESTFN is `eq'.

This is an alias of `nalist-remove'.")

(defmacro nalist-poppair (nalist)
  "Return a pair in NALIST, and remove it from NALIST."
  (let ((pair (cl-gensym "pair-")))
    `(progn
       (cl-assert (nalist-nalist-p ,nalist))
       (let ((,pair (car ,nalist)))
         (setq ,nalist (cdr ,nalist))
         ,pair))))

(defun nalist-map (function nalist)
  "Call FUNCTION for all pairs in NALIST.

FUNCTION is called with two arguments, KEY and VALUE.
‘nalist-map’ always returns nil."
  (cl-assert (nalist-nalist-p nalist) t)
  (let ((remaining nalist))
    (while remaining
      (let ((pair (car remaining)))
        (funcall function (car pair) (cdr pair))
        (setq remaining (cdr remaining))))
    nil))

(defun nalist-subset-p (nalist-a nalist-b)
  "Return t if NALIST-A is a subset of NALIST-B with `equal', otherwise nil."
  (cl-assert (nalist-nalist-p nalist-a) t)
  (cl-assert (nalist-nalist-p nalist-b) t)
  (let ((res t))
    (mapc #'(lambda (pair)
              (unless (member pair nalist-b)
                (setq res nil)))
          nalist-a)
    res))

(defun nalist-equal (nalist-a nalist-b)
  "Return t if NALIST-A nad NALIST-B are identical with `equal', otherwise nil."
  (cl-assert (nalist-nalist-p nalist-a) t)
  (cl-assert (nalist-nalist-p nalist-b) t)
  (equal nalist-a nalist-b))

(defmacro nalist--compat-seq-set-equal-p (sequence1 sequence2 &optional testfn)
  "Return non-nil if SEQUENCE1 and SEQUENCE2 contain the same set of elements.

Equality is defined by `equal' on Emacs 25. On Emacs newer than
version 26, it is defined by TESTFN if non-nil or by `equal’ if
nil."
  (if (>= emacs-major-version 26)
      `(seq-set-equal-p ,sequence1 ,sequence2 ,testfn)
    `(and (nalist-subset-p ,sequence1 ,sequence2)
          (nalist-subset-p ,sequence2 ,sequence1))))

(defun nalist-set-equal-p (nalist-a nalist-b)
  "Test with `equal' if NALIST-A and NALIST-B contain the same set of pairs.

Return t if so, otherwise nil."
  (cl-assert (nalist-nalist-p nalist-a) t)
  (cl-assert (nalist-nalist-p nalist-b) t)
  (nalist--compat-seq-set-equal-p nalist-a nalist-b))



(provide 'nalist)
;;; nalist.el ends here
