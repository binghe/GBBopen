;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/tools/read-object.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jan  9 16:37:57 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Read Saved/Sent Object Support
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-09-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clos:class-slots
            clos:slot-definition-name)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(read-saved-object		; not yet documented
            read-sent-object)))		; not yet documented

;;; ===========================================================================
;;;  Dispatch-macro-character readers

(defun unbound-value-reader (stream sub-char infix-parameter)
  (declare (ignore sub-char infix-parameter stream))
  unbound-value-indicator)

;;; ---------------------------------------------------------------------------

(defun hash-table-reader (stream sub-char infix-parameter)
  (declare (ignore sub-char infix-parameter))
  (destructuring-bind (ht-test ht-count &rest initargs)
      (read stream t nil 't)
    (declare (dynamic-extent initargs))
    (let ((ht (make-hash-table :test ht-test :size ht-count)))
      (loop for (indicator value) on initargs by #'cddr
	  do (setf (gethash indicator ht) value))
      ht)))

;;; ===========================================================================
;;;  The saved/sent-object readtable

(defparameter *saved/sent-object-readtable*
    (let ((*readtable* (copy-readtable)))
      ;; Duplicate infinity reader (from declared-numerics.lisp):
      (safely-set-dispatch-macro-character #\# #\@ 
					   #-cormanlisp 'inf-reader
					   #+cormanlisp #'inf-reader)
      (safely-set-dispatch-macro-character #\# #\_ 'unbound-value-reader)
      (safely-set-dispatch-macro-character #\# #\H 'hash-table-reader)
      *readtable*))

;;; ===========================================================================
;;;  With-saved/sent-object-syntax

(defmacro with-saved/sent-object-syntax ((&key (readtable
						'*saved/sent-object-readtable*))
					 &body body)
  `(let ((*readtable* ,readtable))
     ,@body))

;;; ===========================================================================
;;;  Temporary testing function

(defun test (string)
  (with-saved/sent-object-syntax ()
    (read-from-string string)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
