;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/tools/read-object.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Jan 12 14:03:38 2008 *-*
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

;;; ---------------------------------------------------------------------------

(defun standard-object-reader (stream sub-char infix-parameter)
  (declare (ignore sub-char infix-parameter))
  (destructuring-bind (class-name &rest slots-and-values)
      (read stream t nil 't)
    (declare (dynamic-extent initargs))
    (let* ((class (find-class class-name))
	   (instance (allocate-instance class))
	   (class-slot-names 
	    (mapcar #'slot-definition-name (class-slots class))))
      (loop for (slot-name value) on slots-and-values by #'cddr
	  do (setf class-slot-names (delq slot-name class-slot-names)) 
	     ;; Set the slot value, unless it is to remain unbound:
	     (unless (eq value unbound-value-indicator)
	       (setf (slot-value instance slot-name) value)))
      ;; Initialize any remaining slots:
      (shared-initialize instance class-slot-names)
      instance)))

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
      (safely-set-dispatch-macro-character #\# #\I 'standard-object-reader)
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
