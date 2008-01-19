;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/tools/read-object.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Jan 19 10:35:43 2008 *-*
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
            read-sent-object		; not yet documented
            with-reading-object-block))) ; not yet documented

;;; ===========================================================================

(defmacro with-reading-object-block ((&key) &body body)
  `(let ((*recorded-class-descriptions-ht* (make-hash-table :test 'eq)))
     ,@body))


;;; ===========================================================================
;;;  Dispatch-macro-character readers

(defun gbbopen-save/send-dispatch-reader (stream sub-char-infix-parameter)
  (declare (ignore sub-char infix-parameter))
  (funcall
   (ecase (read-char stream)
     (#\C 'class-description-reader)
     (#\H 'hash-table-reader)
     (#\I 'standard-object-reader)
     (#\U 'unbound-value-reader))
   stream))

;;; ---------------------------------------------------------------------------

(defun unbound-value-reader (stream)
  (declare (ignore stream))
  unbound-value-indicator)

;;; ---------------------------------------------------------------------------

(defun class-description-reader (stream)
  (destructuring-bind (class-name &rest slot-names)
      (read stream t nil 't)
    (setf (gethash class-name *recorded-class-descriptions-ht*)
          slot-names)))

;;; ---------------------------------------------------------------------------

(defun hash-table-reader (stream)
  (destructuring-bind (ht-test ht-count &rest initargs)
      (read stream t nil 't)
    (declare (dynamic-extent initargs))
    (let ((ht (make-hash-table :test ht-test :size ht-count)))
      (loop for (indicator value) on initargs by #'cddr
	  do (setf (gethash indicator ht) value))
      ht)))

;;; ---------------------------------------------------------------------------

(defun standard-object-reader (stream)
  (destructuring-bind (class-name &rest slot-values)
      (read stream t nil 't)
    (declare (dynamic-extent initargs))
    (let* ((class (find-class class-name))
           (class-slots (class-slots class))
	   (instance (allocate-instance class))
	   (incoming-class-slot-names 
            (or (copy-list 
                 (gethash class-name *recorded-class-descriptions-ht*))
                (error "No class description has been read for class ~s"
                       class-name))))
      (loop for slot-name in incoming-class-slot-names
          and value in slot-values
          do (setf incoming-class-slot-names
                   (delq slot-name incoming-class-slot-names))
             ;; Check that incoming slot-name is present in the current
             ;; class definition:
             (when (member slot-name class-slots
                           :key #'slot-definition-name
                           :test #'eq)
               ;; Set the slot value, unless it is to remain unbound:
               (unless (eq value unbound-value-indicator)
                 (setf (slot-value instance slot-name) value))))
      ;; Initialize any remaining slots:
      (when incoming-class-slot-names
	(shared-initialize instance incoming-class-slot-names))
      instance)))

;;; ===========================================================================
;;;  The saved/sent-object readtable

(defparameter *saved/sent-object-readtable*
    (let ((*readtable* (copy-readtable)))
      ;; Duplicate infinity reader (from declared-numerics.lisp):
      (safely-set-dispatch-macro-character #\# #\@ 
					   #-cormanlisp 'inf-reader
					   #+cormanlisp #'inf-reader)
      (safely-set-dispatch-macro-character #\# #\G 
                                           'gbbopen-save/send-dispatch-reader)
      *readtable*))

;;; ===========================================================================
;;;  With-saved/sent-object-syntax

(defmacro with-saved/sent-object-syntax ((&key (readtable
						'*saved/sent-object-readtable*))
					 &body body)
  `(let ((*readtable* ,readtable))
     ,@body))

;;; ===========================================================================
;;;  Temporary testing functions

(defun save-test (obj)
  (with-output-to-string (s)
    (with-saving/sending-block ()
      (print-object-for-saving obj s))))

(defun read-test (string)
  (with-reading-object-block ()
    (with-saved/sent-object-syntax ()
      (let ((eof-marker '#:eof)
            obj
            (position 0))
        (loop
          (multiple-value-setq (obj position)
            (read-from-string string nil eof-marker :start position))
          (when (eq obj eof-marker) (return))
          (describe obj))))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
