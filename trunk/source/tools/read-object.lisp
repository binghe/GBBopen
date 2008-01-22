;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/tools/read-object.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jan 22 04:01:39 2008 *-*
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
  (export '(*forward-referenced-saved/sent-instances* ; not yet documented
            allocate-gbbopen-save/send-instance ; not yet documented
            gbbopen-save/send-object-reader ; not yet documented
            initialize-gbbopen-save/send-instance ; not yet documented
            with-reading-object-block   ; not yet documented
            with-saved/sent-object-syntax))) ; not yet documented

;;; ---------------------------------------------------------------------------

;; Dynamically bound in with-reading-object-block to maintain instances that
;; have been referenced but not yet read:

(defvar *forward-referenced-saved/sent-instances*)

;;; ===========================================================================

(defmacro with-reading-object-block ((&key) &body body)
  `(let ((*recorded-class-descriptions-ht* (make-hash-table :test 'eq))
         (*forward-referenced-saved/sent-instances* 
          #+allegro (make-hash-table :test 'equal :values nil)
          #-allegro (make-hash-table :test 'equal)))
     (multiple-value-prog1 (progn ,@body)
       (when (plusp& (hash-table-count 
                      *forward-referenced-saved/sent-instances*))
         (warn "These instances were referenced and never defined: ~s"
               (loop for key being the hash-keys of
                         *forward-referenced-saved/sent-instances* 
                   collect key))))))

;;; ===========================================================================
;;;  Standard GBBopen save/send reader methods

(defgeneric gbbopen-save/send-object-reader (char stream))
(defgeneric allocate-gbbopen-save/send-instance 
    (class slot-names slot-values))
(defgeneric initialize-gbbopen-save/send-instance 
    (instance incoming-class-slot-names slot-values))

;;; ---------------------------------------------------------------------------
;;;  Default (error) reader

(defmethod gbbopen-save/send-object-reader (char stream)
  (declare (ignore stream))
  (error "No GBBopen save/send-object reader defined for #G~c" char))

;;; ---------------------------------------------------------------------------
;;;  Unbound value reader
         
(defmethod gbbopen-save/send-object-reader ((char (eql #\U)) stream)
  (declare (ignore stream))
  unbound-value-indicator)

;;; ---------------------------------------------------------------------------
;;;  Class-description reader

(defmethod gbbopen-save/send-object-reader ((char (eql #\C)) stream)
  (destructuring-bind (class-name &rest slot-names)
      (read stream t nil 't)
    (setf (gethash class-name *recorded-class-descriptions-ht*)
          slot-names)))

;;; ---------------------------------------------------------------------------
;;;  Hash-table reader

(defmethod gbbopen-save/send-object-reader ((char (eql #\H)) stream)
  (destructuring-bind (ht-test ht-count ht-values &rest keys-and-values)
      (read stream t nil 't)
    (declare (dynamic-extent keys-and-values))
    (let ((ht (if ht-values 
                  (make-hash-table :test ht-test :size ht-count)
                  (make-hash-table :test ht-test :size ht-count :values nil))))
      (if ht-values 
          (loop for (key value) on keys-and-values by #'cddr
              do (setf (gethash key ht) value))
          (dolist (key keys-and-values)
            (setf (gethash key ht) 't)))
      ht)))

;;; ---------------------------------------------------------------------------
;;;  Standard-object reader

(defmethod allocate-gbbopen-save/send-instance ((class standard-class)
                                                slot-names slot-values)
  (declare (ignore slot-names slot-values))
  (allocate-instance class))

;;; ---------------------------------------------------------------------------

(defmethod initialize-gbbopen-save/send-instance
    ((instance standard-object) incoming-class-slot-names slot-values)
  (let* ((class (class-of instance))
         (class-slots (class-slots class)))
    ;; Allow destructive (delq) removal below:
    (setf incoming-class-slot-names (copy-list incoming-class-slot-names))
    (loop for slot-name in incoming-class-slot-names
        and value in slot-values
        do (setf incoming-class-slot-names
                 (delq slot-name incoming-class-slot-names))
           ;; Check that incoming slot-name is present in the current
           ;; class definition:
           (let ((slot (find slot-name class-slots
                             :key #'slot-definition-name
                             :test #'eq)))
             (when slot
               ;; Set the slot value, unless it is to remain unbound:
               (unless (eq value unbound-value-indicator)
                 (setf (slot-value-using-class class instance slot) 
                       value)))))
    ;; Initialize any remaining slots:
    (when incoming-class-slot-names
      (shared-initialize instance incoming-class-slot-names))))

;;; ---------------------------------------------------------------------------

(defmethod gbbopen-save/send-object-reader ((char (eql #\I)) stream)
  (destructuring-bind (class-name &rest slot-values)
      (read stream t nil 't)
    (declare (dynamic-extent slot-values))
    (let* ((incoming-class-slot-names 
            (or (gethash class-name *recorded-class-descriptions-ht*)
                (error "No class description has been read for class ~s"
                       class-name)))
           (instance (allocate-gbbopen-save/send-instance 
                      (find-class class-name 't)
                      incoming-class-slot-names
                      slot-values)))
      (initialize-gbbopen-save/send-instance 
       instance incoming-class-slot-names slot-values)
      instance)))

;;; ===========================================================================
;;;  The saved/sent-object readtable

(defun gbbopen-save/send-reader-dispatcher (stream sub-char infix-parameter)
  (declare (ignore sub-char infix-parameter))
  (gbbopen-save/send-object-reader (read-char stream) stream))

;;; ---------------------------------------------------------------------------

(defparameter *saved/sent-object-readtable*
    (let ((*readtable* (copy-readtable)))
      ;; Duplicate infinity reader (from declared-numerics.lisp):
      (safely-set-dispatch-macro-character
       #\# #\@ 
       #-cormanlisp 'inf-reader
       #+cormanlisp #'inf-reader)
      (safely-set-dispatch-macro-character 
       #\# #\G 
       'gbbopen-save/send-reader-dispatcher)
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
