;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/tools/read-object.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Jan 26 05:56:47 2008 *-*
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
            with-reading-object-block))) ; not yet documented

;;; ===========================================================================
;;;  Standard GBBopen save/send reader methods

(defgeneric gbbopen-save/send-object-reader (char stream))
(defgeneric allocate-gbbopen-save/send-instance (class slots slot-values))
(defgeneric initialize-gbbopen-save/send-instance 
    (instance incoming-slots slot-values missing-slot-names))

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
    (let* ((class (find-class class-name 't))
           (class-slots (copy-list (progn
                                     (ensure-finalized-class class)
                                     (class-slots class))))
           (supplied-slots nil))
      (dolist (slot-name slot-names)
        ;; Check that incoming slot-name is present in the current class
        ;; definition:
        (let ((slot (find slot-name class-slots
                          :key #'slot-definition-name
                          :test #'eq)))
          (unless slot
            (warn "Slot ~s is no longer defined for class ~s; saved values ~
                   for this slot will be discarded."
                  slot-name class-name))
          (push slot supplied-slots)
          (setf class-slots (delq slot class-slots))))
      ;; Save the incoming class-slots and the missing slot-names for this
      ;; class:
      (setf (gethash class-name *recorded-class-descriptions-ht*)
            (list (nreverse supplied-slots)
                  (mapcar #'slot-definition-name class-slots))))))

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
                                                slots slot-values)
  (declare (ignore slots slot-values))
  (allocate-instance class))

;;; ---------------------------------------------------------------------------

(defvar *%skip-gbbopen-shared-initialize-method-processing%* nil)

(defmethod initialize-gbbopen-save/send-instance
    ((instance standard-object) slots slot-values missing-slot-names)
  (let ((class (class-of instance)))
    (loop for slot in slots
        and value in slot-values
        do (when slot
             ;; Set the slot value, unless it is to remain unbound:
             (unless (eq value unbound-value-indicator)
               ;; Lispworks can't use slot-value-using-class here
               ;; (prior to shared-initialize?):
               #+lispworks
               (setf (slot-value instance (slot-definition-name slot)) value)
               #-lispworks
               (setf (slot-value-using-class class instance slot) 
                     value))))
    ;; Initialize any remaining slots:
    (when missing-slot-names
      (let ((*%skip-gbbopen-shared-initialize-method-processing%* 't))
        (shared-initialize instance missing-slot-names)))))

;;; ---------------------------------------------------------------------------

(defmethod gbbopen-save/send-object-reader ((char (eql #\I)) stream)
  (destructuring-bind (class-name &rest slot-values)
      (read stream t nil 't)
    (declare (dynamic-extent slot-values))
    (destructuring-bind (incoming-slots missing-slot-names)
        (or (gethash class-name *recorded-class-descriptions-ht*)
            (error "No class description has been read for class ~s"
                   class-name))
      (let ((instance (allocate-gbbopen-save/send-instance 
                       (find-class class-name 't)
                       incoming-slots
                       slot-values)))
        (initialize-gbbopen-save/send-instance 
         instance incoming-slots slot-values missing-slot-names)
        instance))))

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
;;;  With-reading-object-block

;; Dynamically bound in with-reading-object-block to maintain instances that
;; have been referenced but not yet read:
(defvar *forward-referenced-saved/sent-instances*)

;; Dynamically bound in with-reading-object-block to hold the universal-time
;; value recorded by with-saving/sending-block:
(defvar *block-written-time*)

;;; ---------------------------------------------------------------------------

(defun check-for-undefined-instance-references ()
  (when (plusp& (hash-table-count *forward-referenced-saved/sent-instances*))
    (warn "These instances were referenced and never defined: ~s"
          (loop for key being the hash-keys 
              of *forward-referenced-saved/sent-instances* 
              collect key))))

;;; ---------------------------------------------------------------------------

(defmacro with-reading-object-block ((stream 
                                      &key (readtable
                                            '*saved/sent-object-readtable*)
                                           (read-eval nil))
                                     &body body)
  `(with-standard-io-syntax 
     (let ((*block-written-time*
            ;; Note that read-saving/sending-block-info also sets
            ;; *package* and *read-default-float-format*:
            (read-saving/sending-block-info ,stream))
           (*recorded-class-descriptions-ht* (make-hash-table :test 'eq))
           (*forward-referenced-saved/sent-instances* 
            (make-keys-only-hash-table-if-supported :test 'equal)))
     (setf *readtable* ,readtable)           
     (setf *read-eval* ,read-eval)
       (multiple-value-prog1
           (progn ,@body)
         (check-for-undefined-instance-references)))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
