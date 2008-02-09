;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/tools/print-object-for.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Feb  7 03:50:51 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *             Print Object For Saving & For Sending Support
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2007-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-07 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clos:class-slots
            clos:slot-definition-name)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*print-object-for-sending*  ; not yet documented
            *save/send-references-only* ; not yet documented
            omitted-slots-for-saving/sending
            print-object-for-saving/sending
            print-slot-for-saving/sending
            with-saving/sending-block)))

;;; ---------------------------------------------------------------------------

(defgeneric omitted-slots-for-saving/sending (instance))
(defgeneric print-object-for-saving/sending (object stream))
(defgeneric print-slot-for-saving/sending (instance slot-name stream))

;;; ---------------------------------------------------------------------------

(defvar *print-object-for-sending* nil)
(defvar *save/send-references-only* 't)

;;; ---------------------------------------------------------------------------
;;;  Saving/sending-block format version

(defparameter *saving/sending-block-format-version* "1.0")

;;; ---------------------------------------------------------------------------

(defun write-saving/sending-block-info (stream)
  ;; Record the format-version, time, *package*, and
  ;; *read-default-float-format* for reading by read-saving/sending-block-info
  ;; (below) which is called by with-reading-object-block:
  (format stream "(~s ~s ~s ~s)~%" 
          *saving/sending-block-format-version*
          (get-universal-time)
          ;; save package-name as keyword symbol, to avoid package/modern-mode
          ;; issues:
          (make-keyword (package-name *package*))
          *read-default-float-format*))

;;; ---------------------------------------------------------------------------

(defun read-saving/sending-block-info (stream)
  ;; Read the format-version, time, *package*, and *read-default-float-format*
  ;; recorded by with-saving/sending-block, returning the time:
  (destructuring-bind (format-version date-written package-name 
                       read-default-float-format)
      (read stream)
    (unless (string= format-version *saving/sending-block-format-version*)
      (warn "Reading old ~s format version ~a (the current version is ~a)"
            'with-saving/sending-block
            format-version
            *saving/sending-block-format-version*))
    (setf *package* (ensure-package package-name))
    (setf *read-default-float-format* read-default-float-format)
    date-written))

;;; ===========================================================================
;;;  With-sending/saving-block

(defvar *recorded-class-descriptions-ht*)

(defmacro with-saving/sending-block ((stream 
                                      &key (package '':cl-user)
                                           (read-default-float-format 
                                            ''single-float))
                                     &body body)
  (with-gensyms (package-sym)
    `(with-standard-io-syntax 
       (let ((*recorded-class-descriptions-ht* (make-hash-table :test 'eq))
             (stream ,stream))
         (setf *package*
               (let ((,package-sym ,package))
                 (if (packagep ,package-sym)
                     ,package-sym
                     (ensure-package ,package-sym))))
         (setf *read-default-float-format* ,read-default-float-format)
         (write-saving/sending-block-info stream)
               ,@body))))

;;; ---------------------------------------------------------------------------

(defun outside-saving/sending-block-error (function-name)
  (error "Call to ~s is not within a ~s form"
         function-name
         'with-saving/sending-block))

;;; ===========================================================================
;;;  Slots-for-saving/sending methods

(defun slots-for-saving/sending (instance stream)
  ;; Caches the slots that should be saved/sent for `class', saving/sending
  ;; the class description, if we've not done so in the save/send block.
  ;; Always returns the list of slots that should be saved/sent.
  (let* ((class (class-of instance))
         (class-name (class-name class)))
    ;; Check that we are in a with-saving/sending-block:
    (unless (boundp '*recorded-class-descriptions-ht*)
      (outside-saving/sending-block-error 'slots-for-saving/sending))
    (multiple-value-bind (slots-for-saving/sending present-p)
        (gethash class-name *recorded-class-descriptions-ht*)
      ;; Determine the slots that should be saved/sent and save/send the class
      ;; description, if we've not done so already in this block:
      (unless present-p
        (let ((omitted-slot-names (omitted-slots-for-saving/sending instance)))
          (setf slots-for-saving/sending
                (setf (gethash class-name *recorded-class-descriptions-ht*)
                      (remove-if 
                       #'(lambda (slot)
                           (memq (slot-definition-name slot)
                                 omitted-slot-names))
                       (class-slots class)))))
        (print-object-for-saving/sending class stream))
      slots-for-saving/sending)))

;;; ---------------------------------------------------------------------------
;;;  Default

(defmethod omitted-slots-for-saving/sending (object)
  (declare (ignore object))
  nil)

;;; ===========================================================================
;;;  Standard print-object-for-saving/sending methods
;;;
;;; ---------------------------------------------------------------------------
;;;  Default

(defmethod print-object-for-saving/sending (object stream)
  (prin1 object stream))

;;; ---------------------------------------------------------------------------
;;;  Lists

(defmethod print-object-for-saving/sending ((cons cons) stream)
  (cond
   ;; Compact (quote object) printing:
   ((and (eq (first cons) 'quote)
         (null (cddr cons))
         (not (null (cdr cons))))
    (princ "'" stream)
    (print-object-for-saving/sending (second cons) stream))
   ;; Regular list printing:
   (t (let ((ptr cons))
        (princ "(" stream)
        (print-object-for-saving/sending (car ptr) stream)
        (loop
          (when (atom (setf ptr (cdr ptr))) (return))
          (princ " " stream)
          (print-object-for-saving/sending (car ptr) stream))
        (unless (null ptr)
          (princ " . " stream)
          (print-object-for-saving/sending ptr stream))
        (princ ")" stream))))
  cons)

;;; ---------------------------------------------------------------------------
;;;  Strings

(defmethod print-object-for-saving/sending ((string string) stream)
  (prin1 string stream))

;;; ---------------------------------------------------------------------------
;;;  Vectors

(defmethod print-object-for-saving/sending ((vector vector) stream)
  (format stream "#(")
  (dotimes (i (length vector))
    (declare (fixnum i))
    (unless (zerop i) (princ " " stream))
    (print-object-for-saving/sending (aref vector i) stream))
  (princ ")" stream)
  vector)

;;; ---------------------------------------------------------------------------
;;;  Bit-vectors

(defmethod print-object-for-saving/sending ((bit-vector bit-vector) stream)
  (prin1 bit-vector stream))

;;; ---------------------------------------------------------------------------
;;;  Arrays (simple only--someday support extended syntax for arrays with
;;;          a fill-pointer, specialized element-type, etc.)

(defmethod print-object-for-saving/sending ((array array) stream)
  (let ((dimensions (array-dimensions array))
        (index -1))
    (declare (fixnum index))
    (labels
        ((helper (dimensions)
           (cond 
            ((null dimensions)
             (print-object-for-saving/sending
              (row-major-aref array (the fixnum (incf index))) stream))
            (t (let ((dimension (first dimensions)))
                 (princ "(" stream)
                 (dotimes (i dimension)
                   (declare (fixnum i))
                   (unless (zerop i) (princ " " stream))
                   (helper (rest dimensions)))
                 (princ ")" stream))))))
      (format stream "#~sA" (array-rank array))
      (helper dimensions)
      (princ " " stream)))
  array)

;;; ---------------------------------------------------------------------------
;;;  Structures

(defmethod print-object-for-saving/sending ((structure structure-object) 
                                            stream)
  (let* ((slots-for-saving/sending 
          ;; Cache/write the class description, if needed; always get the
          ;; slots to be written:
          (slots-for-saving/sending structure stream)))
    (format stream "#S(~s" (type-of structure))
    (dolist (slot slots-for-saving/sending)
      (let ((slot-name (slot-definition-name slot)))
        (format stream " :~a " slot-name)
        (print-object-for-saving/sending
         (slot-value structure slot-name) stream)))
    (princ ")" stream))
  structure)

;;; ---------------------------------------------------------------------------
;;;  Class Descriptions

(defmethod print-object-for-saving/sending ((class standard-class) stream)
  ;; Check that we are in a with-saving/sending-block:
  (unless (boundp '*recorded-class-descriptions-ht*)
    (outside-saving/sending-block-error 'print-object-for-saving/sending))
  (let ((class-name (class-name class)))
    (multiple-value-bind (slots-for-saving/sending present-p)
        (gethash class-name *recorded-class-descriptions-ht*)
      (unless present-p
        (error "Class ~s has not been recorded in this saving/sending block"))
      (format stream "~&#GC(~s" class-name)
      (dolist (slot slots-for-saving/sending)
        (let ((slot-name (slot-definition-name slot)))
          (format stream " ~s" slot-name)))
      (princ ")" stream)))
  class)

;;; ---------------------------------------------------------------------------
;;;  Instances

(defmethod print-slot-for-saving/sending ((instance standard-object)
                                          slot-name
                                          stream)
  (print-object-for-saving/sending (slot-value instance slot-name) stream))

;;; ---------------------------------------------------------------------------

(defmethod print-object-for-saving/sending ((instance standard-object) stream)
  (let ((slots-for-saving/sending 
         ;; Cache/write the class description, if needed; always get the
         ;; slots to be written:
         (slots-for-saving/sending instance stream)))
    (format stream "~&#GI(~s" (type-of instance))
    (dolist (slot slots-for-saving/sending)
      (princ " " stream)
      (if (slot-boundp-using-class (class-of instance) instance slot)
          (print-slot-for-saving/sending 
           instance (slot-definition-name slot) stream)
          ;; Unbound value indicator:
          (format stream "#GU")))
    (princ ")" stream))
  instance)

;;; ---------------------------------------------------------------------------
;;;  Hash Tables

(defmethod print-object-for-saving/sending ((hash-table hash-table) stream)
  (let ((keys-and-values-hash-table? 
         #+allegro (excl:hash-table-values hash-table)
         #-allegro 't))
    (format stream "#GH(~s ~s ~s"
            (hash-table-test hash-table)
            (hash-table-count hash-table)
            keys-and-values-hash-table?)
    (maphash 
     (if keys-and-values-hash-table? 
         #'(lambda (key value)
             (format stream " ")
             (print-object-for-saving/sending key stream)
             (format stream " ")
             (print-object-for-saving/sending value stream))
         #'(lambda (key value)
             (declare (ignore value))
             (format stream " ")
             (print-object-for-saving/sending key stream)))
     hash-table)
    (princ ")" stream)
    hash-table))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
