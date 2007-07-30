;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/print-object-for.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jul 25 13:03:51 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

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
;;; Copyright (C) 2007, Dan Corkill <corkill@GBBopen.org>
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
  (export '(print-object-for-saving     ; not yet documented
            print-object-for-sending))) ; not yet documented

;;; ===========================================================================

(defgeneric print-object-for-saving (object stream))
(defgeneric print-object-for-sending (object stream))

;;; ---------------------------------------------------------------------------
;;;  Default

(macrolet ((make (name)
             `(defmethod ,name (object stream)
                (prin1 object stream))))
  (make print-object-for-saving)
  (make print-object-for-sending))

;;; ---------------------------------------------------------------------------
;;;  Lists

(macrolet ((make (name)
             `(defmethod ,name ((cons cons) stream)
                (cond
                 ;; Compact (quote object) printing:
                 ((and (eq (first cons) 'quote)
                       (null (cddr cons))
                       (not (null (cdr cons))))
                  (princ "'" stream)
                  (,name (second cons) stream))
                 ;; Regular list printing:
                 (t (let ((ptr cons))
                      (princ "(" stream)
                      (,name (car ptr) stream)
                      (loop
                        (when (atom (setf ptr (cdr ptr))) (return))
                        (princ " " stream)
                        (,name (car ptr) stream))
                      (unless (null ptr)
                        (princ " . " stream)
                        (,name ptr stream))
                      (princ ")" stream))))
                cons)))
  (make print-object-for-saving)
  (make print-object-for-sending))

;;; ---------------------------------------------------------------------------
;;;  Vectors

(macrolet ((make (name)
             `(defmethod ,name ((vector vector) stream)
                (format stream "#(")
                (dotimes (i (length vector))
                  (declare (fixnum i))
                  (unless (zerop i) (princ " " stream))
                  (,name (aref vector i) stream))
                (princ ")" stream)
                vector)))
  (make print-object-for-saving)
  (make print-object-for-sending))

;;; ---------------------------------------------------------------------------
;;;  Arrays

(macrolet ((make (name)
             `(defmethod ,name ((array array) stream)
                (let ((dimensions (array-dimensions array))
                      (index -1))
                  (declare (fixnum index))
                  (labels ((helper (dimensions)
                             (cond 
                              ((null dimensions)
                               (,name (row-major-aref 
                                       array 
                                       (the fixnum (incf index)))
                                      stream))
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
                array)))
  (make print-object-for-saving)
  (make print-object-for-sending))

;;; ---------------------------------------------------------------------------
;;;  Structures

(macrolet ((make (name)
             `(defmethod ,name ((structure structure-object) stream)
                (let ((class (class-of structure)))
                  (format stream "#S(~s" (class-name class))
                  (dolist (slot (class-slots class))
                    (let ((slot-name (slot-definition-name slot)))
                      (format stream " :~a " slot-name)
                      (,name (slot-value structure slot-name) stream)))
                  (princ ")" stream))
                structure)))
  (make print-object-for-saving)
  (make print-object-for-sending))

;;; ---------------------------------------------------------------------------
;;;  Instances

(macrolet ((make (name)
             `(defmethod ,name ((instance standard-object) stream)
                (let ((class (class-of instance)))
                  (format stream "#I(~s" (class-name class))
                  (dolist (slot (class-slots class))
                    (let ((slot-name (slot-definition-name slot)))
                      (format stream " :~a " slot-name)
                      (if (slot-boundp instance slot-name)
                          (,name (slot-value instance slot-name) stream)
                          ;; Unbound value indicator:
                          (format stream "#_"))))
                  (princ ")" stream))
                instance)))
  (make print-object-for-saving)
  (make print-object-for-sending))

;;; ---------------------------------------------------------------------------
;;;  Hash Tables

(macrolet ((make (name)
             `(defmethod ,name ((hash-table hash-table) stream)
                (format stream "#H(~s ~s"
                        (hash-table-test hash-table)
                        (hash-table-count hash-table))
                (maphash #'(lambda (key value)
                             (format stream " ")
                             (,name key stream)
                             (format stream " ")
                             (,name value stream))
                         hash-table)
                (princ ")" stream)
                hash-table)))
  (make print-object-for-saving)
  (make print-object-for-sending))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
