;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/duplicate-instance.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon May 19 09:53:37 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                          Duplicate Instance
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
;;;  05-18-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-duplicate-instance     ; not yet documented
            unduplicated-slot-names     ; not yet documented
            )))

;;; ===========================================================================

(defgeneric initialize-duplicated-instance (instance slots slot-values 
                                            missing-slot-names))
(defgeneric make-duplicate-instance (instance unduplicated-slot-names
                                     &rest initargs))
(defgeneric unduplicated-slot-names (instance))

;;; ---------------------------------------------------------------------------

(defmethod unduplicated-slot-names ((instance standard-object))
  nil)

;;; ---------------------------------------------------------------------------

(defmethod initialize-duplicated-instance ((instance standard-object)
                                           slots slot-values 
                                           missing-slot-names)
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
      (let ((*%%skip-gbbopen-shared-initialize-method-processing%%* 't))
        (shared-initialize instance missing-slot-names)))))

;;; ---------------------------------------------------------------------------

(defun lookup-initarg-value (indicators initargs not-found-value)
  ;; Return the value associated with an indicator (in `indicators') from
  ;; `initargs' or `not-found-value' if none is found:
  (dolist (indicator indicators not-found-value)
    (let ((result (getf initargs indicator not-found-value)))
      (unless (eq result not-found-value)
        (return-from lookup-initarg-value result)))))

;;; ---------------------------------------------------------------------------

(defmethod make-duplicate-instance ((instance standard-object) 
                                    unduplicated-slot-names
                                    &rest initargs)
  (declare (dynamic-extent initargs))
  (let* ((class (class-of instance))
         (new-instance (allocate-instance class))
         (slots nil)
         (slot-values nil)
         (missing-slot-names nil)
         (not-found-value ""))
    (setf unduplicated-slot-names 
          (append unduplicated-slot-names
                  (unduplicated-slot-names instance)))
    (dolist (slot (class-slots class))
      (let ((slot-name (slot-definition-name slot))
            ;; See if we are setting via an initarg:
            (maybe-initarg-value
             (lookup-initarg-value 
              (slot-definition-initargs slot) initargs not-found-value)))
        (cond 
         ;; An initarg was specified for the slot:
         ((not (eq maybe-initarg-value not-found-value))
          (push slot slots)
          (push maybe-initarg-value slot-values))
         ;; Record as a missing-slot-name, if asked not to duplicate:
         ((memq slot-name unduplicated-slot-names)
          (push slot-name missing-slot-names))
         ;; Not an initarg or an unduplicated-slot-name, use the slot-value:
         (t (push slot slots)
            (push (if (slot-boundp-using-class class instance slot)
                      (slot-value-using-class class instance slot)
                      unbound-value-indicator)
                  slot-values)))))
    (initialize-duplicated-instance 
     new-instance slots slot-values missing-slot-names)
    (values new-instance slots)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
