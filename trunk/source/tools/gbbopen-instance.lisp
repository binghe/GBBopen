;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/gbbopen-instance.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Dec 27 10:16:05 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      Standard GBBopen Instance
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-06-06 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(print-instance-slots
            standard-gbbopen-instance)))

(defgeneric print-instance-slots (instance stream))

;;; ---------------------------------------------------------------------------

(define-class standard-gbbopen-instance ()
  ()
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((instance standard-gbbopen-instance) stream)
  (cond (*print-readably* (call-next-method))
	(t (print-unreadable-object (instance stream :type nil)
	     (print-instance-slots instance stream))
	   ;; Print-object must return object:
	   instance)))

;;; ---------------------------------------------------------------------------
;;;  Print instance slots (extension of print-object method for
;;;  standard-gbbopen-instance objects)

(defmethod print-instance-slots ((instance standard-gbbopen-instance) stream)
  (format stream "~s" (type-of instance))
  nil)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


