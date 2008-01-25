;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/gbbopen/unstructured-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jan 25 03:54:52 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  Unstructured Space-Instance Storage
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-23-06 Split out from storage.lisp.  (Corkill)
;;;  06-11-07 Converted unstructured-storage accessors from :prefix to modern
;;;           "-of" format.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::flag-set-p)))

;;; ===========================================================================
;;;  Unstructured storage

(define-class unstructured-storage (storage)
  ((instances :initform (make-hash-table 
                         :test 'eq
                         ;; Use Allegro's sans-value hash tables:
                         #+allegro :values #+allegro nil)))
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defun print-unstructured-storage-usage-message (storage)
  (print-storage-usage-message storage)
  (format *trace-output* 
          "~&;; - ~s: Using unstructured storage (~s instance~:p)~&"
          't (hash-table-count (instances-of storage))))

;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage ((instance standard-unit-instance)
                                    (storage unstructured-storage)
                                    verbose)
  (when verbose (print-unstructured-storage-usage-message storage))
  (setf (gethash instance (instances-of storage)) 't))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
                                         (storage unstructured-storage)
                                         old-dimension-values 
                                         dimensions-being-changed verbose)
  (declare (ignore old-dimension-values dimensions-being-changed))
  (when verbose (print-unstructured-storage-usage-message storage))
  (remhash instance (instances-of storage)))

;;; ---------------------------------------------------------------------------

(defun do-unstructured-map-actions (fn storage 
				    disjunctive-dimensional-extents verbose)
  (declare (ignore disjunctive-dimensional-extents))
  (declare (type function fn))
  (when verbose (print-unstructured-storage-usage-message storage))  
  (maphash fn (instances-of storage))
  ;; record the bucket count:
  (let ((find-stats *find-stats*))
    (when find-stats 
      (incf (find-stats.bucket-count find-stats)))))

;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage unstructured-storage)
                                            disjunctive-dimensional-extents
                                            verbose)
  (do-unstructured-map-actions 
      #'(lambda (instance value)
	  (declare (ignore value))
          (when (mbr-instance-mark-set-p instance)
            (funcall (the function fn) instance)))
    storage disjunctive-dimensional-extents verbose))

;;; ---------------------------------------------------------------------------

(defmethod map-all-instances-on-storage (fn (storage unstructured-storage)
                                         disjunctive-dimensional-extents
					 verbose)
  (do-unstructured-map-actions 
      #'(lambda (instance value)
	  (declare (ignore value))
	  (funcall (the function fn) instance))
    storage disjunctive-dimensional-extents verbose))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

