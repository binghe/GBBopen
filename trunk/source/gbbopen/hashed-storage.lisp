;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/hashed-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jul  3 10:20:55 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    Hashed Space-Instance Storage
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-23-06 Split out from storage.lisp.  (Corkill)
;;;  06-11-07 Converted hashed-storage accessors from :prefix to modern
;;;           "-of" format.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::flag-set-p)))

;;; ---------------------------------------------------------------------------

(defparameter *hashed-test-functions* '(eq eql equal equalp))

;;; ===========================================================================
;;;  Hashed storage
;;;
;;;  Still to do: 
;;;    * add support for composite dimensions

(define-class hashed-storage (storage)
  ((bound-instances)                    ; initialized in shared-initialize
   (unbound-value-instances :initform (make-hash-table :test 'eq)))
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :after ((storage hashed-storage)
                                     slot-names &rest initargs
                                     &key (test 'eql))
  (declare (ignore slot-names))
  (unless (memq test *hashed-test-functions*)
    (missing-test-option storage initargs))
  (setf (bound-instances-of storage) (make-hash-table :test test)))
  
;;; ---------------------------------------------------------------------------

(defun print-hashed-storage-usage-message (storage)
  (print-storage-usage-message storage)
  (let ((hash-table (bound-instances-of storage)))
    (format *trace-output* 
	    "~&;; - ~s: Using ~s hashed storage (~s instance~:p)~&"
	    't 
	    (hash-table-test hash-table)
	    (hash-table-count hash-table))))

;;; ---------------------------------------------------------------------------

(defun do-hashed-add/remove-action (instance storage verbose
				    unbound-value-action
				    bound-value-action)
  (declare (type function unbound-value-action bound-value-action))
  (when verbose (print-hashed-storage-usage-message storage))
  (multiple-value-bind (dimension-value dimension-type composite-type
			composite-dimension-name)
      (instance-dimension-value 
       instance (sole-element (dimension-names-of storage)))
    (declare (ignore dimension-type composite-dimension-name))
    (flet ((do-a-value (dimension-value)
	     (funcall bound-value-action instance storage dimension-value)))
      (cond 
       ;; unbound value:
       ((eq dimension-value unbound-value-indicator)
	(funcall unbound-value-action instance storage))
       ;; set composite dimension value:
       ((eq composite-type ':set)
	(map nil #'do-a-value dimension-value))
       ;; sequence composite dimension value:
       ((eq composite-type ':sequence)
	(nyi))
       ;; series composite dimension value:
       ((eq composite-type ':series)
	(nyi))
       ;; incomposite dimension value:
       (t (do-a-value dimension-value))))))
  
;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage ((instance standard-unit-instance)
                                    (storage hashed-storage)
                                    verbose)
  (do-hashed-add/remove-action 
      instance storage verbose
      ;; unbound-value action:
      #'(lambda (instance storage)
	  (setf (gethash instance (unbound-value-instances-of storage)) 
		instance))
       ;; bound-value action:
      #'(lambda (instance storage dimension-value)
	  (pushnew instance
		   (gethash dimension-value (bound-instances-of storage))
		   :test #'eq))))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
                                         (storage hashed-storage)
                                         old-dimension-values 
                                         dimensions-being-changed
                                         verbose)
  (declare (ignore old-dimension-values dimensions-being-changed))
  (do-hashed-add/remove-action 
      instance storage verbose
      ;; unbound-value action:
      #'(lambda (instance storage)
	  (remhash instance (unbound-value-instances-of storage)))
      ;; bound-value action:
      #'(lambda (instance storage dimension-value)
	  (setf (gethash dimension-value (bound-instances-of storage))
		(delq instance
		      (gethash dimension-value
                               (bound-instances-of storage)))))))

;;; ---------------------------------------------------------------------------

(defun determine-hashed-storage-extents (storage 
					 disjunctive-dimensional-extents)
  (let ((dimension-name (sole-element (dimension-names-of storage)))
	(extents nil))
    (cond 
     ((eq disjunctive-dimensional-extents 't)
      (values nil 't))
     (t (dolist (dimensional-extents disjunctive-dimensional-extents)
	  (let ((dimensional-extent (assoc dimension-name dimensional-extents
					   :test #'eq)))
	    (when dimensional-extent
	      (destructuring-bind (extent-dimension-name
				   dimension-type . new-extents)
		  dimensional-extent
		(declare (ignore extent-dimension-name dimension-type))
		;; Support for composite extents (listed) is still needed:
		(setq extents (pushnew new-extents extents :test #'eq))))))
	(or extents
	    (values nil 't))))))

;;; ---------------------------------------------------------------------------

(defun do-hashed-map-actions (action storage disjunctive-dimensional-extents
			      verbose)
  ;; disjunctive-dimensional-extents for a hashed can contain unbound-value
  ;; or one or more bound values:
  (declare (type function action))
  (when verbose (print-hashed-storage-usage-message storage))
  (let ((bucket-count 0)
	(bound-instances-hash-table (bound-instances-of storage)))
    (multiple-value-bind (storage-extents full-map-p)
	(determine-hashed-storage-extents 
	 storage disjunctive-dimensional-extents)
      (declare (type list storage-extents))
      (flet ((map-bucket (key bucket)
	       (declare (ignore key))
	       (incf& bucket-count)
	       (dolist (instance bucket)
		 (funcall action instance instance))))
	(cond (full-map-p
	       (maphash #'map-bucket bound-instances-hash-table))		
	      (t (dolist (storage-extent storage-extents)
		   (map-bucket storage-extent 
			       (gethash storage-extent 
					bound-instances-hash-table))))))
      (when (or full-map-p
		(memq unbound-value-indicator storage-extents))
	(incf& bucket-count)
	(maphash action (unbound-value-instances-of storage)))
      ;; record the bucket count:
      (let ((find-stats *find-stats*))
	(when find-stats 
	  (incf (find-stats.bucket-count find-stats) bucket-count))))))

;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage hashed-storage)
                                            disjunctive-dimensional-extents
                                            verbose)
  (do-hashed-map-actions
      #'(lambda (key instance)
	  (declare (ignore key))
          (when (mbr-instance-mark-set-p instance)
            (funcall (the function fn) instance)))
    storage disjunctive-dimensional-extents verbose))

;;; ---------------------------------------------------------------------------

(defmethod map-all-instances-on-storage (fn (storage hashed-storage)
                                         disjunctive-dimensional-extents
					 verbose)
  (do-hashed-map-actions 
      #'(lambda (key instance)
	  (declare (ignore key))
	  (funcall (the function fn) instance))
    storage disjunctive-dimensional-extents verbose))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

