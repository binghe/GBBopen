;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/hashed-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Sep 23 21:50:53 2006 *-*
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
;;; Copyright (C) 2003-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-23-06 Split out from storage.lisp.  (Corkill)
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
  (:generate-accessors-format :prefix)
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :after ((storage hashed-storage)
                                     slot-names &rest initargs
                                     &key (test 'eql))
  (declare (ignore slot-names))
  (unless (memq test *hashed-test-functions*)
    (missing-test-option storage initargs))
  (setf (hashed-storage.bound-instances storage)
	(make-hash-table :test test)))
  
;;; ---------------------------------------------------------------------------

(defun print-hashed-storage-usage-message (storage)
  (print-storage-usage-message storage)
  (let ((hash-table (hashed-storage.bound-instances storage)))
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
       instance (sole-element (storage.dimension-names storage)))
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
	  (setf (gethash instance 
			 (hashed-storage.unbound-value-instances storage)) 
		instance))
       ;; bound-value action:
      #'(lambda (instance storage dimension-value)
	  (pushnew instance
		   (gethash dimension-value
			    (hashed-storage.bound-instances storage))
		   :test #'eq))))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
                                         (storage hashed-storage)
                                         dimension-values verbose)
  (declare (ignore dimension-values))
  (do-hashed-add/remove-action 
      instance storage verbose
      ;; unbound-value action:
      #'(lambda (instance storage)
	  (remhash instance (hashed-storage.unbound-value-instances storage)))
      ;; bound-value action:
      #'(lambda (instance storage dimension-value)
	  (setf (gethash dimension-value
			 (hashed-storage.bound-instances storage))
		(delq instance
		      (gethash dimension-value
			       (hashed-storage.bound-instances storage)))))))

;;; ---------------------------------------------------------------------------

(defun determine-hashed-storage-extents (storage 
					 disjunctive-dimensional-extents)
  (let ((dimension-name (sole-element (storage.dimension-names storage)))
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
	(bound-instances-hash-table
	 (hashed-storage.bound-instances storage)))
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
	(maphash action (hashed-storage.unbound-value-instances storage)))
      ;; record the bucket count:
      (let ((find-stats *find-stats*))
	(when find-stats 
	  (incf (find-stats.bucket-count find-stats) bucket-count))))))

;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage hashed-storage)
                                            disjunctive-dimensional-extents
					    mark-index verbose)
  (do-hashed-map-actions
      #'(lambda (key instance)
	  (declare (ignore key))
	  (let ((marks (standard-unit-instance.%%marks%% instance)))
	    (when (flag-set-p marks mark-index)
	      (funcall (the function fn) instance marks))))
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

