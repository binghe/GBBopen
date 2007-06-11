;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/1d-uniform-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 11 12:48:46 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 1d-Uniform-Buckets Space-Instance Storage
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
;;;  06-11-07 Converted 1d-uniform-buckets accessors from :prefix to modern
;;;           "-of" format.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::flag-set-p)))

;;; ===========================================================================
;;;  1D uniform-buckets storage (optimization of nD uniform-buckets storage)
;;;
;;;  Still to do: 
;;;    * add support for composite dimensions

(define-class 1d-uniform-buckets (storage)
  ((start)
   (size)
   (end)
   (unbound-value-instances :initform (make-hash-table :test 'eq))
   (preceeding-instances :initform (make-hash-table :test 'eq))
   (buckets :type (simple-array t (*)))
   (following-instances :initform (make-hash-table :test 'eq)))
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :after ((storage 1d-uniform-buckets)
                                     slot-names &rest initargs
                                     &key layout)
  (declare (ignore slot-names))
  (unless layout (missing-layout-option storage initargs))
  (let ((layout (ensure-list-of-lists layout)))
    (check-storage-dimensions/layout-lengths storage layout initargs)
    (destructuring-bind (start end size)
	(car layout)
      (let ((number-of-buckets (ceiling (- end start) size)))
	(setf (start-of storage) start)
	(setf (size-of storage) size)
	;; note that the effective end value can be larger than the specified 
	;; `end' value:
	(setf (end-of storage) 
	      (+ start (* number-of-buckets size)))
	;; Make the bucket vector:
	(setf (buckets-of storage)
	      (make-array (list number-of-buckets) :initial-element nil))))))

;;; ---------------------------------------------------------------------------

(defun print-1d-uniform-buckets-usage-message (buckets storage)
  (let ((dimension-name (sole-element (dimension-names-of storage))))
    (format *trace-output* 
	    "~&;; - ~s: Using 1D-uniform buckets "
	    dimension-name))
  (case buckets
    (:unbound-value
     (format *trace-output* "(unbound-value instances)~&"))
    (:preceeding
     (format *trace-output* "(preceeding instances)~&"))
    (:following
     (format *trace-output* "(following instances)~&"))
    (otherwise 
     (format *trace-output* "(~s bucket~:p)~&" buckets))))

;;; ---------------------------------------------------------------------------

(defun do-1d-uniform-buckets-add/remove-action (instance storage verbose
						dimension-values
						unbound-value-action
						preceeding-action
						bucket-action
						following-action)
  (declare (type function unbound-value-action preceeding-action
		 bucket-action following-action))
  (when verbose (print-storage-usage-message storage))
  (let* ((buckets (buckets-of storage))
	 (number-of-buckets (length buckets))
	 (start (start-of storage))
	 (size (size-of storage))
	 (dimension-name (sole-element (dimension-names-of storage))))
    (multiple-value-bind (dimension-value dimension-type composite-type
			  composite-dimension-name)
	(if dimension-values
	    (values (cdr (assoc dimension-name dimension-values :test #'eq)))
	    (instance-dimension-value instance dimension-name))
      (declare (ignore dimension-type composite-dimension-name))
      (flet ((do-a-value (dimension-value)
	       (cond
		;; scalar value:
		((numberp dimension-value)
		 (let ((bucket-index 
			(uniform-bucket-index dimension-value start size)))
		   (cond 
		    ;; preceeding instance:
		    ((minusp& bucket-index) 
		     (when verbose
		       (print-1d-uniform-buckets-usage-message 
			':preceeding storage))
		     (funcall preceeding-action instance storage))
		    ;; in a bucket:
		    ((not (>=& bucket-index number-of-buckets))
		     (when verbose
		       (print-1d-uniform-buckets-usage-message 1 storage))
		     (funcall bucket-action instance buckets bucket-index))
		    ;; following instance:
		    (t (when verbose
			 (print-1d-uniform-buckets-usage-message 
			  ':following storage))
		       (funcall following-action instance storage)))))
		;; interval value:
		(t (let ((start-index (uniform-bucket-index 
				       (interval-start dimension-value)
				       start size))
			 (end-index (uniform-bucket-index 
				     (interval-end dimension-value)
				     start size)))
		     ;; preceeding inverval:
		     (when (minusp& start-index) 
		       (when verbose
			 (print-1d-uniform-buckets-usage-message 
			  ':preceeding storage))
		       (funcall preceeding-action instance storage))
		     ;; in some buckets:
		     (when (and (not (minusp& end-index))
				(<& start-index number-of-buckets))
		       (let ((bucket-index (max& start-index 0))
			     (max-index (min& (1-& number-of-buckets) 
					      end-index)))
			 (when verbose
			   (print-1d-uniform-buckets-usage-message
			    (max& 1 (-& max-index bucket-index))
			    storage))
			 (do-until (funcall bucket-action instance 
					    buckets bucket-index)
			   (>& (incf& bucket-index) max-index))))
		     ;; following interval:
		     (when (>=& end-index number-of-buckets) 
		       (when verbose
			 (print-1d-uniform-buckets-usage-message 
			  ':following storage))
		       (funcall following-action instance storage)))))))
	(cond 
	 ;; unbound value:
	 ((eq dimension-value unbound-value-indicator)
	  (when verbose
	    (print-1d-uniform-buckets-usage-message 
	     ':unbound-value storage))
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
	 (t (do-a-value dimension-value)))))))

;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage ((instance standard-unit-instance)
				    (storage 1d-uniform-buckets) 
				    verbose)
  (let ((excess-count 0))
    (do-1d-uniform-buckets-add/remove-action 
	instance storage verbose nil
	;; unbound-value action:
	#'(lambda (instance storage)
	    (setf (gethash instance (unbound-value-instances-of storage))
		  instance)
	    (incf& excess-count))
	;; preceeding-action:
	#'(lambda (instance storage)
	    (setf (gethash instance (preceeding-instances-of storage))
		  instance)
	    (incf& excess-count))
	;; bucket-action:
	#'(lambda (instance buckets bucket-index)
	    (declare (type (simple-array t (*)) buckets))
	    (declare (type fixnum bucket-index))
	    (push instance (svref buckets bucket-index))
	    (incf& excess-count))
	;; following-action:
	#'(lambda (instance storage)
	    (setf (gethash instance (following-instances-of storage))
		  instance)
	    (incf& excess-count)))
    ;; save the excess count:
    (incf& (excess-locators-of storage) 
	   ;; remove the non-excess count for this instance:
	   (1-& excess-count))))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
					 (storage 1d-uniform-buckets) 
					 dimension-values
					 verbose)
  (let ((excess-count 0))
    (do-1d-uniform-buckets-add/remove-action 
	instance storage verbose dimension-values
	;; unbound-value action:
	#'(lambda (instance storage)
	    (remhash instance (unbound-value-instances-of storage))
	    (decf& excess-count))
	;; preceeding-action:
	#'(lambda (instance storage)
	    (remhash instance (preceeding-instances-of storage))
	    (decf& excess-count))
	;; bucket-action:
	#'(lambda (instance buckets bucket-index)
	    (declare (type (simple-array t (*)) buckets))
	    (declare (type fixnum bucket-index))
	    (setf (svref buckets bucket-index)
		  (delq instance (svref buckets bucket-index)))
	    (decf& excess-count))
	;; following-action:
	#'(lambda (instance storage)
	    (remhash instance (following-instances-of storage))
	    (decf& excess-count)))
    ;; save the excess count:
    (incf& (excess-locators-of storage) 
	   ;; add back in the non-excess count for this instance:
	   (1+& excess-count))))
  
;;; ---------------------------------------------------------------------------

(defun determine-1d-uniform-storage-extents (storage 
					     disjunctive-dimensional-extents)
  (let ((dimension-name (sole-element (dimension-names-of storage)))
	(extents nil))
    (unless (eq disjunctive-dimensional-extents 't)
      (dolist (dimensional-extents disjunctive-dimensional-extents)
	(let ((dimensional-extent (assoc dimension-name dimensional-extents
					 :test #'eq)))
	  (when dimensional-extent
	    (destructuring-bind (extent-dimension-name
				 dimension-type . new-extents)
		dimensional-extent
	      (declare (ignore extent-dimension-name dimension-type))
	      (dolist (new-extent new-extents)
		(setq extents (merge-ordered-extent new-extent extents)))))))
      ;; sort by increasing interval starts:
      (setq extents (sort extents #'< :key #'car)))
    (or extents 
	(values (list infinite-interval) 't))))

;;; ---------------------------------------------------------------------------

(defun do-1d-uniform-map-actions (fn storage disjunctive-dimensional-extents 
				  verbose)
  (declare (type function fn))
  (when verbose (print-storage-usage-message storage))
  (multiple-value-bind (storage-extents full-map-p)
      (determine-1d-uniform-storage-extents 
       storage disjunctive-dimensional-extents)
    (let* ((buckets (buckets-of storage))
	   (number-of-buckets (length buckets))
	   (start (start-of storage))
	   (size (size-of storage))
	   (end (end-of storage))
	   storage-region 
	   (unfinished-region-p nil)
	   (bucket-count 0))
      (declare (type (simple-array t (*)) buckets))
      ;; unbound instances req'd:
      (when full-map-p
	(when verbose
	  (print-1d-uniform-buckets-usage-message ':unbound-value storage))
	(incf& bucket-count)
	(maphash fn (unbound-value-instances-of storage)))
      (setq storage-region (pop storage-extents))
      ;; preceeding instances req'd:
      (when (and storage-region
		 (< (first storage-region) start))
	(when verbose
	  (print-1d-uniform-buckets-usage-message ':preceeding storage))
	(incf& bucket-count)
	(maphash fn (preceeding-instances-of storage)))
      ;; skip preceeding-bucket extents:
      (while (and storage-region
		  (< (second storage-region) start))
	(setq storage-region (pop storage-extents)))    
      ;; bucket instances req'd:
      (until (or (null storage-region)
		 (> (first storage-region) end))
	(let ((start-index (uniform-bucket-index 
			    (first storage-region)
			    start size))
	      (end-index (uniform-bucket-index 
			  (second storage-region)
			  start size)))
	  (when (and (not (minusp& end-index))
		     (<& start-index number-of-buckets))
	    (let ((bucket-index (max& start-index 0))
		  (max-index (min& (1-& number-of-buckets) end-index)))
	      (when verbose
		(print-1d-uniform-buckets-usage-message
		 (max& 1 (-& max-index bucket-index))
		 storage))
	      (do-until
		  (progn
		    (incf& bucket-count)
		    (dolist (instance 
				(svref buckets 
				       (the fixnum bucket-index)))
		      (funcall fn instance instance)))
		(>& (incf& bucket-index) max-index)))))
	(when (> (second storage-region) end)
	  (setq unfinished-region-p 't))
	(setq storage-region (pop storage-extents)))    
      ;; following instance req'd:
      (when (or storage-region unfinished-region-p)
	(when verbose
	  (print-1d-uniform-buckets-usage-message ':following storage))
	(incf& bucket-count)
	(maphash fn (following-instances-of storage)))
      ;; record the bucket count:
      (let ((find-stats *find-stats*))
	(when find-stats 
	  (incf (find-stats.bucket-count find-stats) bucket-count))))))

;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage 1d-uniform-buckets)
					    disjunctive-dimensional-extents 
					    mark-index verbose)
  (do-1d-uniform-map-actions 
      #'(lambda (key instance)
	  (declare (ignore key))
	  (let ((marks (standard-unit-instance.%%marks%% instance)))
	    (when (flag-set-p marks mark-index)
	      (funcall (the function fn) instance marks))))
    storage disjunctive-dimensional-extents verbose))

;;; ---------------------------------------------------------------------------

(defmethod map-all-instances-on-storage (fn (storage 1d-uniform-buckets)
					 disjunctive-dimensional-extents 
					 verbose)
  (do-1d-uniform-map-actions 
      #'(lambda (key instance)
	  (declare (ignore key))
	  (funcall (the function fn) instance))
    storage disjunctive-dimensional-extents verbose))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

