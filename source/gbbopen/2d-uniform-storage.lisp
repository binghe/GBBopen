;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/2d-uniform-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Sep 23 21:50:09 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *               2d-Uniform-Buckets Space-Instance Storage
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

;;; ===========================================================================
;;;  2D uniform-buckets storage (unrolled optimization of nD uniform-buckets)
;;;
;;;  Still to do: 
;;;    * add bucket-extent merging
;;;    * handle a fully infeasible extent (without a full sweep!)
;;;    * add support for composite dimensions

(define-class 2d-uniform-buckets (storage)
  ((x-start)
   (x-size)
   (number-of-x-buckets)
   (y-start)
   (y-size)
   (number-of-y-buckets)
   (buckets :type (simple-array t (* *)))
   (unbound-value-instances :initform (make-hash-table :test 'eq)))
  (:generate-accessors-format :prefix)
  (:generate-initargs nil)
  (:export-class-name t))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :after ((storage 2d-uniform-buckets)
                                     slot-names &rest initargs
				     &key layout)
  (declare (ignore slot-names))
  (unless layout (missing-layout-option storage initargs))
  (let ((layout (ensure-list-of-lists layout)))
    (check-storage-dimensions/layout-lengths storage layout initargs)
    ;; 1st "X" dimension:
    (destructuring-bind (start end size)
	(first layout)
      (let ((number-of-x-buckets (ceiling (- end start) size)))
	(setf (2d-uniform-buckets.x-start storage) start)
	(setf (2d-uniform-buckets.x-size storage) size)
	(setf (2d-uniform-buckets.number-of-x-buckets storage)
	      number-of-x-buckets)
	;; 2nd "Y" dimension:
	(destructuring-bind (start end size)
	    (second layout)
	  (let ((number-of-y-buckets (ceiling (- end start) size)))
	    (setf (2d-uniform-buckets.y-start storage) start)
	    (setf (2d-uniform-buckets.y-size storage) size)
	    (setf (2d-uniform-buckets.number-of-y-buckets storage)
		  number-of-y-buckets)
	    ;; make the bucket array, adding two additional buckets in each
	    ;; dimension for preceeding and following (out-of-bucket-range)
	    ;; instances:
	    (setf (2d-uniform-buckets.buckets storage)
		  (make-array `(,(+& 2 number-of-x-buckets)
				,(+& 2 number-of-y-buckets))
			      :initial-element nil))))))))

;;; ---------------------------------------------------------------------------

(defun print-2d-uniform-buckets-usage-message (buckets storage)
  (format *trace-output* 
	  "~&;; - ~s: Using 2D-uniform buckets "
	  (storage.dimension-names storage))
  (case buckets
    (:unbound-value
     (format *trace-output* "(unbound-value instances)~&"))
    (otherwise 
     (format *trace-output* "(~s bucket~:p)~&" buckets))))

;;; ---------------------------------------------------------------------------

(defun do-2d-uniform-buckets-add/remove-action (instance storage verbose
						dimension-values
						unbound-value-action
						bucket-action)
  (declare (type function unbound-value-action bucket-action))
  (when verbose (print-storage-usage-message storage))
  (let* ((buckets (2d-uniform-buckets.buckets storage))
	 (x-start (2d-uniform-buckets.x-start storage))
	 (x-size (2d-uniform-buckets.x-size storage))
	 (number-of-x-buckets (2d-uniform-buckets.number-of-x-buckets storage))
	 (y-start (2d-uniform-buckets.y-start storage))
	 (y-size (2d-uniform-buckets.y-size storage))
	 (number-of-y-buckets (2d-uniform-buckets.number-of-y-buckets storage))
	 (dimension-names (storage.dimension-names storage))
	 (x-dimension-name (first dimension-names))
	 (y-dimension-name (second dimension-names))
	 (x-dimension-value
	  (if dimension-values
	      (cdr (assoc x-dimension-name dimension-values :test #'eq))
	      (instance-dimension-value instance x-dimension-name)))
	 (y-dimension-value
	  (if dimension-values
	      (cdr (assoc y-dimension-name dimension-values :test #'eq))
	      (instance-dimension-value instance y-dimension-name))))
    (cond 
     ;; unbound value in either dimension:
     ((or (eq x-dimension-value unbound-value-indicator)
	  (eq x-dimension-value unbound-value-indicator))
      (when verbose
	(print-2d-uniform-buckets-usage-message 
	 ':unbound-value storage))
      (funcall unbound-value-action instance storage))
     ;; both are scalar values (special optimization for 2D points):
     ((and (numberp x-dimension-value)
	   (numberp x-dimension-value))
      (let ((x-bucket-index 
	     (bounded-uniform-bucket-index 
	      x-dimension-value x-start x-size number-of-x-buckets))
	    (y-bucket-index 
	     (bounded-uniform-bucket-index 
	      y-dimension-value y-start y-size number-of-y-buckets)))
	(when verbose
	  (print-2d-uniform-buckets-usage-message 1 storage))
	(funcall bucket-action instance buckets 
		 x-bucket-index y-bucket-index)))
     ;; at least one is an interval value:
     (t (multiple-value-bind (x-start-index x-end-index)
	    (bounded-uniform-bucket-interval-indexes
	     (if (numberp x-dimension-value)
		 x-dimension-value
		 (interval-start x-dimension-value))
	     (if (numberp x-dimension-value)
		 x-dimension-value
		 (interval-end x-dimension-value))
	     x-start x-size number-of-x-buckets)
	  (multiple-value-bind (y-start-index y-end-index)
	      (bounded-uniform-bucket-interval-indexes
	       (if (numberp y-dimension-value)
		   y-dimension-value
		   (interval-start y-dimension-value))
	       (if (numberp y-dimension-value)
		   y-dimension-value
		   (interval-end y-dimension-value))
	       y-start y-size number-of-y-buckets)
	    (when verbose
	      (print-2d-uniform-buckets-usage-message
	       (* (max& 1 (-& x-end-index x-start-index))
		  (max& 1 (-& y-end-index y-start-index)))
	       storage))
	      (let ((x-bucket-index x-start-index)
		    (y-bucket-index y-start-index))
		(until (>& x-bucket-index x-end-index)
		  (until (>& y-bucket-index y-end-index)
		    (funcall bucket-action instance buckets 
			     x-bucket-index y-bucket-index)
		    (incf& y-bucket-index))
		  (incf& x-bucket-index)
		  ;; on each x iteraction, reset y-bucket-index to 
		  ;; its start index:
		  (setq y-bucket-index y-start-index)))))))))
  
;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage ((instance standard-unit-instance)
				    (storage 2d-uniform-buckets) 
				    verbose)
  (let ((excess-count 0))
    (do-2d-uniform-buckets-add/remove-action 
	instance storage verbose nil
	;; unbound-value action:
	#'(lambda (instance storage)
	    (setf (gethash 
		   instance
		   (2d-uniform-buckets.unbound-value-instances storage))
		  instance)
	    (incf& excess-count))
      ;; bucket-action:
      #'(lambda (instance buckets x-bucket-index y-bucket-index)
	  (declare (type (simple-array t (* *)) buckets))
	  (declare (type fixnum x-bucket-index y-bucket-index))
	  (push instance (aref buckets x-bucket-index y-bucket-index))
	  (incf& excess-count)))
    ;; save the excess count:
    (incf& (storage.excess-locators storage) 
	   ;; remove the non-excess count for this instance:
	   (1-& excess-count))))
  
;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage ((instance standard-unit-instance)
					 (storage 2d-uniform-buckets) 
					 dimension-values
					 verbose)
  (let ((excess-count 0))
    (do-2d-uniform-buckets-add/remove-action 
	instance storage verbose dimension-values
	;; unbound-value action:
	#'(lambda (instance storage)
	    (remhash instance
		     (2d-uniform-buckets.unbound-value-instances storage))
	    (decf& excess-count))
	;; bucket-action:
	#'(lambda (instance buckets x-bucket-index y-bucket-index)
	    (declare (type (simple-array t (* *)) buckets))
	    (declare (type fixnum x-bucket-index y-bucket-index))
	    (setf (aref buckets x-bucket-index y-bucket-index)
		  (delq instance
			(aref buckets x-bucket-index y-bucket-index)))
	    (decf& excess-count)))
    ;; save the excess count:
    (incf& (storage.excess-locators storage) 
	   ;; add back in the non-excess count for this instance:
	   (1+& excess-count))))

;;; ---------------------------------------------------------------------------

(defun determine-2d-uniform-storage-extents (storage 
					     disjunctive-dimensional-extents)
  ;;; Returns two values:
  ;;; 1) a list of disjunctive extents in the "X" and "Y" dimensions
  ;;; 2) a boolean indicating if a full-map was requested (via 't)
  (let ((extents nil))
    (unless (eq disjunctive-dimensional-extents 't)
      (destructuring-bind (x-dimension-name y-dimension-name)
	  (storage.dimension-names storage)
	(dolist (dimensional-extents disjunctive-dimensional-extents)
	  (let* ((x-dimensional-extent 
		  (assoc x-dimension-name dimensional-extents
			 :test #'eq))
		 (y-dimensional-extent 
		  (assoc y-dimension-name dimensional-extents
			 :test #'eq))
		 (x-new-extents
		  (when x-dimensional-extent 
		    (destructuring-bind (extent-dimension-name
					 dimension-type . new-extents)
			x-dimensional-extent
		      (declare (ignore extent-dimension-name
				       dimension-type))
		      new-extents)))
		 (y-new-extents
		  (when y-dimensional-extent 
		    (destructuring-bind (extent-dimension-name
					 dimension-type . new-extents)
			y-dimensional-extent
		      (declare (ignore extent-dimension-name
				       dimension-type))
		      new-extents))))
	    ;; This isn't quite right, we need to remove the clause *AND*
	    ;; not do a full sweep if it is the only one...
	    (unless (or (equal x-new-extents '(:infeasible))
			(equal y-new-extents '(:infeasible)))
	      (push (list (or x-new-extents 
			      (list infinite-interval))
			  (or y-new-extents 
			      (list infinite-interval)))
		    extents))))))
    (or
     ;; TODO: Someday add in 2D-extent merging, for now we'll simply
     ;;       delete obvious duplicates:
     (delete-duplicates (the list extents) :test #'equal)
     (values (list (list (list infinite-interval)
			 (list infinite-interval)))
	     't))))

;;; ---------------------------------------------------------------------------

(defun do-2d-uniform-map-actions (fn storage disjunctive-dimensional-extents 
				  verbose)
  (declare (type function fn))
  (when verbose (print-storage-usage-message storage))
  (multiple-value-bind (disjunctive-storage-extents full-map-p)
      (determine-2d-uniform-storage-extents 
       storage disjunctive-dimensional-extents)
    (let* ((buckets (2d-uniform-buckets.buckets storage))
	   (number-of-x-buckets
	    (2d-uniform-buckets.number-of-x-buckets storage))
	   (x-start (2d-uniform-buckets.x-start storage))
	   (x-size (2d-uniform-buckets.x-size storage))
	   (number-of-y-buckets
	    (2d-uniform-buckets.number-of-y-buckets storage))
	   (y-start (2d-uniform-buckets.y-start storage))
	   (y-size (2d-uniform-buckets.y-size storage))
	   (bucket-count 0))
      (declare (type (simple-array t (* *)) buckets))
      ;; unbound instances req'd:
      (when full-map-p
	(when verbose
	  (print-2d-uniform-buckets-usage-message ':unbound-value storage))
	(incf& bucket-count)
	(maphash fn (2d-uniform-buckets.unbound-value-instances storage)))
      (dolist (storage-extents disjunctive-storage-extents)
	(dolist (x-region (first storage-extents))
	  (dolist (y-region (second storage-extents))
	    (multiple-value-bind (x-start-index x-end-index)
		(bounded-uniform-bucket-interval-indexes
		 (first x-region)
		 (second x-region)
		 x-start x-size number-of-x-buckets)
	      (multiple-value-bind (y-start-index y-end-index)
		  (bounded-uniform-bucket-interval-indexes
		   (first y-region)
		   (second y-region)
		   y-start y-size number-of-y-buckets)
		(when verbose
		  (print-2d-uniform-buckets-usage-message
		   (* (max& 1 (-& x-end-index x-start-index))
		      (max& 1 (-& y-end-index y-start-index)))
		   storage))
		(let ((x-bucket-index x-start-index)
		      (y-bucket-index y-start-index))
		  (declare (type fixnum x-bucket-index y-bucket-index))
		  (until (>& x-bucket-index x-end-index)
		    (until (>& y-bucket-index y-end-index)
		      (incf& bucket-count)
		      (let ((instances 
			     (aref buckets x-bucket-index y-bucket-index)))
			(dolist (instance instances)
			  (funcall fn instance instance)))
		      (incf& y-bucket-index))
		    (incf& x-bucket-index)
		    ;; on each x iteraction, reset y-bucket-index to 
		    ;; its start index:
		    (setq y-bucket-index y-start-index))))))))
      ;; record the bucket count:
      (let ((find-stats *find-stats*))
	(when find-stats 
	  (incf (find-stats.bucket-count find-stats) bucket-count))))))
      
;;; ---------------------------------------------------------------------------

(defmethod map-marked-instances-on-storage (fn (storage 2d-uniform-buckets)
					    disjunctive-dimensional-extents 
					    mark-index verbose)
  (do-2d-uniform-map-actions 
      #'(lambda (key instance)
	  (declare (ignore key))
	  (let ((marks (standard-unit-instance.%%marks%% instance)))
	    (when (flag-set-p marks mark-index)
	      (funcall (the function fn) instance marks))))
    storage disjunctive-dimensional-extents verbose))

;;; ---------------------------------------------------------------------------

(defmethod map-all-instances-on-storage (fn (storage 2d-uniform-buckets)
					 disjunctive-dimensional-extents 
					 verbose)
  (do-2d-uniform-map-actions 
      #'(lambda (key instance)
	  (declare (ignore key))
	  (funcall (the function fn) instance))
    storage disjunctive-dimensional-extents verbose))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

