;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/2d-uniform-storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 11 12:49:17 2007 *-*
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
;;; Copyright (C) 2003-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-23-06 Split out from storage.lisp.  (Corkill)
;;;  06-11-07 Change "x" and "y" names to "1st-d" and "2nd-d" to avoid 
;;;           confusion with application dimension names.  (Corkill)
;;;  06-11-07 Converted 2d-uniform-buckets accessors from :prefix to modern
;;;           "-of" format.  (Corkill)
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
  ((1st-d-start)
   (1st-d-size)
   (number-of-1st-d-buckets)
   (2nd-d-start)
   (2nd-d-size)
   (number-of-2nd-d-buckets)
   (buckets :type (simple-array t (* *)))
   (unbound-value-instances :initform (make-hash-table :test 'eq)))
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
    ;; 1st dimension:
    (destructuring-bind (start end size)
	(first layout)
      (let ((number-of-1st-d-buckets (ceiling (- end start) size)))
	(setf (1st-d-start-of storage) start)
	(setf (1st-d-size-of storage) size)
	(setf (number-of-1st-d-buckets-of storage) number-of-1st-d-buckets)
	;; 2nd dimension:
	(destructuring-bind (start end size)
	    (second layout)
	  (let ((number-of-2nd-d-buckets (ceiling (- end start) size)))
	    (setf (2nd-d-start-of storage) start)
	    (setf (2nd-d-size-of storage) size)
	    (setf (number-of-2nd-d-buckets-of storage) number-of-2nd-d-buckets)
	    ;; make the bucket array, adding two additional buckets in each
	    ;; dimension for preceeding and following (out-of-bucket-range)
	    ;; instances:
	    (setf (buckets-of storage)
		  (make-array `(,(+& 2 number-of-1st-d-buckets)
				,(+& 2 number-of-2nd-d-buckets))
			      :initial-element nil))))))))

;;; ---------------------------------------------------------------------------

(defun print-2d-uniform-buckets-usage-message (buckets storage)
  (format *trace-output* 
	  "~&;; - ~s: Using 2D-uniform buckets "
	  (dimension-names-of storage))
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
  (let* ((buckets (buckets-of storage))
	 (1st-d-start (1st-d-start-of storage))
	 (1st-d-size (1st-d-size-of storage))
	 (number-of-1st-d-buckets (number-of-1st-d-buckets-of storage))
	 (2nd-d-start (2nd-d-start-of storage))
	 (2nd-d-size (2nd-d-size-of storage))
	 (number-of-2nd-d-buckets (number-of-2nd-d-buckets-of storage))
	 (dimension-names (dimension-names-of storage))
	 (1st-d-dimension-name (first dimension-names))
	 (2nd-d-dimension-name (second dimension-names))
	 (1st-d-dimension-value
	  (if dimension-values
	      (cdr (assoc 1st-d-dimension-name dimension-values :test #'eq))
	      (instance-dimension-value instance 1st-d-dimension-name)))
	 (2nd-d-dimension-value
	  (if dimension-values
	      (cdr (assoc 2nd-d-dimension-name dimension-values :test #'eq))
	      (instance-dimension-value instance 2nd-d-dimension-name))))
    (cond 
     ;; unbound value in either dimension:
     ((or (eq 1st-d-dimension-value unbound-value-indicator)
	  (eq 1st-d-dimension-value unbound-value-indicator))
      (when verbose
	(print-2d-uniform-buckets-usage-message 
	 ':unbound-value storage))
      (funcall unbound-value-action instance storage))
     ;; both are scalar values (special optimization for 2D points):
     ((and (numberp 1st-d-dimension-value)
	   (numberp 2nd-d-dimension-value))
      (let ((1st-d-bucket-index 
	     (bounded-uniform-bucket-index 
	      1st-d-dimension-value 1st-d-start 1st-d-size
              number-of-1st-d-buckets))
	    (2nd-d-bucket-index 
	     (bounded-uniform-bucket-index 
	      2nd-d-dimension-value 2nd-d-start 2nd-d-size
              number-of-2nd-d-buckets)))
	(when verbose
	  (print-2d-uniform-buckets-usage-message 1 storage))
	(funcall bucket-action instance buckets 
		 1st-d-bucket-index 2nd-d-bucket-index)))
     ;; at least one is an interval value:
     (t (multiple-value-bind (1st-d-start-index 1st-d-end-index)
	    (bounded-uniform-bucket-interval-indexes
	     (if (numberp 1st-d-dimension-value)
		 1st-d-dimension-value
		 (interval-start 1st-d-dimension-value))
	     (if (numberp 1st-d-dimension-value)
		 1st-d-dimension-value
		 (interval-end 1st-d-dimension-value))
	     1st-d-start 1st-d-size number-of-1st-d-buckets)
	  (multiple-value-bind (2nd-d-start-index 2nd-d-end-index)
	      (bounded-uniform-bucket-interval-indexes
	       (if (numberp 2nd-d-dimension-value)
		   2nd-d-dimension-value
		   (interval-start 2nd-d-dimension-value))
	       (if (numberp 2nd-d-dimension-value)
		   2nd-d-dimension-value
		   (interval-end 2nd-d-dimension-value))
	       2nd-d-start 2nd-d-size number-of-2nd-d-buckets)
	    (when verbose
	      (print-2d-uniform-buckets-usage-message
	       (* (max& 1 (-& 1st-d-end-index 1st-d-start-index))
		  (max& 1 (-& 2nd-d-end-index 2nd-d-start-index)))
	       storage))
	      (let ((1st-d-bucket-index 1st-d-start-index)
		    (2nd-d-bucket-index 2nd-d-start-index))
		(until (>& 1st-d-bucket-index 1st-d-end-index)
		  (until (>& 2nd-d-bucket-index 2nd-d-end-index)
		    (funcall bucket-action instance buckets 
			     1st-d-bucket-index 2nd-d-bucket-index)
		    (incf& 2nd-d-bucket-index))
		  (incf& 1st-d-bucket-index)
		  ;; on each x iteraction, reset 2nd-d-bucket-index to 
		  ;; its start index:
		  (setq 2nd-d-bucket-index 2nd-d-start-index)))))))))
  
;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage ((instance standard-unit-instance)
				    (storage 2d-uniform-buckets) 
				    verbose)
  (let ((excess-count 0))
    (do-2d-uniform-buckets-add/remove-action 
	instance storage verbose nil
	;; unbound-value action:
	#'(lambda (instance storage)
	    (setf (gethash instance (unbound-value-instances-of storage))
		  instance)
	    (incf& excess-count))
      ;; bucket-action:
      #'(lambda (instance buckets 1st-d-bucket-index 2nd-d-bucket-index)
	  (declare (type (simple-array t (* *)) buckets))
	  (declare (type fixnum 1st-d-bucket-index 2nd-d-bucket-index))
	  (push instance (aref buckets 1st-d-bucket-index 2nd-d-bucket-index))
	  (incf& excess-count)))
    ;; save the excess count:
    (incf& (excess-locators-of storage) 
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
	    (remhash instance (unbound-value-instances-of storage))
	    (decf& excess-count))
	;; bucket-action:
	#'(lambda (instance buckets 1st-d-bucket-index 2nd-d-bucket-index)
	    (declare (type (simple-array t (* *)) buckets))
	    (declare (type fixnum 1st-d-bucket-index 2nd-d-bucket-index))
	    (setf (aref buckets 1st-d-bucket-index 2nd-d-bucket-index)
		  (delq instance
			(aref buckets 1st-d-bucket-index 2nd-d-bucket-index)))
	    (decf& excess-count)))
    ;; save the excess count:
    (incf& (excess-locators-of storage) 
	   ;; add back in the non-excess count for this instance:
	   (1+& excess-count))))

;;; ---------------------------------------------------------------------------

(defun determine-2d-uniform-storage-extents (storage 
					     disjunctive-dimensional-extents)
  ;;; Returns two values:
  ;;; 1) a list of disjunctive extents in the 1st-d and 2nd-d dimensions
  ;;; 2) a boolean indicating if a full-map was requested (via 't)
  (let ((extents nil))
    (unless (eq disjunctive-dimensional-extents 't)
      (destructuring-bind (1st-d-dimension-name 2nd-d-dimension-name)
	  (dimension-names-of storage)
	(dolist (dimensional-extents disjunctive-dimensional-extents)
	  (let* ((1st-d-dimensional-extent 
		  (assoc 1st-d-dimension-name dimensional-extents
			 :test #'eq))
		 (2nd-d-dimensional-extent 
		  (assoc 2nd-d-dimension-name dimensional-extents
			 :test #'eq))
		 (1st-d-new-extents
		  (when 1st-d-dimensional-extent 
		    (destructuring-bind (extent-dimension-name
					 dimension-type . new-extents)
			1st-d-dimensional-extent
		      (declare (ignore extent-dimension-name
				       dimension-type))
		      new-extents)))
		 (2nd-d-new-extents
		  (when 2nd-d-dimensional-extent 
		    (destructuring-bind (extent-dimension-name
					 dimension-type . new-extents)
			2nd-d-dimensional-extent
		      (declare (ignore extent-dimension-name
				       dimension-type))
		      new-extents))))
	    ;; This isn't quite right, we need to remove the clause *AND*
	    ;; not do a full sweep if it is the only one...
	    (unless (or (equal 1st-d-new-extents '(:infeasible))
			(equal 2nd-d-new-extents '(:infeasible)))
	      (push (list (or 1st-d-new-extents 
			      (list infinite-interval))
			  (or 2nd-d-new-extents 
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
    (let* ((buckets (buckets-of storage))
	   (number-of-1st-d-buckets (number-of-1st-d-buckets-of storage))
	   (1st-d-start (1st-d-start-of storage))
	   (1st-d-size (1st-d-size-of storage))
	   (number-of-2nd-d-buckets (number-of-2nd-d-buckets-of storage))
	   (2nd-d-start (2nd-d-start-of storage))
	   (2nd-d-size (2nd-d-size-of storage))
	   (bucket-count 0))
      (declare (type (simple-array t (* *)) buckets))
      ;; unbound instances req'd:
      (when full-map-p
	(when verbose
	  (print-2d-uniform-buckets-usage-message ':unbound-value storage))
	(incf& bucket-count)
	(maphash fn (unbound-value-instances-of storage)))
      (dolist (storage-extents disjunctive-storage-extents)
	(dolist (1st-d-region (first storage-extents))
	  (dolist (2nd-d-region (second storage-extents))
	    (multiple-value-bind (1st-d-start-index 1st-d-end-index)
		(bounded-uniform-bucket-interval-indexes
		 (first 1st-d-region)
		 (second 1st-d-region)
		 1st-d-start 1st-d-size number-of-1st-d-buckets)
	      (multiple-value-bind (2nd-d-start-index 2nd-d-end-index)
		  (bounded-uniform-bucket-interval-indexes
		   (first 2nd-d-region)
		   (second 2nd-d-region)
		   2nd-d-start 2nd-d-size number-of-2nd-d-buckets)
		(when verbose
		  (print-2d-uniform-buckets-usage-message
		   (* (max& 1 (-& 1st-d-end-index 1st-d-start-index))
		      (max& 1 (-& 2nd-d-end-index 2nd-d-start-index)))
		   storage))
		(let ((1st-d-bucket-index 1st-d-start-index)
		      (2nd-d-bucket-index 2nd-d-start-index))
		  (declare (type fixnum 1st-d-bucket-index 2nd-d-bucket-index))
		  (until (>& 1st-d-bucket-index 1st-d-end-index)
		    (until (>& 2nd-d-bucket-index 2nd-d-end-index)
		      (incf& bucket-count)
		      (let ((instances 
			     (aref buckets 
                                   1st-d-bucket-index 
                                   2nd-d-bucket-index)))
			(dolist (instance instances)
			  (funcall fn instance instance)))
		      (incf& 2nd-d-bucket-index))
		    (incf& 1st-d-bucket-index)
		    ;; on each 1st-d iteraction, reset 2nd-d-bucket-index to
		    ;; its start index:
		    (setq 2nd-d-bucket-index 2nd-d-start-index))))))))
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

