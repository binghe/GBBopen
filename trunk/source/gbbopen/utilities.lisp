;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/utilities.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Sep 29 05:45:22 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       GBBopen Utility Functions
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
;;;  09-18-03 File Created.  (Corkill)
;;;  05-21-04 Export interval-start, -end, and -values.  (Corkill)
;;;  06-07-04 Added bounded-uniform-bucket-index and
;;;           bounded-uniform-bucket-interval-indexes.  (Corkill)
;;;  11-05-05 Added expand-interval and shift-interval.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(expand-interval
	    interval-start
	    interval-end
	    interval-values		; not yet documented
	    shift-interval)))

;;; ---------------------------------------------------------------------------
;;;  Interval Functions:
;;;
;;;  Intervals are one of: (start end), (start . end), or #(start end).
;;;  We don't worry about length or (<= start end) checking here.

(defun invalid-interval-value (interval)
  (error "Invalid interval ~s" interval))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun interval-start (interval)
    ;;; Returns the start value of `interval'
    (if (consp interval)
	(car (the cons interval)) 
	(without-cmu/sbcl-optimization-warnings
	  (elt (the (simple-array * (2)) interval) 0)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun (setf interval-start) (nv interval)
    ;;; Sets the start value of `interval'
    (if (consp interval)
	(setf (car (the cons interval)) nv)
	(without-cmu/sbcl-optimization-warnings
 	  (setf (elt (the (simple-array * (2)) interval) 0) nv)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun interval-end (interval)
    ;;; Returns the end value of `interval'
    (if (consp interval) 
	(let ((maybe-end (cdr (the cons interval))))
	  (if (consp maybe-end)
	      (sole-element maybe-end)
	      maybe-end))
	(without-cmu/sbcl-optimization-warnings
	    (elt (the (simple-array * (2)) interval) 1)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun (setf interval-end) (nv interval)
    ;;; Sets the end value of `interval'
    (if (consp interval) 
	(let ((maybe-end (cdr (the cons interval))))
	  (if (consp maybe-end)
	      (setf (car maybe-end) nv)
	      (setf (cdr interval) nv)))
	(without-cmu/sbcl-optimization-warnings
	   (setf (elt (the (simple-array * (2)) interval) 1) nv)))))

;;; ---------------------------------------------------------------------------

(defun expand-interval (interval amount)
  (flet ((expand (start end amount)
	   (let ((new-start (- start amount))
		 (new-end (+ end amount)))
	     (if (> new-start new-end)
		 (let ((point-value (/ (+ new-start new-end) 2)))
		   ;; Coerce non-integral rationals, if requested:
		   (when (and (not (integerp point-value))
			      (rationalp point-value)
			      (integerp new-start)
			      (integerp new-end)
			      *coerce-interval-rationals-to-floats*)
		     (setq point-value (coerce point-value 'float)))
		   (values point-value point-value))
		 (values new-start new-end)))))
    (if (consp interval)
	(let ((maybe-end (cdr (the cons interval))))
	  (if (consp maybe-end)
	      (multiple-value-call 
		  #'list 
		(expand (car interval) (sole-element maybe-end) amount))
	      (multiple-value-call
	       #'cons (expand (car interval) maybe-end amount))))
	(make-array 
	 '(2) 
	 :element-type (array-element-type interval)
	 :initial-contents (multiple-value-call
			       #'list
			     (expand (elt (the (simple-array * (2)) interval) 0)
				     (elt (the (simple-array * (2)) interval) 1)
				     amount))))))
      
;;; ---------------------------------------------------------------------------

(defun shift-interval (interval amount)
  (if (consp interval)
      (let ((maybe-end (cdr (the cons interval))))
	(if (consp maybe-end)
	    (list (+ (car interval) amount)
		  (+ (sole-element maybe-end) amount))
	    (cons (+ (car interval) amount)
		  (+ maybe-end amount))))
      (make-array 
       '(2) 
       :element-type (array-element-type interval)
       :initial-contents (list (+ (elt (the (simple-array * (2)) interval) 0)
				  amount)
			       (+ (elt (the (simple-array * (2)) interval) 1)
				  amount)))))

;;; ---------------------------------------------------------------------------

(defun interval-values (interval)
  ;;; Returns the start and end values of `interval' as multiple value:
  (cond ((consp interval)
	 (values (car (the cons interval))
		 (let ((maybe-end (cdr (the cons interval))))
		   (if (consp maybe-end)
		       (sole-element maybe-end)
		       maybe-end))))
	(t (without-cmu/sbcl-optimization-warnings
	       (values 
		(elt (the (simple-array * (2)) interval) 0) 
		(elt (the (simple-array * (2)) interval) 1))))))
  
;;; ---------------------------------------------------------------------------
;;;   Uniform-bucket index computation:

(defun uniform-bucket-index (value start size)
  ;;; Compute the bucket index for `value' for uniform buckets of `size':
  ;;;  - a negative index preceed the buckets
  ;;;
  ;;;  Although the result is a fixnum, the internal calculations are general
  ;;;  numeric with infinity values.
  (case value
    (#.-infinity most-negative-fixnum)
    (#.infinity most-positive-fixnum)
    (otherwise (floor (- value start) size))))

;;; ---------------------------------------------------------------------------

(defun bounded-bucket-index (value start size number-of-buckets)
  ;;; Compute the bounded uniform bucket index for `value' for
  ;;; `number-of-buckets' uniform buckets of `size':
  ;;;  - a value that preceeds the buckets returns an index of 0
  ;;;  - a value that follows the buckets returns an index of
  ;;;    (+ 1 number-of-buckets)
  ;;;  - other values return the "natural" index incremented by 1
  ;;;
  ;;;  Although the result is a fixnum, the internal calculations are general
  ;;;  numeric with infinity values.
  (let ((bucket-index (floor (- value start) size)))
    (declare (type fixnum bucket-index))
    (cond ((minusp& bucket-index) 0)
          ((>& bucket-index number-of-buckets)
           (1+& number-of-buckets))
          (t (1+& bucket-index)))))

;;; ---------------------------------------------------------------------------

(defun bounded-uniform-bucket-index (value start size number-of-buckets)
  ;;; Compute the bounded uniform bucket index for `value' for
  ;;; `number-of-buckets' uniform buckets of `size':
  ;;;  - a value that preceeds the buckets returns an index of 0
  ;;;  - a value that follows the buckets returns an index of
  ;;;    (+ 1 number-of-buckets)
  ;;;  - other values return the "natural" index incremented by 1
  (case value
    (#.-infinity 0)
    (#.infinity (1+& number-of-buckets))
    (otherwise (bounded-bucket-index value start size number-of-buckets))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun bounded-uniform-bucket-interval-indexes (start-value end-value
                                                  start size number-of-buckets)
    ;;; Compute the bounded uniform bucket interval indexes for
    ;;; `start-value' through `end-value' for `number-of-buckets'
    ;;; uniform buckets of `size':
    ;;;  - a value that preceeds the buckets includes an index of 0
    ;;;  - a value that follows the buckets includes an index of
    ;;;    (+ 1 number-of-buckets)
    ;;;  - other values return the "natural" indexes are incremented by 1
    (values 
     (bounded-uniform-bucket-index start-value start size number-of-buckets)
     (bounded-uniform-bucket-index end-value start size number-of-buckets))))
  
;;; ---------------------------------------------------------------------------
;;;  Formatted output helpers:

(defun format-column (tab-column values &optional (ctrl-string "~s")
                                                  (key #'identity))
  (if values
      (let ((key (coerce key 'function)))
	(dolist (value values)
	  (multiple-value-call
	      #'format t "~&~vt~@?" tab-column ctrl-string 
	      (funcall key value))))
      (format t "~vtNone" tab-column)))
  
;;; ---------------------------------------------------------------------------

(defun maybe-format-labeled-entry (tab-column test label 
                                   &optional (value test))
  (when test
    (format t "~&~vt~s ~w" tab-column label value)))
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

