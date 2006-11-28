;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/test/timing-tests.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Jul 30 10:17:08 2005 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *           Some Simple Larger-Scale Timing Tests for GBBopen
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2005, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-03-04 File Created.  (Corkill)
;;;  06-09-04 Reworked for different storage-layout testing.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

(defun position.x (position) (svref position 0))
(defun position.y (position) (svref position 1))
(defun position.z (position) (svref position 2))

(define-unit-class tuc-1 ()
  ((position :initform #(0 0 0))
   (classification :initform :unknown)
   (rlink :link :reflexive :singular t)
   (mlink :link :reflexive ))
  (:dimensional-values
   (x :point #'position.x position)
   (y :point #'position.y position)
   (z :point #'position.z position)
   (classification :element classification))
  (:initial-space-instances (space-1)))

;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((obj tuc-1) stream)
  (call-next-method)
  (when (slot-boundp obj 'position)
    (format stream " ~s"
            (slot-value obj 'position))))

;;; ---------------------------------------------------------------------------

(defun do-the-time-tests (n number-of-finds storage)
  (reset-gbbopen)
  (let* ((cube-root (ceiling (expt n 1/3)))
	 (n (expt cube-root 3)))
    (declare (type fixnum cube-root n))
    (format t "~&;; *** Timing tests with ~s instances (~s^3) and ~
               ~s finds ***~%"
	    n cube-root number-of-finds)
    (format t "~&;; Storage: ~s~%" storage)
    (time
     (let ((space-1 (make-space-instance 
		     '(space-1)
		     :dimensions (unit-class-dimensions 'tuc-1)
		     :storage storage
		     :make-parents t)))
       
       (format t "~&;; Running creation test (~s instances)...~%" n)
       (time (dotimes (i cube-root) 
	       (declare (type fixnum i))
	       (dotimes (j cube-root) 
		 (declare (type fixnum j))
		 (dotimes (k cube-root) 
		   (declare (type fixnum i))
		   (make-instance 'tuc-1 :position (vector i j k))))))
       
       (describe-space-instance-storage space-1)
       
       (format t "~&;; Running retrieval test (w/ hashing) ~
                 (~s finds on ~s instances)...~%" 
	       number-of-finds
	       n)
       (time (dotimes (i number-of-finds) 
	       (declare (type fixnum i))
	       (let ((i (mod& i cube-root)))
		 (declare (type fixnum i))
		 (let ((interval `(,(-& i 1) ,(+& i 1))))
		   (declare (dynamic-extent interval))
		   (find-instances 'tuc-1 space-1 
				   `(within& (x y z) 
					     (,interval
					      ,interval
					      ,interval))
				   :use-marking nil)))))

       (format t "~&;; Running retrieval test (w/ marking) ~
                  (~s finds on ~s instances)...~%" 
	       number-of-finds
	       n)
       (time (dotimes (i number-of-finds) 
	       (declare (type fixnum i))
	       (let ((l (mod& i cube-root)))
		 (declare (type fixnum l))
		 (let ((interval `(,(-& l 1) ,(+& l 1))))
		   (declare (dynamic-extent interval))
		   (find-instances 'tuc-1 space-1 
				   `(within& (x y z) 
					     (,interval
					      ,interval
					      ,interval))
				   :use-marking 't)))))
       
       (format t "~&;; Running mapping-instances-of-class test ~
                (~s instances)...~%" n)
       ;; Avoid smart optimizations (such as with #'identity):
       (time (map-instances-of-class #'tuc-1.classification 'tuc-1))
       
       (let ((a-unit-instance (find-instance-by-name 1 'tuc-1)))
	 (format t "~&;; Running link-setf test (~s instances)...~%" n)
	 (time (map-instances-of-class 
		#'(lambda (instance)
		    (link-setf (tuc-1.rlink instance) a-unit-instance))
		'tuc-1))
	 
	 (format t "~&;; Running linkf test (1 instance to ~s instances)...~%" n)
	 (time (map-instances-of-class 
		#'(lambda (instance)
		    (linkf (tuc-1.mlink instance) a-unit-instance))
		'tuc-1)))
       
       (format t "~&;; Running deletion test (~s instances)...~%" n)
       (time (map-instances-of-class #'delete-instance 'tuc-1))
       
       (format t "~&;; *** Timing test totals ***~%")))))

(progn
  (do-the-time-tests 25000 10 '(tuc-1 t unstructured))
  (do-the-time-tests 25000 100 '(tuc-1 x uniform-buckets 
				 :layout (0 30 1)))
  (do-the-time-tests 25000 500 '(tuc-1 (x y) uniform-buckets 
				 :layout ((0 30 2)
					  (0 30 2)))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


