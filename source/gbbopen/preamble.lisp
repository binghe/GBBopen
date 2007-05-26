;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/preamble.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri May 25 04:56:55 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                          GBBopen Preamble
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-17-02 File Created.  (Corkill)
;;;  09-26-04 Renamed instance-hash-table-test to more descriptive
;;;           instance-name-comparison-test.  (Corkill)
;;;  11-05-05 Added *coerce-interval-rationals-to-floats*.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :portable-threads))

;; Avoid conflicts with :ccl exports (the :ccl package contains CLOS, is
;; nicknamed :clos by GBBopen, and later used by :gbbopen-user):
#+(or digitool-mcl openmcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(ccl:false ccl:true)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*coerce-interval-rationals-to-floats* ; not documented
	    *warn-about-unusual-requests*
    	    draw-instance-on-bb-widget  ; used for bb-widget drawing
	    gbbopen-graphics-started-p
	    gbbopen-implementation-version
	    instance-name-comparison-test)))

;;; ---------------------------------------------------------------------------

(defun gbbopen-implementation-version ()
  "0.9.5")

;;; Added to *features* in epilogue.lisp:
(defparameter *gbbopen-version-keyword* :gbbopen-0.9.5)

;;; ---------------------------------------------------------------------------
;;;  Control warnings when:
;;;   1. an instance is added to a space instance when it is already there
;;;   2. an instance is removed from a space instance when it is not there
;;;   3. an instance is added to a space instance that shares no dimensions 
;;;      with it

(defvar *warn-about-unusual-requests* 't)

;;; ---------------------------------------------------------------------------
;;;  Control automatic coercion of non-integer rationals (that are created
;;;  while manipulating intervals) to floats:

(defvar *coerce-interval-rationals-to-floats* 't)

;;; ---------------------------------------------------------------------------

;;; Although a bit heavy-weight, we use one master lock to synchronize
;;; all unit-instance & space-instance activities:

(defvar *master-instance-lock*
    (make-process-lock :name "Master instance lock"))

;;; ---------------------------------------------------------------------------

(defun print-gbbopen-herald ()
  (format t "~%;;; ~72,,,'-<-~>
;;;  GBBopen ~a~@
;;;
;;;  GBBopen is open-source (free) software
;;;  (see ~a)
;;;
;;; ~72,,,'-<-~>~2%"
          (gbbopen-implementation-version)
          (let ((this-file *load-truename*))
            (namestring
             (make-pathname
              :directory (butlast (pathname-directory this-file) 2)
              :name "LICENSE"
              :type nil
              :defaults this-file)))))
  
(eval-when (:load-toplevel)
  (print-gbbopen-herald))

;;; ---------------------------------------------------------------------------

(defun internal-error (datum &rest args)
  (declare (dynamic-extent args))
  (apply #'error (concatenate 'string "Internal GBBopen Error => " datum)
	 args))

;;; ===========================================================================
;;;  GBBopen Core Generic Functions

(defgeneric addto-evfn-using-class (fn event-class plus-subevents 
                                    unit-class/instance plus-subclasses
                                    slot-names paths permanent priority printing 
                                    evfn-blk-fn evfn-blk-fn-args))
(defgeneric add-instance-to-space-instance (instance space-instance))
(defgeneric add-instance-to-storage (instance storage verbose))
(defgeneric class-instances-count (unit-class-name-or-class))
(defgeneric class-instances-summary (unit-class-name-or-class))
(defgeneric delete-space-instance (space-instance))
(defgeneric describe-instance (instance))
(defgeneric describe-space-instance (space-instance))
(defgeneric describe-unit-class (unit-class-name-or-class))
(defgeneric describe-unit-slot (class slot))
(defgeneric ds-evfn-using-class (fn event-class &rest args))
(defgeneric do-evfns (evfn-blk event-class args))
(defgeneric draw-instance-on-bb-widget (instance bb-widget &optional filter))
(defgeneric event-printer (class stream &rest args))
(defgeneric evfn-describer (evfn-blk fn))
(defgeneric hidden-nonlink-slot-names (instance))
(defgeneric instance-name-comparison-test (class))
(defgeneric link-slot-p (slot))
(defgeneric map-all-instances-on-storage (fn storage 
					  disjunctive-dimensional-extents  
					  verbose))
(defgeneric map-marked-instances-on-storage (fn storage 
					     disjunctive-dimensional-extents 
					     mark-index verbose))
(defgeneric propagate-event-evfns/printing (event-class
                                            super-event-classes))
(defgeneric remove-instance-from-space-instance (instance space-instance))
(defgeneric remove-instance-from-storage (instance storage dimension-values 
                                          verbose))
(defgeneric reset-unit-class (unit-class-name-or-class))
(defgeneric rmfrom-evfn-using-class (fn event-class plus-subevents 
                                     unit-class/instance plus-subclasses
                                     slot-names paths permanent printing 
                                     evfn-blk-fn evfn-blk-fn-args))
(defgeneric setup-instance-storage (space-instance instance-mapping))
(defgeneric signal-event-using-class (class &rest args))
(defgeneric validate-event-metaclass (event-metaclass-name
                                      super-event-metaclasses))
(defgeneric validate-class-option (metaclass option))

;;; ---------------------------------------------------------------------------
;;;   Multinode Interface Generic Functions (predefined here)

(defgeneric save-gbbopen-node-state (node-state))
(defgeneric restore-gbbopen-node-state (node-state))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


