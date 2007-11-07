;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/epilogue.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Nov  7 11:03:10 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                GBBopen Miscellaneous Entities & Epilogue
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-16-04 File Created.  (Corkill)
;;;  05-03-04 Added reset-gbbopen.  (Corkill)
;;;  11-07-07 Retain the root-space-instance when resetting GBBopen.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(reset-gbbopen)))

;;; ===========================================================================
;;;  Miscellaneous Entities

(defun reset-gbbopen (&key (disable-events t)
			   (retain-classes nil)
			   (retain-event-printing nil)
			   (retain-event-functions nil)
			   ;; Not documented:
			   (retain-space-instance-event-functions nil))
  ;;; Deletes all unit and space instances; resets instance counters to 1.
  (let ((*%%events-enabled%%* (not disable-events)))
    (map-extended-unit-classes 
     #'(lambda (unit-class plus-subclasses)
	 (declare (ignore plus-subclasses))
	 (unless (or 
                  ;; Retain the root-space-instance
                  (eq (class-name unit-class) 'root-space-instance)
                  (and retain-classes
                       (unit-class-in-specifier-p unit-class retain-classes)))
	   ;; We must practice safe delete-instance:
	   (let ((instances nil))
	     (map-instances-given-class 
	      #'(lambda (instance) (push instance instances)) unit-class)
	     (mapc #'delete-instance instances)
	     (reset-unit-class unit-class))))
     't)
    (unless retain-event-printing (disable-event-printing))
    ;; Keep around the path-event-functions specifications for new
    ;; space-instances if either :retain-event-functions (or the more
    ;; specific :retain-space-instance-event-functions) is specified:
    (unless (or retain-event-functions
		retain-space-instance-event-functions)
      (map-event-classes 
       #'(lambda (event-class plus-subclasses)
	   (declare (ignore plus-subclasses))
	   (setf (space-instance-event-class.path-event-functions event-class)
		 nil))
       (find-class 'space-instance-event)))
    ;; Remove all event functions (path-event-functions specifications
    ;; for new space instances are removed above, if appropriate):
    (unless retain-event-functions
      (remove-all-event-functions))))

;;; ===========================================================================
;;;  GBBopen is fully loaded

(pushnew :gbbopen *features*)
(pushnew *gbbopen-version-keyword* *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


