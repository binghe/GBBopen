;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/control-shells/test/agenda-shell-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Jul 14 13:39:57 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Simple Agenda-Shell Trip Test
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
;;;  04-21-04 File Created.  (Corkill)
;;;  10-05-06 Added define-ks-class and define-ksa-class tests.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '(:agenda-shell)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*autorun-modules*
            agenda-shell::*cs*)))

;;; ===========================================================================
;;;   Unit definitions

(define-unit-class uc-one ()
  ((value :initform 0)
   (uc-two :link (uc-two uc-one :singular t)
	 :singular t))
  (:dimensional-values
   (value :point value))
  (:initial-space-instances (space-1)))

;;; ---------------------------------------------------------------------------

(define-unit-class uc-two ()
  ((value)
   (uc-one :link (uc-one uc-two :singular t)
	 :singular t))
  (:dimensional-values
   (value :point value))
  (:initial-space-instances (space-2)))

;;; ---------------------------------------------------------------------------
;;;  A user-defined KS and KSA class for testing purposes:

(define-unit-class do-nothing-mixin ()
  ((do-nothing-slot :initform nil)))

(define-ks-class triggered-ks (ks do-nothing-mixin)
  ((an-unused-slot :initform nil)))

(define-ks-class obviated-ks (ks do-nothing-mixin)
  ((an-unused-slot :initform nil)))

(define-ksa-class triggered-ksa (ksa do-nothing-mixin)
  ((rating :documentation "Simply shadowing this slot")))

(define-ksa-class retriggered-ksa (ksa do-nothing-mixin)
  ((rating :documentation "Simply shadowing this slot")))

;;; ===========================================================================
;;;   KS definitions

(define-ks start-control-shell-ks
    :trigger-events ((start-control-shell-event))
    :activation-predicate 
    #'(lambda (ks event)
        (declare (ignore ks event))
        (format t "~&;; Start-control-shell-ks ~
                        :activation-predicate executed~%")
        t)
    :revalidation-predicate 
    #'(lambda (ksa)
        (declare (ignore ksa))
        (format t "~&;; Start-control-shell-ks ~
                        :revalidation-predicate executed~%")
        t)
    :precondition-function 
    #'(lambda (ks events)
        (declare (ignore ks events))
        (format t "~&;; Start-control-shell-ks ~
                        :precondition-function executed~%")
        most-positive-rating)
    :execution-function 
    #'(lambda (ksa)
	(let ((trigger-events (trigger-events-of ksa)))
	  (unless (and (consp trigger-events)
		       (= (length trigger-events) 1)
		       (typep (first trigger-events)
			      'start-control-shell-event))
	    (error "Start-control-shell: Wrong events ~s" 
		   trigger-events)))
        (format t "~&;; Start-control-shell-ks ~
                        :execution-function executed~%")
        (values)))

;;; ---------------------------------------------------------------------------

(define-ks quiescence-event-ks
  :trigger-events ((quiescence-event))
  :execution-function 
  #'(lambda (ksa)
      (format t "~&;; Quiescense ks executed~%")
      ;; disable this KS after 1 use!
      (setf (ks-enabled-p (ks-of ksa)) nil)
      (make-instance 'uc-one)
      (values)))
		   
;;; ---------------------------------------------------------------------------

(define-ks retriggered-event-ks
  :trigger-events ((create-instance-event uc-one))
  :retrigger-events ((create-instance-event uc-two))
  :rating -1
  :retrigger-function
  #'(lambda (ksa event)
      (declare (ignore event))
      (format t "~&;; Retriggered KS retriggered~%")
      (setf (rating-of ksa) 100)
      ;; Check that this ksa is now first on the pending-ksas queue:
      (unless (eq ksa (select-ksa-to-execute *cs*))
        (error "KSA rating did not effect the pending-ksas queue")))
  :execution-function 
  #'(lambda (ksa)
      (declare (ignore ksa))
      (format t "~&;; Retriggered KS executed~%"))
  :ksa-class retriggered-ksa)
		   
;;; ---------------------------------------------------------------------------

(define-ks obviation-event-ks
  :trigger-events ((create-instance-event uc-one))
  :obviation-events ((quiescence-event))
  :rating -1
  :obviation-predicate 
  #'(lambda (ksa event)
      (declare (ignore ksa event))
      (format t "~&;; Obviation KS obviated~%")      
      't)
  :execution-function 
  #'(lambda (ksa)
      (error "Obviation KS ~s was not obviated" ksa))
  :ks-class obviated-ks)
		   
;;; ---------------------------------------------------------------------------

(define-ks create-uc-one-instance-event-ks
  :trigger-events ((create-instance-event uc-one))
  :execution-function 
  #'(lambda (ksa)
      (format t "~&;; Create-uc-one-instance-event ks executed~%")
      (let ((trigger-instance (sole-trigger-instance-of ksa)))
	(make-instance 'uc-two :uc-one trigger-instance))
      (values)))
		   
;;; ---------------------------------------------------------------------------

(define-ks add-uc-one-to-space-instance-event-ks
    :trigger-events ((add-instance-to-space-instance-event 
                      uc-one :path (space-1)))
    :precondition-function 
    #'(lambda (ks events)
        (declare (ignore ks events))
        most-positive-rating)
    :execution-function 
    #'(lambda (ksa)
	(declare (ignore ksa))
	(format t "~&;; Add-uc-one-to-space-instance-event ks executed~%")
	(values))
    :ks-class triggered-ks
    :ksa-class triggered-ksa)
		   
;;; ---------------------------------------------------------------------------

(define-ks update-uc-one-value-slot-event-ks
  :trigger-events ((update-nonlink-slot-event uc-one :slot-name value))
  :execution-function 
  #'(lambda (ksa)
      (format t "~&;; Update-uc-one-value-slot-event ks executed~%")
      (format t "~&;; Instantiating ~s...~%" 'space-3)
      (add-instance-to-space-instance 
       (sole-trigger-instance-of ksa)
       (make-space-instance '(space-3) :dimensions '((value :ordered))))
      (values)))
		   
;;; ---------------------------------------------------------------------------

(define-ks add-uc-one-to-future-space-instance-event-ks
    :trigger-events ((add-instance-to-space-instance-event uc-one 
                                                           :path (space-3)))
    :execution-function 
    #'(lambda (ksa)
	(declare (ignore ksa))
	(format t "~&;; Add-uc-one-to-future-space-instance-event ks ~
                        executed~%")
	(values)))
		   
;;; ---------------------------------------------------------------------------

(define-ks link-uc-two-event-ks
  :trigger-events ((link-event uc-two :slot-name uc-one))
  :execution-function 
  #'(lambda (ksa)
      (declare (ignore ksa))
      (format t "~&;; Link-uc-two-event ks executed~%")
      (values)))
		   
;;; ===========================================================================
;;;   Initializations (run at Agenda Shell startup)

(defun initializations (event-name &key &allow-other-keys)
  (declare (ignore event-name))
  ;; Clean up any previous run:
  (reset-gbbopen :retain-classes '((ks :plus-subclasses))
                 :retain-event-printing 't
		 :retain-event-functions 't)
  ;; Enable all KSs:
  (map-instances-of-class #'(lambda (ks) (setf (ks-enabled-p ks) 't)) 'ks)
  ;; Make new space instances:
  (make-space-instance '(space-1)
		       :dimensions '((value :ordered)))
  (make-space-instance '(space-2)
		       :dimensions '((value :ordered)))
  #+when-desired
  (enable-event-printing '(ksa-event :plus-subevents) 'ksa))

(add-event-function 'initializations 'start-control-shell-event
		    ;; Initializations should be done first!
		    :priority 100)

;;; ===========================================================================
;;;   Agenda Shell test

(defun agenda-shell-test (&rest initargs)
  (declare (dynamic-extent initargs))
  (format t "~&;;; Starting Agenda Shell Test....~%")
  (multiple-value-prog1 
      (apply #'start-control-shell initargs)
    (format t "~&;;; Agenda Shell Test Completed~%")))

;;; ---------------------------------------------------------------------------

(when *autorun-modules*
  (format t "~{~&~s~%~}"
	  (multiple-value-list 
	   (agenda-shell-test :save-executed-ksas 't))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


