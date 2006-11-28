;;;; -*- Mode:Common-Lisp; Package:PORTABLE-THREADS-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/test/portable-threads-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Mar 22 17:05:03 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Simple Portable Threads Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  08-21-05 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :portable-threads-user) 
    (defpackage :portable-threads-user
      (:use :common-lisp :portable-threads))))

(in-package :portable-threads-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*autorun-gbbopen-modules*)))

;;; ---------------------------------------------------------------------------

#+multiprocessing-not-available
(error "Multiprocessing is not available in ~s running on ~s."
       (lisp-implementation-type) 
       (machine-type))

(defparameter *w* nil)
(defparameter *x* 1)
(defparameter *y* 2)
(defparameter *z* 3)

(defun portable-threads-tests ()
  (let* ((gate (make-gate nil))
	 (process 
	  (spawn-process
	   "Portable Threads Test"
	   #'(lambda (gate)
	       (format t "~&;; Portable threads process started.~%")
	       #-(or thread-scheduling-not-available openmcl)
	       (progn
		 (open-gate gate)
		 (format t "~&;; Hibernating test process...~%")
		 (finish-output)
		 (hibernate-process)
		 (format t "~&;; Awakened.~%")
		 (finish-output))
	       (let ((*v* 4)
		     (*x* 5)
		     (*y* 6))
		 (declare (special *v* *x* *y))
		 (makunbound '*y*)
		 (open-gate gate)			       
		 ;; symbol-value-in-process can block on hibernating
		 ;; processes, so we simply sleep for a while:
		 (sleep 1000)))
	   gate)))
    (format t "~&;; Starting portable threads tests...~%")
    ;; Allow process to start up (also tests process-wait):
    #-(or thread-scheduling-not-available openmcl)
    (progn
      (process-wait "Spawned process startup" #'gate-open-p gate)
      (format t "~&;; Gate open.~%")
      (finish-output))
    ;; Test awaken of a hibernating process:
    #-thread-scheduling-not-available
    (progn 
      (format t "~&;; Awakening test process...~%")
      (finish-output)
      (awaken-process process)
      (close-gate gate))
    (process-wait "Bindings complete" #'gate-open-p gate)
    ;; Test symbol-value-in-process:
    (flet ((test (symbol result)
	     (let ((test-result
		    (multiple-value-list 
		     (symbol-value-in-process symbol process))))
	       (unless (equal test-result result)
		 (warn "~s call with ~s failed:~%~9t~s expected; ~s returned."
		       'symbol-value-in-process symbol result test-result)))))
      (format t "~&;; Testing symbol-value-in-process...~%")
      (finish-output)
      (test '*v* '(4 t))
      (test '*w* '(nil t))
      (test '*x* '(5 t))
      (test '*y* '(nil nil))
      (test '*z* '(3 t))
      (test 'pi  (list pi 't))
      (test '*garbage* '(nil nil)))
    (finish-output)
    (kill-process process)
    (format t "~&;; Symbol-value-in-process test completed.~%")
    (finish-output))
  ;; Test atomic-incf/decf basic operation (atomic-operation exclusion not
  ;; tested):
  (format t "~&;; Testing atomic-incf/decf...~%")
  (finish-output)
  (let* ((x 0))
    (atomic-incf x 2)
    (atomic-decf x)
    (unless (= x 1)
      (error "Incorrect atomic-incf/decf result: ~s"
	     x)))
  (format t "~&;; Atomic-incf/decf test completed.~%")
  (finish-output)
  ;; Test recursive locking and with-process-lock returned values:
  (let ((counter 0)
	(lock (make-process-lock :name "Test")))
    (format t "~&;; Testing recursive locking...~%")
    (finish-output)
    (with-process-lock (lock :whostate "First level")
      (incf counter)
      (spawn-process 
       "Lock contender" 
       #'(lambda (counter lock) 
	   (with-process-lock (lock :whostate "Contender level")
	     (decf counter)))
       counter
       lock)
      ;; Give the contender a chance:
      (sleep 1)
      (with-process-lock (lock :whostate "Second level")
	(incf counter))
      ;; Give the contender another chance:
      (sleep 1)
      (unless (= counter 2)
	(error "Incorrect lock-counter value (should be 2): ~s" counter)))
    (format t "~&;; Testing with-process-lock returned values...~%")
    (finish-output)
    (let ((returned-values
	   (multiple-value-list (with-process-lock (lock) (values 1 2)))))
      (unless (equal returned-values '(1 2))
	(error "Incorrect ~s returned values: ~s"
	       'with-process-lock
	       returned-values))))
  (format t "~&;; Recursive locking test completed.~%")
  (finish-output)
  (format t "~&;; Portable threads tests completed.~%"))

;;; ---------------------------------------------------------------------------

(when *autorun-gbbopen-modules*
  (portable-threads-tests))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


