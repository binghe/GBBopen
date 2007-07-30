;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/polling-functions.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jul 30 10:02:15 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         Polling-Function Entities
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
;;;  01-02-06 Split out from multiprocessing.lisp.  (Corkill)
;;;  11-15-06 Added polling-sleep.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*polling-sleep-poll-interval* ; not yet documented
            add-polling-function 
	    describe-all-polling-functions
            polling-sleep               ; not yet documented
	    remove-all-polling-functions
	    remove-polling-function
	    run-polling-functions)))

;;; ---------------------------------------------------------------------------

(defvar *polling-sleep-poll-interval* .2)

(defvar *polling-functions* nil)

;;; ===========================================================================
;;;   Polling Functions (primarily for non-threaded CLs)

(defun describe-all-polling-functions ()
  (format t "~&; Polling functions:~:[  None~;~]~%" *polling-functions*)
  (dolist (item *polling-functions*)
    (format t "; ~6d ~s~%" (cdr item) (car item)))
  (values))

(defun remove-all-polling-functions ()
  (setq *polling-functions* nil))

(defun remove-polling-function (function)
  (setq *polling-functions* (delete function *polling-functions* :key #'car))
  function)

(defun add-polling-function (function &key (priority 0))
    ;; Do the user a favor, and delete an existing copy, if present:
    (remove-polling-function function)
    (setq *polling-functions* (nsorted-insert (cons function priority)
					      *polling-functions*
					      #'>
					      #'cdr))
    function)
  
(defun run-polling-functions ()
  (dolist (item *polling-functions*)
    (with-simple-restart
	(:abort "Continue with the next polling function")
      (funcall (car item)))))

;;; ---------------------------------------------------------------------------

(defun polling-sleep (seconds &optional (poll-interval 
                                         *polling-sleep-poll-interval*))
  #-threads-not-available
  (declare (ignore poll-interval))
  #-threads-not-available
  (sleep seconds)
  #+threads-not-available
  (if (< seconds 1)
      (sleep seconds)
      (let ((end-time (+ (get-internal-real-time) 
                         (* internal-time-units-per-second seconds))))
        (loop until (> (get-internal-real-time) end-time)
            do (sleep poll-interval)
               (thread-yield)))))

#-(or full-safety threads-not-available)
(define-compiler-macro polling-sleep (seconds &optional poll-interval)
  (declare (ignore poll-interval))
  `(sleep ,seconds))

;;; ---------------------------------------------------------------------------
;;;  Run the polling functions on thread-yield on non-threaded CLs:

#+threads-not-available
(pushnew 'run-polling-functions *non-threaded-polling-function-hook*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


