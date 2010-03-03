;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/timing/cl-timing.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Mar  3 09:48:19 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        CL Implementation Timing 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-02-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(cl-timing)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *test-value*)
  
  (defmacro timer (n &body body)
    `(let ((%start-time% (get-internal-real-time)))
       (with-full-optimization ()
           (dotimes (i ,n) 
             (declare (fixnum i)) 
             (setf *test-value* ,@body)))
       (/ (- (get-internal-real-time) %start-time%)
          (load-time-value (float internal-time-units-per-second))))))

;;; ---------------------------------------------------------------------------

(defun do-division-timing (&optional
                           (numerator 6)
                           (denominator 3))
  (let ((iterations 
         ;; keep interations a fixnum (on CLISP):
         (min most-positive-fixnum 200000000)))
    (format t "~&;;   Division timing (~:d iterations)..."
            iterations)
    (let ((times
           `(,(timer iterations (/& numerator denominator))
             ,(timer iterations (floor& numerator denominator))
             ,(timer iterations (truncate& numerator denominator)))))
      (format t 
             "~{~&;;     /&:       ~6,2f seconds~
                ~%;;     floor&:   ~6,2f seconds~
                ~%;;     truncate&:~6,2f seconds~%~}"
             times))))

;;; ---------------------------------------------------------------------------

(defun cl-timing ()
  (format t "~&;;; Starting timings...")
  (do-division-timing)
  (format t "~&;;; Timings completed.~%"))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (cl-timing))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
  