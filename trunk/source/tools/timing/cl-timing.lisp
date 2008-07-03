;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/timing/cl-timing.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul  3 03:15:56 2008 *-*
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
;;; Copyright (C) 2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-02-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

(defun time-/& (n a b)
  (dotimes (i n) 
    (declare (fixnum i n)) 
    (/& a b)))

(defun time-truncate& (n a b)
  (dotimes (i n) 
    (declare (fixnum i n))
    (truncate& a b)))

(defun time-floor& (n a b)
  (dotimes (i n) 
    (declare (fixnum i n))
    (floor& a b)))

(defun do-division-timing ()
  (let ((iterations 
         ;; keep interations a fixnum (on CLISP):
         (min most-positive-fixnum 100000000)))
    (format t "~&;; Timing /&:")
    (time (time-/& iterations 6 3))
    (format t "~&;; Timing floor&:")
    (time (time-floor& iterations 6 3))
    (format t "~&;; Timing truncate&:")
    (time (time-truncate& iterations 6 3))))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (do-division-timing))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
  