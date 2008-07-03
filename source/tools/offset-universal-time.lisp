;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/offset-universal-time.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 30 01:14:29 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                   Offset Universal Time Entities
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2007-2008, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; This file is self-contained and can be used stand-alone.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-07 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':gbbopen-tools)
    (make-package ':gbbopen-tools 
                  :use '(:common-lisp))))

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*ot-base*
            check-ot-base
            ot2ut
            set-ot-base
            ut2ot)))

;;; ===========================================================================
;;;   Offset Universal Time
;;;
;;; Standard Common Lisp has three time representations: Decoded Time,
;;; Universal Time, and Internal Time.  We add a fourth representation: Offset
;;; Universal Time (OT).  OT is Universal Time (UT) that is offset by an
;;; integer time-base value so that the most often used OT values in an
;;; application are fixnums.
;;;
;;; Nearly all CL implementations provide fixnums of at least 30 bits (34
;;; years of time range) or more, but CLISP on 32-bit machines provides only
;;; 25 bits (388 days).  The ANSI standard requires an implementation to
;;; provide fixnums with at least 16 bits (only 18 hours), but fortunately CL
;;; implementations are considerably more generous!
;;;
;;; Applications that are representing time values that exceed the fixnum
;;; range need to consider what value of *ot-base* works best in reducing
;;; bignum costs.  Of course, existing OT values will appear shifted if
;;; *ot-base* is changed.

(defvar *ot-base* "You must set the offset-universal-time base (set-ot-base).")

;;; ---------------------------------------------------------------------------
;;; Warn if the CL implementation doesn't have at least 25-bit fixnums:

(defun small-fixnum-ot-warning ()
  (let ((fixnum-size #.(1+ (integer-length most-positive-fixnum))))
    (warn "The ~s-bit fixnums on ~a (~a) provide only ~,1f days of fixnum ~
           OT values."
          fixnum-size
          (lisp-implementation-type) 
          (machine-type)
          (/ (expt 2 fixnum-size) #.(float (* 60 60 24))))))

(let ((fixnum-size (1+ (integer-length most-positive-fixnum))))
  ;; Suppress unreachable code warning in CMUCL
  #+cmu
  (declare (optimize (extensions:inhibit-warnings 3)))
  (when (< fixnum-size 25)
    (small-fixnum-ot-warning)))

;;; ---------------------------------------------------------------------------

(defun ut2ot (&optional (universal-time (get-universal-time)))
  (- universal-time *ot-base*))

(defcm ut2ot (&optional (universal-time '(get-universal-time)))
  `(- ,universal-time *ot-base*))

;;; ---------------------------------------------------------------------------

(defun ot2ut (offset-universal-time)
  (+ offset-universal-time *ot-base*))

(defcm ot2ut (offset-universal-time)
  `(+ ,offset-universal-time *ot-base*))

;;; ---------------------------------------------------------------------------

(defun check-ot-base (&optional suppress-warning)
  ;;; Return true if the current time can be represented as a fixnum;
  ;;; otherwise issue a warning and return nil.
  (or (typep (ut2ot) 'fixnum)
      (unless suppress-warning
        (warn "The current time represented as an offset-universal-time is ~
               not a fixnum."))))

;;; ---------------------------------------------------------------------------

(defun set-ot-base (&rest args)
  ;;; Sets the offset-universal-time base to today's date (no supplied args)
  ;;; or to a specified date:
  (declare (dynamic-extent args))
  (setf *ot-base*
        (+ (if args
               (destructuring-bind (date month year &optional (time-zone 0))
                   args
                 (encode-universal-time 0 0 0 date month year time-zone))
               ;; Maintain 0 hours, minutes, & seconds GMT:
               (let ((seconds-in-a-day #.(* 24 60 60)))
                 (* (floor (get-universal-time) seconds-in-a-day)
                    seconds-in-a-day)))
           (expt 2 24)))
  (when args (check-ot-base))
  *ot-base*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


