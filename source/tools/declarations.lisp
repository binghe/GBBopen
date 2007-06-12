;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/declarations.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jun 12 12:20:44 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      Optimization Declarations
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
;;;  07-18-02 File Created.  (Corkill)
;;;  01-18-04 Added NYI error signalling.  (Corkill)
;;;  06-22-04 Added (debug 0) to with-full-optimization.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*generate-nyi-errors*	; not documented
	    allow-redefinition		; not documented
	    nyi				; not documented
	    unbound-value-indicator
	    without-cmu/sbcl-optimization-warnings ; not documented
	    with-full-optimization)))

;;; ---------------------------------------------------------------------------

(defmacro allow-redefinition (&body body)
  `(#+allegro excl:without-redefinition-warnings
    #+lispworks system::without-warning-on-redefinition
    #-(or allegro lispworks)
    progn
    ,@body))

;;; ---------------------------------------------------------------------------

(defmacro with-full-optimization ((&key) &body body)
  ;;  The feature :full-safety disables with-full-optimization optimizations:
  `(locally #+full-safety ()
            #-full-safety (declare (optimize (speed 3) (safety 0) (debug 0)
					     (compilation-speed 0)))
            ,@body))

;;; ---------------------------------------------------------------------------

(defmacro without-cmu/sbcl-optimization-warnings (&body body)
  ;;  Suppress CMUCL and SBCL compilation notes on failed optimizations by
  ;;  lowering the speed setting.  (It would be better to find a good way to
  ;;  supress these--and only these--optimization warnings.)
  #+(or cmu sbcl)
  `(locally (declare (optimize (speed 1)))
     ,@body)
  #-(or cmu sbcl)
  `(progn ,@body))

;;; ---------------------------------------------------------------------------
;;;   NYI wrapper (for use with code that is not yet ready for prime time)

(defvar *generate-nyi-errors* t)

(defmacro nyi (&body body)
  `(progn
     (when *generate-nyi-errors*
       (error "Not yet implemented."))
     ,@body))

;;; ---------------------------------------------------------------------------

(defconstant unbound-value-indicator
    ;; We use Allegro's keyword symbol (as good as any choice...)
    :---unbound---)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


