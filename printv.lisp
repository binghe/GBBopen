;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/printv.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Oct 28 05:34:46 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                            Handy PRINTV Macro
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-05-09 Copied from module-manager.lisp for stand-alone use.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ---------------------------------------------------------------------------
;;;  NOTE: Keep synchronized with the original PRINTV & PRINTV-PRINTER
;;;  definitions in module-manager.lisp
;;; ---------------------------------------------------------------------------

(defun printv-printer (forms forms-values-lists
                       ;; Allow for customized printv-style printv'ers:
                       &optional values-trans-fn)
  (let ((*print-readably* nil))
    (loop
        for form in forms
        and form-values-list in forms-values-lists
        do (typecase form
             (keyword (format *trace-output* "~&;; ~s~%" form))
             (string (format *trace-output* "~&;; ~a~%" form))
             (t (format *trace-output* 
                        "~&;;  ~w =>~{ ~w~^;~}~%" 
                        form 
                        (if values-trans-fn
                            (funcall values-trans-fn form-values-list)
                            form-values-list))))))
  (force-output *trace-output*)
  (values-list (first (last forms-values-lists))))

;;; ---------------------------------------------------------------------------

(defmacro printv (&rest forms)
  (let ((forms-values-lists (gensym)))
    `(let ((,forms-values-lists
            (list ,.(mapcar #'(lambda (form)
                                `(multiple-value-list ,form))
                            forms))))
       (declare (dynamic-extent ,forms-values-lists))
       (printv-printer ',forms ,forms-values-lists))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
