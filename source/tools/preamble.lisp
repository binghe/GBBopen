;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/preamble.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jul 20 18:14:19 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        GBBopen-Tools Preamble
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
;;;  03-15-04 File created.  (Corkill)
;;;  06-15-05 Added add-package-nickname.  (Corkill)
;;;  09-13-05 Added hyperdoc-filename.  (Corkill)
;;;  09-28-05 Added import of *preferred-browser* setting.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(unless (find-package :gbbopen-tools)
  (defpackage :gbbopen-tools 
    (:use :common-lisp)))

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((mini-module-package (find-package :mini-module)))
    (when mini-module-package
      (use-package (list mini-module-package)))))

;;; ---------------------------------------------------------------------------
;;;  Import user's preferred browser setting

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*preferred-browser*
	    common-lisp-user::*inf-reader-escape-hook*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(add-package-nickname        ; not documented
            delete-instance             ; needed for :queue module (see below)
            hyperdoc-filename           ; not yet documented
	    hyperdoc-url		; not yet documented
            insert-on-queue             ; needed for :queue module (see below)
	    printv
	    with-gensyms
	    with-once-only-bindings)))	; not yet documented

;;; ---------------------------------------------------------------------------
;;; To allow the :queue module to be used with or without :gbbopen-core, we
;;; have to export the symbols delete-instance and insert-on-queue (above)
;;; from :gbbopen-tools.  We define the delete-instance generic function here
;;; as well:

(defgeneric delete-instance (instance))

;;; ===========================================================================
;;;  Convenient package-nickname adder

(defun add-package-nickname (nickname package)
  (check-type nickname string)
  (let ((package (find-package package))
	(nickname-package (find-package nickname)))
    (if nickname-package
	(unless (eq package nickname-package)
	  (error "Another package is named ~s" nickname))
	(rename-package package
			(package-name package)
			(cons nickname (package-nicknames package))))))

;;; ===========================================================================
;;;  With-gensyms
;;;
;;;  GBBopen-tools version of the widely used gensym binding macro
;;;
;;; Placed here to make this macro available ASAP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms ((&rest symbols) &body body)
    `(let ,(mapcar
	    #'(lambda (symbol) `(,symbol (gensym)))
	    symbols)
       ,@body)))

;;; ===========================================================================
;;;  With-once-only-bindings  
;;;
;;; GBBopen's version of the "once-only" macro-writing macro which 
;;; ensures that the specified forms are only evaluated once and in the
;;; specified order.
;;;
;;; Placed here to make this macro available ASAP

(defmacro with-once-only-bindings ((&rest symbols) &body body)
  (let ((gensyms (mapcar #'(lambda (symbol)
			     (declare (ignore symbol))
			     (gensym))
			 symbols)))
    `(let (,@(mapcar #'(lambda (gensym) `(,gensym (gensym)))
		     gensyms))
       `(let (,,@(mapcar #'(lambda (symbol gensym) ``(,,gensym ,,symbol))
			 symbols
			 gensyms))
	  ,(let (,@(mapcar #'(lambda (symbol gensym) `(,symbol ,gensym))
			   symbols
			   gensyms))
	     ,@body)))))

;;; ===========================================================================
;;;  Printv
;;;
;;;  A handy debugging macro
;;;
;;; Placed here to make this macro available ASAP

(defmacro printv (&rest forms)
  (with-gensyms (values)
    `(let* ((,values (list ,@(mapcar #'(lambda (form)
					 `(multiple-value-list ,form))
				     forms))))
       (declare (dynamic-extent ,values))
       (loop for form in ',forms
	   and value in ,values
	   do (typecase form
		(keyword (format *trace-output* "~&;; ~s~%" form))
		(string (format *trace-output* "~&;; ~a~%" form))
		(t (format *trace-output* 
			   "~&;;  ~w =>~{ ~w~^;~}~%" form value))))
       (force-output *trace-output*)
       (values-list (first (last ,values))))))

;;; ===========================================================================
;;;   Hyperdoc lookup helper

(defun hyperdoc-filename (symbol)
  (namestring
   (merge-pathnames 
    (format nil "ref-~a.html"
	    (let ((basename (string-downcase (symbol-name symbol))))
	      (cond 
	       ;; Global variables:
	       ((eql #\* (aref basename 0))
		(format nil "~a-var" 
			(subseq basename 
				1 
				(the fixnum
				  (1- (the fixnum (length basename)))))))
	       ;; Using ~a above handles keyword-symbol conversions
	       ;; automatically...
	       (t basename))))
    (load-time-value
     (compute-relative-directory :gbbopen-root '(:up "hyperdoc") nil)))))

;;; ---------------------------------------------------------------------------

(defun hyperdoc-url (symbol)
  (let ((filename (hyperdoc-filename symbol)))
    (when (probe-file filename)
      (format nil "file://~a" filename))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
