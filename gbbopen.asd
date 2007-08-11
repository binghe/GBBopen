;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/gbbopen.asd *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Aug 10 21:24:41 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         GBBopen ASDF Interface
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;;  Interfaces GBBopen's mini-module facility, a lightweight and easy to use
;;;  mechanism for maintaining (compiling and loading) modules, with ASDF.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  12-12-05 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

(require :asdf)

(let ((truename *load-truename*))
  (load (make-pathname 
	 :name "gbbopen-init"
	 :type "lisp"
	 :defaults truename)))

;;; ---------------------------------------------------------------------------

(defun do-mini-module (mini-module)
  (funcall
   ;; Support cross-case mode CLs:
   (read-from-string 
    (format nil "cl-user::~a" (asdf:component-name mini-module)))))

;;; ---------------------------------------------------------------------------

(defun mini-module-done-p (mini-module)
  (when (find-package ':mini-module)
    (funcall (intern (symbol-name '#:module-loaded-p) ':mini-module)
	     ;; Support cross-case mode CLs:
	     (read-from-string 
	      (format nil ":~a" (asdf:component-name mini-module))))))

;;; ---------------------------------------------------------------------------

(defclass mini-module (asdf:component)
  ())

(defmethod asdf:component-pathname ((component mini-module))
  nil)

(defmethod asdf:operation-done-p ((op asdf:compile-op)
				  (component mini-module))
  (mini-module-done-p component))

(defmethod asdf:operation-done-p ((op asdf:load-op)
				  (component mini-module))
  (mini-module-done-p component))

(defmethod asdf:operation-done-p ((op asdf:load-source-op)
				  (component mini-module))
  (mini-module-done-p component))

(defmethod asdf:perform ((op asdf:compile-op) (component mini-module))
  (do-mini-module component))

(defmethod asdf:perform ((op asdf:load-op) (component mini-module))
  (do-mini-module component))

(defmethod asdf:perform ((op asdf:load-source-op) (component mini-module))
  (do-mini-module component))

;;; ---------------------------------------------------------------------------
;;;  Still to do: Generate asdf:defsystems directly from the
;;;  define-tll-command machinery

(dolist (module-name '(:mini-module :gbbopen-tools :multiprocessing
		       :os-interface :gbbopen-core :gbbopen-user
		       :agenda-shell :agenda-shell-user :gbbopen-graphics
		       :gbbopen-test :multiprocessing-test :http-test 
		       :agenda-shell-test))
  (eval `(asdf:defsystem ,module-name
	     :author "The GBBopen Project <gbbopen@GBBopen.org>"
	     :maintainer "Dan Corkill <corkill@GBBopen.org>"
	     :version "0.9.5"
	     :components ((:mini-module ,module-name)))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
