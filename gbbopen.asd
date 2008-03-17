;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/current/gbbopen.asd *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Mar 17 04:46:06 2008 *-*
;;;; *-* Machine: cyclone.local *-*

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
;;; Copyright (C) 2005-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;;  Interfaces GBBopen's mini-module facility, a lightweight and easy to use
;;;  mechanism for maintaining (compiling and loading) modules, with ASDF.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  12-12-05 File Created.  (Corkill)
;;;  03-17-08 Added :mini-module-user.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

(require :asdf)

(defparameter *gbbopen-version* "0.9.8")

;;; ---------------------------------------------------------------------------

(let ((truename *load-truename*))
  (load (make-pathname 
	 :name "gbbopen-init"
	 :type "lisp"
	 :defaults truename)))

;;; ---------------------------------------------------------------------------
;;;  We have trivially completed all of the :gbbopen system operations by
;;;  loading this file:

(defclass gbbopen (asdf:component)
  ())

(defmethod asdf:component-pathname ((component gbbopen))
  nil)

(defmethod asdf:operation-done-p ((op asdf:compile-op) (component gbbopen))
  t)

(defmethod asdf:operation-done-p ((op asdf:load-op) (component gbbopen))
  t)

(defmethod asdf:operation-done-p ((op asdf:load-source-op) (component gbbopen))
  t)

(defmethod asdf:perform ((op asdf:compile-op) (component gbbopen)))

(defmethod asdf:perform ((op asdf:load-op) (component gbbopen)))

(defmethod asdf:perform ((op asdf:load-source-op) (component gbbopen)))

(eval `(asdf:defsystem :gbbopen
           :author "The GBBopen Project <gbbopen@GBBopen.org>"
           :maintainer "Dan Corkill <corkill@GBBopen.org>"
           :version ,*gbbopen-version*))

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

(dolist (module-name '(:mini-module
                       :mini-module-user
                       ;; GBBopen Tools
                       :gbbopen-tools 
                       :portable-threads
                       :portable-sockets
                       :polling-functions
		       :os-interface 
                       ;; GBBopen Core
                       :gbbopen-core 
                       :gbbopen-user
		       ;; Agenda Shell
                       :agenda-shell
                       :agenda-shell-user
                       ;; Example Modules
                       :tutorial-example 
                       :abort-ks-execution-example
                       ;; Test Modules
		       :gbbopen-test 
                       :portable-threads-test
                       :http-test 
		       :agenda-shell-test 
                       ;; Compile All GBBopen Modules
                       :compile-gbbopen))
  (eval `(asdf:defsystem ,module-name
	     :author "The GBBopen Project <gbbopen@GBBopen.org>"
	     :maintainer "Dan Corkill <corkill@GBBopen.org>"
	     :version ,*gbbopen-version*
	     :components ((:mini-module ,module-name)))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
