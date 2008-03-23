;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/current/gbbopen.asd *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Mar 23 10:55:05 2008 *-*
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
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

(require :asdf)

;;; ---------------------------------------------------------------------------

(let ((truename *load-truename*))
  (load (make-pathname 
	 :name "gbbopen-init"
	 :type "lisp"
	 :defaults truename))
  (defparameter *gbbopen-version*
      (with-open-file (version-file 
                       (make-pathname :name "VERSION"
                                      :type nil
                                      :defaults truename))
        (read version-file))))

;;; ===========================================================================
;;;  Compile/load GBBopen's Mini Module system

(mini-module-user :propagate)

(in-package :mini-module)

;;; ===========================================================================
;;;  Utilities

(defun do-mm-component (mm-component &rest options)
  (declare (dynamic-extent options))
  (apply 'compile-module
         ;; Support cross-case mode CLs:
         (read-from-string (format nil ":~a" (asdf:component-name mm-component)))
         :propagate
         options))

;;; ---------------------------------------------------------------------------

(defun mm-component-done-p (mm-component)
  (module-loaded-p
   ;; Support cross-case mode CLs:
   (read-from-string (format nil ":~a" (asdf:component-name mm-component)))))

;;; ---------------------------------------------------------------------------

(defun mm-component-defsystem (module-name &optional no-components-p)
  (eval `(asdf:defsystem ,module-name
	     :author "The GBBopen Project <gbbopen@GBBopen.org>"
	     :maintainer "Dan Corkill <corkill@GBBopen.org>"
             :version ,common-lisp-user::*gbbopen-version*
	     ,@(unless no-components-p
                 `(:components ((:mm-component ,module-name)))))))

;;; ---------------------------------------------------------------------------

(defun mm-component-undefsystem (module-name)
  (remhash module-name asdf::*defined-systems*))

;;; ===========================================================================
;;;  Mini-Module ASDF component

(defclass mm-component (asdf:component)
  ())

(defmethod asdf:component-pathname ((component mm-component))
  nil)

(defmethod asdf:operation-done-p ((op asdf:compile-op)
				  (component mm-component))
  (mm-component-done-p component))

(defmethod asdf:operation-done-p ((op asdf:load-op)
				  (component mm-component))
  (mm-component-done-p component))

(defmethod asdf:operation-done-p ((op asdf:load-source-op)
				  (component mm-component))
  (mm-component-done-p component))

(defmethod asdf:perform ((op asdf:compile-op) (component mm-component))
  (do-mm-component component))

(defmethod asdf:perform ((op asdf:load-op) (component mm-component))
  (do-mm-component component))

(defmethod asdf:perform ((op asdf:load-source-op) (component mm-component))
  (do-mm-component component :source))

;;; ===========================================================================
;;;  Generate an asdf:defsystem for each defined module

(format t "~&;; Defining ASDF defsystems...~%")

(dolist (module-name (list-modules 't))
  (mm-component-defsystem module-name))

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

(mm-component-defsystem :gbbopen 't)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
