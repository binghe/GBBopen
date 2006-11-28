;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/gbbopen-init.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Oct  1 14:24:41 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                Useful (Standard) GBBopen Initializations
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Useful generic GBBopen initialization definitions.  Load this file from
;;; your personal CL initialization file (in place of startup.lisp).  After
;;; loading, handy top-level-loop keyword commands, such as :gbbopen-tools,
;;; :gbbopen, :gbbopen-user, and :gbbopen-test, are available on Allegro CL,
;;; CMUCL, SCL, ECL, Lispworks, OpenMCL, and SBCL.  For example:
;;;
;;;    > :gbbopen-test :create-dirs
;;;
;;; will compile and load GBBopen and perform a basic trip test.
;;;
;;; Equivalent functions such as gbbopen-tools, gbbopen, gbbopen-user, and
;;; gbbopen-test are also defined in the common-lisp-user package.  These
;;; functions can be used in all CL implementations.  For example:
;;;
;;;    > (gbbopen-test :create-dirs)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  12-08-02 File Created.  (Corkill)
;;;  03-21-04 Added :agenda-shell-test.  (Corkill)
;;;  05-04-04 Added :timing-test.  (Corkill)
;;;  06-10-04 Added :multiprocesing.  (Corkill)
;;;  06-04-05 Integrated gbbopen-clinit.cl and gbbopen-lispworks.lisp into
;;;           this file.  (Corkill)
;;;  12-10-05 Added loading of personal gbbopen-tll-commands.lisp file, if one
;;;           is present in (user-homedir-pathname).  (Corkill)
;;;  02-05-06 Always load extended-repl, now that SLIME command interface is
;;;           available.  (Corkill)
;;;  04-07-06 Added gbbopen-modules directory support.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ---------------------------------------------------------------------------
;;;  Used to indicate if GBBopen's startup.lisp file has been loaded:

(defvar *gbbopen-startup-loaded* nil)

;;; ---------------------------------------------------------------------------
;;; Load top-level REPL extensions for CLISP, CMUCL, SCL, ECL, and SBCL and
;;; SLIME command extensions for all:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((truename *load-truename*))
    (load (make-pathname 
	   :name "extended-repl"
	   :type "lisp"
	   :defaults truename))))

;;; ---------------------------------------------------------------------------

(let ((truename *load-truename*))
  (defun startup-gbbopen ()
    (unless *gbbopen-startup-loaded*
      (load (make-pathname 
	     :name "startup"
	     :type "lisp"
	     :defaults truename)))))

;;; ---------------------------------------------------------------------------

(defun set-package (package-specifier)
  (setq *package* (find-package package-specifier))
  ;; For Allegro CL's LEP Emacs interface:
  #+(or allegro scl)
  (when (lep:lep-is-running)
    (lep::eval-in-emacs (format nil "(setq fi:package ~s)"
				(package-name *package*)))))

;;; ---------------------------------------------------------------------------

(defun startup-module (module-name options package)
  (startup-gbbopen)
  (apply (intern (symbol-name '#:compile-module) :mini-module)
	 module-name :propagate options)
  (setf (symbol-value 
	 (intern (symbol-name '#:*last-lm/cm-module*) :mini-module))
	module-name)
  (setf (symbol-value 
	 (intern (symbol-name '#:*last-cm-options*) :mini-module))
	(list* :propagate options))
  (when package 
    (set-package package)
    (import '(common-lisp-user::gbbopen-tools common-lisp-user::gbbopen
              common-lisp-user::gbbopen-user common-lisp-user::gbbopen-test)
            *package*))
  (values))

;;; ===========================================================================
;;;   Define-TLL-command

(defmacro define-tll-command (command lambda-list &rest body)
  `(progn
     (define-top-loop-command 
	 ,command
	 ,lambda-list
       ,@body)
     ;; Define CL-USER package functions on all CL implementations:
     ,(let ((fn-name (intern (symbol-name command) :common-lisp-user)))
	;; don't replace an existing function:
	`(unless (fboundp ',fn-name)
	   (defun ,fn-name ,lambda-list ,@body)))))

;;; ===========================================================================
;;;  Load GBBopen's standard TLL commands

(let ((truename *load-truename*))
  (load (make-pathname 
	 :name "commands"
	 :type "lisp"
	 :defaults truename)))

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-commands.lisp file (or compiled version) in the
;;;  users "home" directory, load it now. 

(load (namestring (make-pathname :name "gbbopen-commands"
				 ;; CLISP and ECL don't handle :unspecific
				 :type #-(or clisp ecl) ':unspecific 
				       #+(or clisp ecl) nil
				 :version ':newest
				 :defaults (user-homedir-pathname)))
      :if-does-not-exist nil)

;;; ===========================================================================
;;;  Load gbbopen-modules-directory processing:

(let ((truename *load-truename*))
  (load (make-pathname 
	 :name "gbbopen-modules-directory"
	 :type "lisp"
	 :defaults truename)))

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-modules directory in the users "home" directory,
;;;  load the commands.lisp file (if present) from each module directory
;;;  that is linked from the gbbopen-modules directory. 

(process-gbbopen-modules-directory "commands")

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
