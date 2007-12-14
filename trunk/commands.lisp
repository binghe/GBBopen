;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/commands.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Dec 14 03:21:42 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  GBBopen Top-Level-Loop (REPL) Commands
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Loaded by gbbopen-init.lisp.  After loading, handy top-level-loop keyword
;;; commands, such as :gbbopen-tools, :gbbopen-core, :gbbopen-user,
;;; :gbbopen-test, :agenda-shell-user, and :agenda-shell-test are available on
;;; Allegro CL, CLISP, Clozure CL, CMUCL, SCL, ECL, Lispworks, OpenMCL, and
;;; SBCL.  GBBopen keyword commands are also supported in the SLIME REPL.
;;;
;;; In many CL implementations, commands with arguments can be specified in
;;; either list or spread notation.  However, Clozure CL, OpenMCL, and the
;;; SLIME interface do not support spread notation, while Allegro CL and
;;; Lispworks do not support list notation. For example:
;;;
;;;    > :gbbopen-test :create-dirs        [not Clozure CL, OpenMCL or SLIME]
;;; or
;;;    > (:gbbopen-test :create-dirs)      [not Allegro CL or Lispworks]
;;;
;;; will compile and load GBBopen and perform a basic trip test.
;;;
;;; On all CL implementations, functions invoking each top-level command, such
;;; as gbbopen-tools, gbbopen-core, gbbopen-user, gbbopen-test,
;;; agenda-shell-user and agenda-shell-test, are defined in the
;;; common-lisp-user package.  For example:
;;;
;;;    > (gbbopen-test :create-dirs)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-04-05 Split out from gbbopen-init.lisp.  (Corkill)
;;;  08-20-05 Added :agenda-shell-user command.  (Corkill)
;;;  08-21-05 Added :multiprocessing-test command.  (Corkill)
;;;  10-08-05 Added :tutorial-example command.  (Corkill)
;;;  01-02-05 Changed :multiprocessing to :portable-threads.  (Corkill)
;;;  01-05-06 Added :portable-sockets command.  (Corkill)
;;;  11-13-06 Added :abort-ks-execution-example command.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;;   Useful GBBopen Commands  (Keep gbbopen.asd consistent with these!)

(define-tll-command :start ()
  "Load GBBopen startup.lisp file"
  (startup-gbbopen))

;;; ---------------------------------------------------------------------------
;;;  Mini-Module System

(define-tll-command :mini-module (&rest options)
  "Compile and Load GBBopen's Mini-Module system"
  (startup-module :mini-module options :mini-module))

;;; ---------------------------------------------------------------------------
;;;  GBBopen Tools

(define-tll-command :gbbopen-tools (&rest options)
  "Compile and Load GBBopen Tools Module"
  (startup-module :gbbopen-tools options :gbbopen-tools))

(define-tll-command :portable-threads (&rest options)
  "Compile and Load Portable Threads Module"
  (startup-module :portable-threads options :portable-threads))

(define-tll-command :portable-sockets (&rest options)
  "Compile and Load Portable Sockets Module"
  (startup-module :portable-sockets options :portable-sockets))

(define-tll-command :polling-functions (&rest options)
  "Compile and Load Polling Functions Module"
  (startup-module :polling-functions options :gbbopen-user))

(define-tll-command :os-interface (&rest options)
  "Compile and Load GBBopen-Tools OS-interface Module"
  (startup-module :os-interface options :gbbopen-tools))

;;; ---------------------------------------------------------------------------
;;;  GBBopen Core

(define-tll-command :gbbopen-core (&rest options)
  "Compile And Load GBBopen Module"
  (startup-module :gbbopen-core options :gbbopen))

(define-tll-command :gbbopen-user (&rest options)
  "Compile And Load GBBopen-User Module"
  (startup-module :gbbopen-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Agenda Shell

(define-tll-command :agenda-shell (&rest options)
  "Compile And Load Agenda-Shell Module"
  (startup-module :agenda-shell options :agenda-shell))

(define-tll-command :agenda-shell-user (&rest options)
  "Compile And Load Agenda-Shell-User Module"
  (startup-module :agenda-shell-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Extensions (not yet documented)

(define-tll-command :multinode (&rest options)
  "Multi-Node Support (under construction)"
  (startup-module :multinode options nil))

(define-tll-command :web-inspector (&rest options)
  "Web Inspector (under construction)"
  (startup-module :web-inspector options nil))

;;; ---------------------------------------------------------------------------
;;;  Example Modules

(define-tll-command :tutorial-example (&rest options)
  "Compile And Load GBBopen Tutorial Example Module"
  (startup-module :tutorial-example options :gbbopen-user))

(define-tll-command :abort-ks-execution-example (&rest options)
  "Compile And Load the Abort KS Execution Example Module"
  (startup-module :abort-ks-execution-example options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Test Modules

(define-tll-command :gbbopen-test (&rest options)
  "Compile And Load GBBopen Test Module"
  (startup-module :gbbopen-test options  :gbbopen-user))

(define-tll-command :timing-tests (&rest options)
  "Compile And Load Timing Tests Module"
  (startup-module :timing-tests options  :gbbopen-user))

(define-tll-command :portable-threads-test (&rest options)
  "Compile And Load Portable Threads Test Module"
  (startup-module :portable-threads-test options :portable-threads-user))

(define-tll-command :http-test (&rest options)
  "Compile And Load Socket/HTTP Test Module"
  (startup-module :http-test options :portable-sockets-user))

(define-tll-command :agenda-shell-test (&rest options)
  "Compile And Load Agenda-Shell-Test Module"
  (startup-module :agenda-shell-test options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Compile All GBBopen Modules

(define-tll-command :compile-gbbopen (&rest options)
  "Compile All GBBopen Modules"
  (let ((*automatically-create-missing-directories* 't)
	(*autorun-modules* nil))
    (declare (special *automatically-create-missing-directories*
		      *autorun-modules*))
    (startup-module :compile-gbbopen options :gbbopen-user)))

;;; ===========================================================================
;;;  Mini-Module Commands

(defun mini-module-not-loaded ()
  (format t "~&The mini-module facility has not been loaded.~%")
  (values))

(define-tll-command :lm (&rest options)
  "Load Module"
  (if (find-package :mini-module)
      (funcall (intern (symbol-name '#:lm-tll-command) :mini-module) options)
      (mini-module-not-loaded)))
  
(define-tll-command :cm (&rest options)
  "Compile And Load Module"
  (if (find-package :mini-module)
      (funcall (intern (symbol-name '#:cm-tll-command) :mini-module) options)
      (mini-module-not-loaded)))

;;; ===========================================================================
;;;   Additional Useful Commands

(define-tll-command :ds (obj)
  "Describe"
  (describe (eval obj)))

(define-tll-command :dsbb ()
  "Describe Blackboard Repository"
  (funcall (intern (symbol-name '#:describe-blackboard-repository) :gbbopen)))

;;; * is not set in SBCL

(define-tll-command :fi (&rest args)
  "Find Instance by Name"
  (let ((instance
         (apply (intern (symbol-name '#:find-instance-by-name) :gbbopen) 
                args)))
    (cond (instance
           (setf * instance)
           (format t "~&Found ~s (assigned to ~s)~%" instance '*)
           (force-output)
           instance)
          (t (format t "~&No unit instance named ~s was found~%"
                     (first args))
             (force-output)))))          

(defun quit-lisp (&rest args)
  #+sbcl
  (let ((swank-package (find-package :swank)))
    (when swank-package
      (let ((quit-lisp-fn (intern (symbol-name 'quit-lisp) swank-package)))
	(when (fboundp quit-lisp-fn)
	  (funcall quit-lisp-fn)))))
  (apply
   ;; Avoid infinite #'quit recursion in Allegro:
   #+allegro #'exit
   #-allegro #'quit
   args))

(define-tll-command :quit (&rest args)
  "Exit Lisp" 
  (apply #'quit-lisp args))

;;  Allegro CL and ECL provide :exit, but we repeat for SLIME interface:
(define-tll-command :exit (&rest args)
  "Exit Lisp" 
  (apply #'quit-lisp args))

;;  Allegro CL provides :pa, but we repeat for SLIME interface:
(define-tll-command :pa (&optional package)
  "Set/Show Current Package"
  (when package
    (let ((the-package (find-package package)))
      (if the-package 
	  (set-package the-package)
	  (format t "~&The package ~s is not defined.~%" package))))
  (format t "~&The ~s package is current.~%" 
	  (package-name *package*)))

;;; ===========================================================================
;;;   Top-Level-Loop Command Help (for those CLs without native help)

#+(or clisp cmu scl sbcl)
(define-tll-command :help ()
  "Show REPL commands"
  (dolist (command (sort (copy-list *extended-repl-commands*)
			 #'(lambda (a b)
			     (string< 
			      (the simple-base-string (symbol-name a))
			      (the simple-base-string (symbol-name b))))
			 :key #'first))
    (format t "~&~s~20,4t~@[~a~]~%"
	    (first command)
	    (third command))))

#+(or clozure openmcl)
(define-tll-command :help ()
  "Show REPL commands"
  (ccl::check-toplevel-command ':?))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
