;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/commands.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Apr 22 02:55:24 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

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
;;; Copyright (C) 2004-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Loaded by initiate.lisp.  After loading, handy top-level-loop keyword
;;; commands, such as :gbbopen-tools, :gbbopen-core, :gbbopen-user,
;;; :gbbopen-test, :agenda-shell-user, and :agenda-shell-test are available on
;;; Allegro CL, CLISP, Clozure CL, CMUCL, SCL, ECL, Lispworks, OpenMCL, and
;;; SBCL.  GBBopen keyword commands are also supported in the SLIME REPL.
;;;
;;; In many CL implementations, commands with arguments can be specified in
;;; either list or spread notation.  However, Clozure CL and OpenMCL do not
;;; support spread notation, while Allegro CL and Lispworks do not support
;;; list notation. CLISP also does not support the list representation and
;;; currently does not support command arguments to spread commands in its
;;; native REPL. For example:
;;;
;;;    > :gbbopen-test :create-dirs     [not CLISP, Clozure CL, or OpenMCL]
;;; or
;;;    > (:gbbopen-test :create-dirs)   [not Allegro CL, CLISP, or Lispworks]
;;; or
;;;    > :gbbopen-test                  [CLISP (cannot provide arguments)]
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
;;;  06-04-05 Split out from initiate.lisp.  (Corkill)
;;;  08-20-05 Added :agenda-shell-user command.  (Corkill)
;;;  08-21-05 Added :multiprocessing-test command.  (Corkill)
;;;  10-08-05 Added :tutorial-example command.  (Corkill)
;;;  01-02-05 Changed :multiprocessing to :portable-threads.  (Corkill)
;;;  01-05-06 Added :portable-sockets command.  (Corkill)
;;;  11-13-06 Added :abort-ks-execution-example command.  (Corkill)
;;;  03-17-08 Added :mini-module-user command.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;;   Useful GBBopen Commands

(define-tll-command :start ()
  "Load GBBopen startup.lisp file"
  (startup-gbbopen))

;;; ---------------------------------------------------------------------------
;;;  Mini-Module System

(define-tll-command :mini-module-user (&rest options)
  "Compile and load Mini-Module-User module"
  (startup-module :mini-module-user options :mini-module-user))

;;; ---------------------------------------------------------------------------
;;;  GBBopen Tools

(define-tll-command :gbbopen-tools (&rest options)
  "Compile and load GBBopen Tools module"
  (startup-module :gbbopen-tools options))

(define-tll-command :portable-threads (&rest options)
  "Compile and load Portable Threads module"
  (startup-module :portable-threads options))

(define-tll-command :portable-sockets (&rest options)
  "Compile and load Portable Sockets module"
  (startup-module :portable-sockets options))

;;; ---------------------------------------------------------------------------
;;;  GBBopen Core

(define-tll-command :gbbopen-user (&rest options)
  "Compile and load GBBopen-User module"
  (startup-module :gbbopen-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Agenda Shell

(define-tll-command :agenda-shell-user (&rest options)
  "Compile and load Agenda-Shell-User module"
  (startup-module :agenda-shell-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Extensions (not yet documented)

(define-tll-command :multinode (&rest options)
  "Compile and load GBBopen multi-node support (under construction)"
  (startup-module :multinode options nil))

(define-tll-command :web-inspector (&rest options)
  "Compile and load Web inspector (under construction)"
  (startup-module :web-inspector options nil))

;;; ---------------------------------------------------------------------------
;;;  Example Modules

(define-tll-command :tutorial-example (&rest options)
  "Compile and load GBBopen Tutorial-Example module"
  (startup-module :tutorial-example options :tutorial))

(define-tll-command :abort-ks-execution-example (&rest options)
  "Compile and load the Abort-KS-Execution Example module"
  (startup-module :abort-ks-execution-example options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Test Modules

(define-tll-command :gbbopen-test (&rest options)
  "Compile and load GBBopen Test module"
  (startup-module :gbbopen-test options  :gbbopen-user))

(define-tll-command :timing-tests (&rest options)
  "Compile and load Timing Tests module"
  (startup-module :timing-tests options  :gbbopen-user))

(define-tll-command :portable-threads-test (&rest options)
  "Compile and load Portable-Threads-Test module"
  (startup-module :portable-threads-test options :portable-threads-user))

(define-tll-command :http-test (&rest options)
  "Compile and load Socket/HTTP-Test module"
  (startup-module :http-test options :portable-sockets-user))

(define-tll-command :agenda-shell-test (&rest options)
  "Compile and load Agenda-Shell-Test module"
  (startup-module :agenda-shell-test options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Compile All GBBopen Modules

(define-tll-command :compile-gbbopen (&rest options)
  "Compile all GBBopen modules and exit Common Lisp"
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

(define-tll-command (:lm :add-to-native-help) (&rest module-name-and-options)
  "Load module"
  (if (find-package :mini-module)
      (funcall (intern (symbol-name '#:lm-tll-command) :mini-module) 
               module-name-and-options)
      (mini-module-not-loaded)))
  
(define-tll-command (:cm :add-to-native-help) (&rest module-name-and-options)
  "Compile and load module"
  (if (find-package :mini-module)
      (funcall (intern (symbol-name '#:cm-tll-command) :mini-module) 
               module-name-and-options)
      (mini-module-not-loaded)))

;;; ===========================================================================
;;;   Additional Useful Commands

(define-tll-command (:ds :add-to-native-help) (&optional (obj nil obj-p))
  "Describe object"
  (if obj-p
      (describe (eval obj))
      (format t "~&No object was specified")))

;; Note: * is not set in SBCL's REPL:
(define-tll-command :fi (&rest args)
  "Find instance by name"
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

(define-tll-command (:quit :add-to-native-help
                           #+(or clisp 
                                 clozure
                                 cmu
                                 digitool-mcl
                                 ecl
                                 openmcl
                                 sbcl
                                 scl)
                           :skip-cl-user-function)
    (&rest args)
  "Exit Lisp" 
  (apply #'quit-lisp args))

;; Allegro CL and ECL provide :exit commands already, but we still define them
;; here on all platforms for SLIME interface:
(define-tll-command (:exit :add-to-native-help
                           #+(or allegro
                                 clisp)
                           :skip-cl-user-function)
    (&rest args)
  "Exit Lisp" 
  (apply #'quit-lisp args))

;;  Allegro CL provides :pa, but we repeat for SLIME interface:
(define-tll-command (:pa :add-to-native-help) (&optional package)
  "Set/show current package"
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
(define-tll-command (:commands :add-to-native-help) ()
  "Show all extended-REPL commands"
  (dolist (command (sort (copy-list *extended-repl-commands*)
			 #'(lambda (a b)
			     (string< 
			      (the simple-base-string (symbol-name a))
			      (the simple-base-string (symbol-name b))))
			 :key #'first))
    (format t "~&~s~24,4t~@[~a~]~%"
	    (first command)
	    (third command))))

#+(or clozure openmcl)
(define-tll-command :help ()
  "Show REPL commands"
  (ccl::check-toplevel-command ':?))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
