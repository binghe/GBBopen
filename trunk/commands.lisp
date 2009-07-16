;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/commands.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul 16 16:26:10 2009 *-*
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
;;; Copyright (C) 2004-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Loaded by initiate.lisp.  After loading, handy top-level-loop keyword
;;; commands, such as :gbbopen-tools, :gbbopen-core, :gbbopen-user,
;;; :gbbopen-test, :agenda-shell-user, and :agenda-shell-test are available on
;;; Allegro CL, CLISP, Clozure CL, CMUCL, SCL, ECL, Lispworks, and SBCL.
;;; GBBopen keyword commands are also supported in the SLIME REPL.
;;;
;;; In many CL implementations, commands with arguments can be specified in
;;; either list or spread notation.  However, Allegro CL, CLISP, and Lispworks
;;; do not support list notation. CLISP versions prior to 2.45 only support
;;; the spread notation---but without arguments. For example:
;;;
;;;    > :gbbopen-test :create-dirs
;;; or
;;;    > (:gbbopen-test :create-dirs)    [not Allegro CL, CLISP, or Lispworks]
;;; or
;;;    > :gbbopen-test             [CLISP pre-2.45 (cannot provide arguments)]
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
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;;   Module Manager Bootstrapping Commands

(with-system-name (:module-manager)
  (define-repl-command :startup (&rest args)
    "Load GBBopen startup.lisp file"
    (apply 'startup-gbbopen args))
  
    (define-repl-command (:module-manager-user :no-help) (&rest options)
    "Compile and load Module Manager Facility (MMF) User module"
    (startup-module :module-manager-user options)))

;;; ===========================================================================
;;;   Useful GBBopen Commands

(with-system-name (:gbbopen)

;;; ---------------------------------------------------------------------------
;;;  GBBopen Tools

  (define-repl-command :gbbopen-tools (&rest options)
    "Compile and load GBBopen Tools module"
    (startup-module :gbbopen-tools options :gbbopen-tools))

  (define-repl-command :portable-threads (&rest options)
    "Compile and load Portable Threads module"
    (startup-module :portable-threads options))

  (define-repl-command :portable-sockets (&rest options)
    "Compile and load Portable Sockets module"
    (startup-module :portable-sockets options))

;;; ---------------------------------------------------------------------------
;;;  GBBopen Core

  (define-repl-command :gbbopen-user (&rest options)
    "Compile and load GBBopen-User module"
    (startup-module :gbbopen-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Agenda Shell

  (define-repl-command :agenda-shell-user (&rest options)
    "Compile and load Agenda-Shell-User module"
    (startup-module :agenda-shell-user options :gbbopen-user))

;;; ---------------------------------------------------------------------------
;;;  Compile All GBBopen Modules

  (define-repl-command :compile-gbbopen (&rest options)
    "Compile all GBBopen modules and exit Common Lisp"
    (startup-module :compile-gbbopen 
                    (list* ':create-dirs ':noautorun options) :gbbopen-user))

  ;; end with-system-name
  )

;;; ---------------------------------------------------------------------------
;;;  Example Modules

(with-system-name (:gbbopen-examples)

  (define-repl-command :tutorial-example (&rest options)
    "Compile and load GBBopen Tutorial-Example module"
    (startup-module :tutorial-example options :tutorial))

  (define-repl-command :abort-ks-execution-example (&rest options)
    "Compile and load the Abort-KS-Execution Example module"
    (startup-module :abort-ks-execution-example options :gbbopen-user))

  ;; end with-system-name
  )

;;; ---------------------------------------------------------------------------
;;;  Test Modules

(with-system-name (:gbbopen-tests)
  
  (define-repl-command :gbbopen-test (&rest options)
    "Compile and load GBBopen Test module"
    (startup-module :gbbopen-test options  :gbbopen-user))

  (define-repl-command :timing-tests (&rest options)
    "Compile and load Timing Tests module"
    (startup-module :timing-tests options  :gbbopen-user))

  (define-repl-command :portable-threads-test (&rest options)
    "Compile and load Portable-Threads-Test module"
    (startup-module :portable-threads-test options :portable-threads-user))

  (define-repl-command :http-test (&rest options)
    "Compile and load Socket/HTTP-Test module"
    (startup-module :http-test options :portable-sockets-user))

  (define-repl-command :os-interface-test (&rest options)
    "Compile and load OS-Interface-Test module"
    (startup-module :os-interface-test options :os-interface-user))

  (define-repl-command :llrb-trees-test (&rest options)
    "Compile and load LLRB Trees Test module"
    (startup-module :llrb-trees-test options :gbbopen-tools))
  
  (define-repl-command :agenda-shell-test (&rest options)
    "Compile and load Agenda-Shell-Test module"
    (startup-module :agenda-shell-test options :gbbopen-user))

  ;; end with-system-name
  )
  
;;; ---------------------------------------------------------------------------
;;;  Extensions (not yet completed or documented)

(with-system-name (:gbbopen-extensions)

  (define-repl-command :multinode (&rest options)
    "Compile and load GBBopen multi-node support (under construction)"
    (startup-module :multinode options nil))

  #+not-yet
  (define-repl-command :restricted-eval (&rest options)
    "Compile and load GBBopen restricted-eval support (under construction)"
    (startup-module :restricted-eval options :gbbopen-tools))

  (define-repl-command :web-inspector (&rest options)
    "Compile and load Web inspector (under construction)"
    (startup-module :web-inspector options nil))

  ;; end with-system-name
  )

;;; ---------------------------------------------------------------------------
;;;  Timing (not yet completed or documented)

(with-system-name (:timing)

  (define-repl-command :cl-timing (&rest options)
    "Compile and load CL timing tests (under construction)"
    (startup-module :cl-timing options :gbbopen-user))

  ;; end with-system-name
  )

;;; ===========================================================================
;;;   Module Manager Commands
  
(with-system-name (:module-manager)
  
  (define-repl-command (:lm :add-to-native-help)
      (&rest module-name-and-options)
    "Load module"
    (startup-gbbopen)
    #+ignore
    (startup-module :module-manager (rest module-name-and-options) nil 't)
    (funcall 
     (fdefinition (intern (symbol-name '#:do-module-manager-repl-command) 
                          ':module-manager))
     ':lm module-name-and-options))
  
  (define-repl-command (:cm :add-to-native-help)
      (&rest module-name-and-options)
    "Compile and load module"
    (startup-gbbopen)
    #+ignore
    (startup-module :module-manager (rest module-name-and-options) nil 't)
    (funcall
     (fdefinition (intern (symbol-name '#:do-module-manager-repl-command) 
                          ':module-manager))
     ':cm module-name-and-options))
  
  ;; end with-system-name
  )

;;; ===========================================================================
;;;   Additional Useful Commands
  
(with-system-name (:built-in)

  (define-repl-command (:ds :add-to-native-help) (&optional obj)
    "Describe object"
    (describe (eval obj)))
  
;;; ---------------------------------------------------------------------------

  (define-repl-command (:quit :add-to-native-help
                             #+(or clisp 
                                   clozure
                                   cmu
                                   digitool-mcl
                                   ecl
                                   lispworks
                                   sbcl
                                   scl)
                             :no-cl-user-function)
      (&rest args)
    "Exit Lisp" 
    (apply #'extended-repl-quit-lisp args))
  
  ;; Allegro CL and ECL provide :exit commands already, but we still define
  ;; them here on all platforms for SLIME interface:
  (define-repl-command (:exit :add-to-native-help
                             #+(or allegro
                                   clisp)
                             :no-cl-user-function)
      (&rest args)
    "Exit Lisp" 
    (apply #'extended-repl-quit-lisp args))
  
;;; ---------------------------------------------------------------------------
  
  ;;  Allegro CL provides :pa, but we repeat for SLIME interface:
  (define-repl-command (:pa :add-to-native-help) (&optional package)
    "Set/show current package"
  (cond
   ;; Package change:
   (package (set-repl-package package))
   ;; Package show:
   (t (format t "~&;; The ~s package is current.~%" 
              (package-name *package*)))))
  
;;; ===========================================================================
;;;   Undefine system-name (commands, directories, & modules)
  
  (define-repl-command :undefine-system (&optional system-name)
    "Undefine a system (commands, directories, & modules)"
    (do-undefine-system-repl-command system-name))
  
;;; ===========================================================================
;;;   Help commands for all Extended REPL systems
  
  (define-repl-command (:systems :add-to-native-help) ()
    "Show all systems"
    (show-all-extended-repl-systems))
  
;;; ---------------------------------------------------------------------------
  
  (define-repl-command (:commands :add-to-native-help) (&optional system-name)
    "Show extended-REPL commands"
    (show-all-extended-repl-commands system-name))
  
;;; ---------------------------------------------------------------------------
;;;   Add :help command, where needed:
  
  #+(or clozure 
        sbcl)
  (define-repl-command :help ()
    "Show REPL commands"
    #+sbcl
    (show-all-extended-repl-commands)
    #+clozure
    (ccl::check-toplevel-command ':?))
  
;;; ---------------------------------------------------------------------------
;;;   Add :h abbreviated command, where needed:
  
  #+(or allegro
        sbcl)
  (define-repl-command (:h :no-help) ()
    #+allegro
    (top-level:do-command ':help)
    #+sbcl
    (show-all-extended-repl-commands))
  
;;; ---------------------------------------------------------------------------
;;;   Add :? abbreviated command, where needed:
  
  #+(or allegro
        sbcl)
  (define-repl-command (:? :no-help) ()
    #+allegro
    (top-level:do-command ':help)
    #+sbcl
    (show-all-extended-repl-commands))
  
    ;; end with-system-name
  )
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
