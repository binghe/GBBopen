;;;; -*- Mode:Common-Lisp; Package:MINI-MODULE; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/modules.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jan 15 11:58:34 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      GBBopen Module Definitions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-17-02 File Created. (Corkill)
;;;  03-07-04 Added :queue module. (Corkill)
;;;  03-21-04 Added :agenda-shell-test module. (Corkill)
;;;  06-17-05 Added :os-interface module. (Corkill)
;;;  08-03-05 Added :sockets module. (Corkill)
;;;  08-20-05 Added :agenda-shell-user module. (Corkill)
;;;  08-21-05 Added :multiprocessing-test module. (Corkill)
;;;  10-08-05 Added :tutorial-example module. (Corkill)
;;;  01-05-06 Changed :sockets module to :portable-sockets to be consistent
;;;           with the :portable-threads module renaming.  (Corkill)
;;;  03-31-06 Added :multinode module. (Corkill)
;;;  04-06-06 Added gbbopen-instance class. (Corkill)
;;;  11-13-06 Added :abort-ks-execution-example module. (Corkill)
;;;  06-28-07 Renamed :gbbopen module to more accurate :gbbopen-core. (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :mini-module)

;;; ---------------------------------------------------------------------------
;;;  Define the root GBBopen installation directory relative to this file:

(let ((this-file-truename *load-truename*))
  (define-root-directory :gbbopen-root this-file-truename))

;;; ===========================================================================
;;;  GBBopen Tools Modules

(define-relative-directory :gbbopen-tools :gbbopen-root "tools")

(define-module :gbbopen-tools
  (:requires :mini-module) 		; required only for brief-date-and-time
  (:directory :gbbopen-tools)
  (:files "preamble"
          ("declarations" :forces-recompile)
          ("declared-numerics" :forces-recompile)
          ("defflags" :forces-recompile)
          ("tools" :forces-recompile)
          ("define-class" :forces-recompile)
          #+ecl
          ("ecl-mop-patches" :forces-recompile)
          "mop-interface"
	  "gbbopen-instance"
          "offset-universal-time"
          "print-object-for"
          "read-object"
          "epilogue"))

(define-module :portable-threads
  (:requires :mini-module)		; not really required, but we want 
                                        ; :mini-module compiled/loaded first
  (:directory :gbbopen-tools)
  (:files ("portable-threads" :forces-recompile)))

(define-module :polling-functions
  (:requires :portable-threads :gbbopen-tools)
  (:directory :gbbopen-tools)
  (:files "polling-functions"))

(define-module :os-interface
  (:requires :gbbopen-tools)
  (:directory :gbbopen-tools)
  (:files ("os-interface")))

(define-module :portable-sockets
  (:requires :portable-threads)
  (:directory :gbbopen-tools)
  (:files ("portable-sockets")))

(define-module :queue
  (:requires :portable-threads :gbbopen-tools)
  (:directory :gbbopen-tools)
  (:files ("queue" :forces-recompile)))

;;; ===========================================================================
;;;  GBBopen Core Modules

(define-relative-directory :gbbopen :gbbopen-root "gbbopen")

(define-module :gbbopen-core
  (:requires :portable-threads :gbbopen-tools)
  (:directory :gbbopen)
  (:files "preamble"
          "utilities"                   
          ("unit-metaclasses" :forces-recompile)
          ("units" :forces-recompile)
          ("event-metaclasses" :forces-recompile)
          ("events" :forces-recompile)
          "system-events"
          ("links" :forces-recompile)
          ("instances" :forces-recompile)
          "spaces"
          "storage"
	  "unstructured-storage"
	  "boolean-storage"
	  "hashed-storage"
	  "1d-uniform-storage"
	  "2d-uniform-storage"
          ("find" :forces-recompile)
          "epilogue"))

(define-module :gbbopen-user
  (:requires :gbbopen-core :os-interface)
  (:directory :gbbopen)
  (:files "gbbopen-user"))

;;; ===========================================================================
;;;  Agenda Shell Modules

(define-module :agenda-shell
  (:requires :gbbopen-core :queue :polling-functions)
  (:directory :gbbopen "control-shells")
  (:files ("agenda-shell-metaclasses" :forces-recompile)
	  ("agenda-shell-metering" :forces-recompile)
	  "agenda-shell"))

(define-module :agenda-shell-user
  (:requires :agenda-shell :gbbopen-user)
  (:directory :gbbopen "control-shells")
  (:files "agenda-shell-user"))

;;; ===========================================================================
;;;  Extensions

(define-module :multinode 
  (:requires :gbbopen-core)
  (:directory :gbbopen "extensions")
  (:files "multinode"))

(define-module :web-inspector 
  (:requires :portable-sockets :gbbopen-core)
  (:directory :gbbopen "extensions")
  (:files "web-inspector"))

;;; ===========================================================================
;;;  Test Modules

(define-module :test-harness
  (:requires :gbbopen-tools)
  (:directory :gbbopen-tools "test")
  (:files ("test-harness")))

;;; ---------------------------------------------------------------------------

(define-module :gbbopen-test
  (:requires :gbbopen-user)
  (:directory :gbbopen "test")
  (:files ("basic-tests" :reload)))

;;; ---------------------------------------------------------------------------

(define-module :timing-tests
  (:requires :gbbopen-user)
  (:directory :gbbopen "test")
  (:files ("timing-tests-metaclasses" :forces-recompile)
          ("timing-tests" :reload)))

;;; ---------------------------------------------------------------------------

(define-module :agenda-shell-test
  (:requires :agenda-shell-user)
  (:directory :gbbopen "control-shells" "test")
  (:files ("agenda-shell-test" :reload)))

;;; ---------------------------------------------------------------------------

(define-module :http-test
  (:requires :portable-sockets)
  (:directory :gbbopen-tools "test")
  (:files ("http-test" :reload)))

;;; ---------------------------------------------------------------------------

(define-module :portable-threads-test
  (:requires :portable-threads)
  (:directory :gbbopen-tools "test")
  (:files ("portable-threads-test" :reload)))

;;; ===========================================================================
;;;  Example Modules

(define-module :tutorial-example
  (:requires :agenda-shell-user)
  (:directory :gbbopen "examples")
  (:files "tutorial"))

(define-module :abort-ks-execution-example
  (:requires :agenda-shell-user)
  (:directory :gbbopen "control-shells" "examples")
  (:files "abort-ks-execution"))

;;; ===========================================================================
;;;  Compile All of GBBopen

(define-module :compile-gbbopen
  (:requires :mini-module)
  (:directory :gbbopen-root)
  (:files ("compile-all" :source))
  ;; The following undocumented capability is used to cleanly exit Common Lisp
  ;; after the module has been compiled.  (Forms in the module's :files are
  ;; preferable to using an :after-form.  However, quitting CL during module
  ;; compiling/loading aborts the compilation-unit of the module, so the
  ;; :after-form allows a clean exit.)
  (:after-form (progn 
                 (format t "~2&;;; Exiting Common Lisp...~2%")
                 (finish-output)
                 (cl-user::quit-lisp))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
