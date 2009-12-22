;;;; -*- Mode:Common-Lisp; Package:MODULE-MANAGER-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/modules.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Dec 19 07:53:57 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

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
;;; Copyright (C) 2002-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-17-02 File created. (Corkill)
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

(in-package :module-manager-user)

(with-system-name (:gbbopen)

;;; ---------------------------------------------------------------------------
;;;  Define the root GBBopen installation directory relative to this file:

  (let ((this-file-truename *load-truename*))
    (define-root-directory :gbbopen-root this-file-truename))

;;; ===========================================================================
;;;  GBBopen Tools Modules

  (define-relative-directory :gbbopen-tools :gbbopen-root "tools")
  
  (define-module :gbbopen-tools
    (:requires :module-manager-user)    ; required only for brief-date-and-time
    (:directory :gbbopen-tools)
    (:files "preamble"
            ("declarations" :forces-recompile)
            ("declared-numerics" :forces-recompile)
            ("defflags" :forces-recompile)
            "clos-interface" 
            ("tools" :forces-recompile)
            ("pseudo-probabilities" :forces-recompile)
            ("define-class" :forces-recompile)
            "gbbopen-instance"
            "date-and-time"
            ("offset-universal-time" :forces-recompile)
            ("print-object-for" :forces-recompile)
            ("read-object" :forces-recompile)
            "duplicate-instance"
            ("llrb-tree" :forces-recompile)
            "epilogue"))
  
  (define-module :portable-threads
    (:requires :module-manager-user)       ; not really required, but we want
                                           ; :module-manager-user
                                           ; compiled/loaded if we are using
                                           ; the Module Manager Facility
    (:directory :gbbopen-tools)
    (:files ("portable-threads" :forces-recompile)
            "scheduled-periodic-functions"))
  
  (define-module :polling-functions
    (:requires :gbbopen-tools :portable-threads)
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
  
  (define-module :http-services
    (:requires  :gbbopen-tools :portable-threads :portable-sockets)
    (:directory :gbbopen-tools)
    (:files ("http-services")))
  
  (define-module :queue
    (:requires :polling-functions :gbbopen-core)
    (:directory :gbbopen)
    (:files ("queue" :forces-recompile)))
  
  #+not-yet
  (define-module :restricted-eval
    (:requires :gbbopen-tools)
    (:directory :gbbopen-tools)
    (:files ("restricted-eval")))
  
;;; ===========================================================================
;;;  GBBopen Core Modules   (Keep gbbopen.asd consistent with these!)
  
  (define-relative-directory :gbbopen :gbbopen-root "gbbopen")
  
  (define-module :gbbopen-core
    (:requires :gbbopen-tools :portable-threads :os-interface)
    (:directory :gbbopen)
    (:files "preamble"
            ("utilities" :forces-recompile)
            ("unit-metaclasses" :forces-recompile 
                                ;; Clozure CL can't recompile these
                                ;; metaclasses in the same image (causes
                                ;; downstream failure compiling
                                ;; "events.lisp"):
                                #+clozure :skip-recompile)
            ("units" :forces-recompile)
            ("event-metaclasses" :forces-recompile)
            ("events" :forces-recompile)
            "system-events"
            ("links" :forces-recompile)
            ("instances" :forces-recompile)
            "spaces"
            "storage"
            #+developing
            "new-storage"
            ("find" :forces-recompile)
            "unstructured-storage"
            "boolean-storage"
            "hashed-storage"
            "1d-uniform-storage"
            "2d-uniform-storage"
            "epilogue"))
  
  (define-module :gbbopen-user
    (:requires :gbbopen-core)
    (:directory :gbbopen)
    (:files "gbbopen-user"))

;;; ===========================================================================
;;;  Agenda Shell Modules

  (define-module :agenda-shell
    (:requires :queue)
    (:directory :gbbopen "control-shells")
    (:files ("agenda-shell-metaclasses" :forces-recompile)
            ("agenda-shell-metering" :forces-recompile)
            "agenda-shell"))
  
  (define-module :agenda-shell-user
    (:requires :agenda-shell :gbbopen-user)
    (:directory :gbbopen "control-shells")
    (:files "agenda-shell-user"))

;;; ===========================================================================
;;;  Compile All of GBBopen

  (define-module :compile-gbbopen
    (:requires :module-manager)
    (:directory :gbbopen-root)
    (:files ("compile-all" :source))
    ;; The following undocumented capability is used to cleanly exit Common
    ;; Lisp after the module has been compiled.  (Forms in the module's :files
    ;; are preferable to using an :after-form.  However, quitting CL during
    ;; module compiling/loading aborts the compilation-unit of the module, so
    ;; the :after-form allows a clean exit.)
    (:after-form (progn 
                   (format t "~2&;;; Exiting Common Lisp...~2%")
                   (finish-output)
                   (cl-user::extended-repl-quit-lisp))))

  ;; end of with-system-name
  )
  
;;; ===========================================================================
;;;  Test Modules

(with-system-name (:gbbopen-tests)

  (define-module :test-harness
    (:requires :gbbopen-tools :portable-threads)
    (:directory :gbbopen-tools "test")
    (:files ("test-harness")))
  
;;; ---------------------------------------------------------------------------

  (define-module :gbbopen-test
    (:requires :gbbopen-user)
    (:directory :gbbopen "test")
    (:files ("basic-tests" :reload))
    (:patches "basic-tests-p001"
              "basic-tests-p002"))
  
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

  (define-module :portable-sockets-test
    (:requires :portable-sockets)
    (:directory :gbbopen-tools "test")
    (:files ("portable-sockets-test" :reload)))
  
;;; ---------------------------------------------------------------------------

  (define-module :os-interface-test
    (:requires :os-interface)
    (:directory :gbbopen-tools "test")
    (:files ("os-interface-test" :reload)))
  
;;; ---------------------------------------------------------------------------

  (define-module :portable-threads-test
    (:requires :portable-threads)
    (:directory :gbbopen-tools "test")
    (:files ("portable-threads-test" :reload)))
  
;;; ---------------------------------------------------------------------------

  (define-module :gbbopen-tools-test
    (:requires :gbbopen-tools)
    (:directory :gbbopen-tools "test")
    (:files ("llrb-tree-test" :reload :noautorun)
            "gbbopen-tools-test"))
  
  ;; end of with-system-name
  )

;;; ===========================================================================
;;;  Example Modules

(with-system-name (:gbbopen-examples)
  
  (define-module :tutorial-example
    (:requires :agenda-shell)
    (:directory :gbbopen "examples")
    (:files "tutorial"))
  
  (define-module :abort-ks-execution-example
    (:requires :agenda-shell-user)
    (:directory :gbbopen "control-shells" "examples")
    (:files "abort-ks-execution"))
  
  ;; end of with-system-name
  )

;;; ===========================================================================
;;;  Extensions
  
(with-system-name (:gbbopen-extensions)

  (define-module :multinode 
    (:requires :gbbopen-core)
    (:directory :gbbopen "extensions")
    (:files "multinode"))
  
  (define-module :web-inspector 
    (:requires :http-services)
    (:directory :gbbopen "extensions")
    (:files "web-inspector"))
  
  ;; end of with-system-name
  )

;;; ===========================================================================
;;;  Timing
  
(with-system-name (:timing)

  (define-module :cl-timing
    (:requires :gbbopen-user)
    (:directory :gbbopen-tools "timing")
    (:files "cl-timing"))
  
  ;; end of with-system-name
  )

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
