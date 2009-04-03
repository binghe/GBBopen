;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/initiate.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Apr  3 14:36:07 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Useful (Standard) GBBopen Initiations
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Useful generic GBBopen initialization definitions.  Load this file from
;;; your personal CL initialization file (in place of startup.lisp).  After
;;; loading, handy top-level-loop keyword commands, such as :gbbopen-tools,
;;; :gbbopen, :gbbopen-user, and :gbbopen-test, are available on Allegro CL,
;;; CLISP, Clozure CL, CMUCL, SCL, ECL, Lispworks, and SBCL.  GBBopen keyword
;;; commands are also supported in the SLIME REPL.
;;;
;;; For example:
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
;;;  12-08-02 File created (originally named gbbopen-init.lisp).  (Corkill)
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
;;;  03-25-08 Renamed to more intuitive initiate.lisp.  (Corkill)
;;;  03-29-08 Add PROCESS-GBBOPEN-MODULES-DIRECTORY rescanning.  (Corkill)
;;;  04-27-08 Added shared-gbbopen-modules directory support.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;; Add a single feature to identify sufficiently new Digitool MCL
;;; implementations (both Digitool MCL and pre-1.2 Clozure CL include the
;;; feature mcl):

#+(and digitool ccl-5.1)
(pushnew :digitool-mcl *features*)

;;; ---------------------------------------------------------------------------
;;; Add clozure feature to legacy OpenMCL:

#+(and openmcl (not clozure))
(pushnew :clozure *features*)

;;; ---------------------------------------------------------------------------
;;;  Used to control gbbopen-modules directory processing by startup.lisp:

(defvar *skip-gbbopen-modules-directory-processing* nil)

;;; ---------------------------------------------------------------------------
;;;  Used to indicate if this file and if GBBopen's startup.lisp file have been
;;;  loaded:

(defvar *gbbopen-initiate-loaded* nil)
(defvar *gbbopen-startup-loaded* nil)

;;; ---------------------------------------------------------------------------
;;;  The GBBopen install directory:

(defvar *gbbopen-install-root* nil)

;; This file establishes the GBBopen install directory:
(setf *gbbopen-install-root* 
      (make-pathname :name nil
                     :type nil
                     :defaults *load-truename*))

;;; ---------------------------------------------------------------------------
;;;  Compile bootstrap-loaded function or macro on CLs where this is
;;;  desirable:
;;;
;;;  NOTE: Copy all changes to COMPILE-IF-ADVANTAGEOUS to startup.lisp:

(defun compile-if-advantageous (fn-name)
  ;;;  CMUCL, Lispworks, and SBCL running in :interpret *evaluator-mode* can't
  ;;;  compile interpreted closures (so we avoid having `fn-name' any
  ;;;  definitions that are closures)
  ;;;
  ;;;  CMUCL and SCL can't compile macro definitions (so we skip compiling
  ;;;  them)
  ;;; 
  ;;;  ECL's compiler is slow and creates temporary files, so we don't bother
  #+ecl (declare (ignore fn-name))
  #-ecl (unless (or #+(or cmu scl) (macro-function fn-name))
          (compile fn-name)))

;;; ---------------------------------------------------------------------------
;;; Load top-level REPL extensions for CLISP, CMUCL, SCL, ECL, and SBCL and
;;; SLIME command extensions for all:

(load (make-pathname 
       :name "extended-repl"
       :type "lisp"
       :defaults *gbbopen-install-root*))

;;; ---------------------------------------------------------------------------
;;;  GBBopen's startup.lisp loader
;;;    :force t -> load startup.lisp even if it has been loaded
;;;    :skip-gbbopen-modules-directory-processing -> controls whether 
;;;                 <homedir>/gbbopen-modules/ processing is performed

(defun startup-gbbopen (&key                         
                        force
                        ((:skip-gbbopen-modules-directory-processing
                          *skip-gbbopen-modules-directory-processing*)
                         *skip-gbbopen-modules-directory-processing*))
  (cond 
   ;; Scan for changes if startup.lisp has been loaded:
   ((and (not force) *gbbopen-startup-loaded*)
    (unless *skip-gbbopen-modules-directory-processing*
      ;; Use funcall in order to avoid forward-referenced "undefined
      ;; function" warnings:
      (funcall 'process-shared-gbbopen-modules-directory "commands")
      (funcall 'process-gbbopen-modules-directory "commands")
      (funcall 'process-shared-gbbopen-modules-directory "modules")
      (funcall 'process-gbbopen-modules-directory "modules")))
   ;; Load startup.lisp:
   (t (load (make-pathname 
             :name "startup"
             :type "lisp"
             :defaults *gbbopen-install-root*)))))
  
(compile-if-advantageous 'startup-gbbopen)

;;; ---------------------------------------------------------------------------

(defun set-repl-package (package-specifier)
  ;; Sets *package* as well as updating LEP and SLIME Emacs interfaces:
  (let ((swank-package (find-package ':swank))
        (requested-package (find-package package-specifier)))
    (cond
     ;; No such package:
     ((not requested-package)
      (format t "~&;; The package ~s is not defined.~%" 
              package-specifier))
     ;; If we are SLIME'ing:
     ((and swank-package 
           ;; Returns true if successful:
           (funcall 
            (fdefinition (intern (symbol-name '#:set-slime-repl-package)
                                 swank-package))
            (package-name requested-package)))
      ;; in SLIME, we're done:
      't)
     ;; Not SLIME:
     (t (setf *package* requested-package)
        ;; For LEP Emacs interface (Allegro & SCL):
        #+(or allegro scl)
        (when (lep:lep-is-running)
          (lep::eval-in-emacs 
           (format nil "(setq fi:package ~s)"
                   (package-name requested-package))))))))

(compile-if-advantageous 'set-repl-package)

;;; ---------------------------------------------------------------------------

(defun startup-module (module-name options &optional package 
                                                     ;; not documented
                                                     dont-remember)
  (startup-gbbopen)
  (funcall 
   (fdefinition (intern (symbol-name '#:do-module-manager-repl-command)
                        ':module-manager))
   ':cm
   (list* module-name ':propagate options)
   dont-remember)
  (when package 
    (set-repl-package package)
    (import '(common-lisp-user::gbbopen-tools 
              common-lisp-user::gbbopen
              common-lisp-user::gbbopen-user 
              common-lisp-user::gbbopen-test)
            *package*))
  (values))

(compile-if-advantageous 'startup-module)

;;; ===========================================================================
;;;  Load GBBopen's standard REPL commands

(load (make-pathname 
       :name "commands"
       :type "lisp"
       :defaults *gbbopen-install-root*))

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-commands.lisp file (or compiled version) in the
;;;  users "home" directory, load it now. 

(load (namestring 
       (make-pathname :name "gbbopen-commands"
                      ;; CLISP, Cormanlisp, and ECL don't handle :unspecific
                      ;; file types:
                      :type #-(or clisp cormanlisp) ':unspecific 
                            #+(or clisp cormanlisp) nil
                      :version ':newest
                      :defaults (user-homedir-pathname)))
      :if-does-not-exist nil)

;;; ===========================================================================
;;;  Load gbbopen-modules-directory processing, if needed:

(load (make-pathname 
       :name "gbbopen-modules-directory"
       :type "lisp"
       :defaults *gbbopen-install-root*))

;;; ---------------------------------------------------------------------------
;;;  Load the commands.lisp file (if present) from each module directory
;;;  that is linked from GBBopen's shared-gbbopen-modules directory:

(process-shared-gbbopen-modules-directory "commands")

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-modules directory in the users "home" directory,
;;;  load the commands.lisp file (if present) from each module directory
;;;  that is linked from the gbbopen-modules directory. 

(process-gbbopen-modules-directory "commands")

;;; ===========================================================================
;;;  Record that this GBBopen initiate file has been loaded:

(setf *gbbopen-initiate-loaded* 't)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
