;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/initiate.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr 27 12:43:29 2008 *-*
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
;;;  12-08-02 File Created (originally named gbbopen-init.lisp).  (Corkill)
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
;;;  03-29-08 Add process-gbbopen-modules-directory rescanning.  (Corkill)
;;;  04-27-08 Added shared-gbbopen-modules directory support.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;; Add a single feature to identify sufficiently new Digitool MCL
;;; implementations (both Digitool MCL and pre-1.2 Clozure CL include the
;;; feature mcl):

#+(and digitool ccl-5.1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :digitool-mcl *features*))

;;; ---------------------------------------------------------------------------
;;; Add clozure feature to legacy OpenMCL:

#+(and openmcl (not clozure))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :clozure *features*))

;;; ---------------------------------------------------------------------------
;;;  Used to control gbbopen-modules directory processing by startup.lisp:

(defvar *skip-gbbopen-modules-directory-processing* nil)

;;; ---------------------------------------------------------------------------
;;;  Used to indicate if GBBopen's startup.lisp file has been loaded:

(defvar *gbbopen-startup-loaded* nil)

;;; ---------------------------------------------------------------------------

(defun compile-if-advantageous (fn-name)
  ;;; Compile bootstrap-loaded function or macro on CLs where this is desirable
  #+ecl (declare (ignore fn-name))
  #-ecl (compile fn-name))

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
;;;  GBBopen's startup.lisp loader
;;;    :force t -> load startup.lisp even if it has been loaded
;;;    :skip-gbbopen-modules-directory-processing -> controls whether 
;;;                 <homedir>/gbbopen-modules/ processing is performed

(let ((truename *load-truename*))
  (defun startup-gbbopen (&key                         
                          force
                          ((:skip-gbbopen-modules-directory-processing
                            *skip-gbbopen-modules-directory-processing*)
                           *skip-gbbopen-modules-directory-processing*))
    (cond 
     ;; Scan for changes if startup.lisp has been loaded:
     ((and (not force) *gbbopen-startup-loaded*)
      (unless *skip-gbbopen-modules-directory-processing*
        (funcall 'process-shared-gbbopen-modules-directory "commands" 't)
        (funcall 'process-gbbopen-modules-directory "commands" 't)
        (funcall 'process-shared-gbbopen-modules-directory "modules" 't)
        (funcall 'process-gbbopen-modules-directory "modules" 't)))
     ;; Load startup.lisp:
     (t (load (make-pathname 
               :name "startup"
               :type "lisp"
               :defaults truename))))))
  
;; CMUCL and Lispworks can't compile the interpreted closure:
#-(or cmu lispworks)
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
           (funcall (intern (symbol-name '#:set-slime-repl-package)
                            swank-package)
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

(defun startup-module (module-name options &optional package)
  (startup-gbbopen)
  (funcall (intern (symbol-name '#:do-mini-module-repl-command) :mini-module)
           ':cm
           (list* module-name ':propagate options))
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
;;;  Load gbbopen-modules-directory processing, if needed:

(let ((truename *load-truename*))
  (load (make-pathname 
         :name "gbbopen-modules-directory"
         :type "lisp"
         :defaults truename)))

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
;;;				  End of File
;;; ===========================================================================
