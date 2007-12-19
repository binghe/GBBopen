;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/gbbopen/startup.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Dec 19 04:37:44 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         GBBopen Startup File
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;;  For GBBopen porting information, refer to the file PORTING.
;;;
;;;  This file is used as source only.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-08-02 File Created.  (Corkill)
;;;  04-14-04 Added MCL feature hacking to keep things simpler 
;;;           throughout.  (Corkill)
;;;  06-09-04 Added <home-dir>gbbopen-init.lisp file loading. (Corkill)
;;;  06-10-04 Moved the :gbbopen package definition here (from
;;;           gbbopen/preamble.lisp) to facilitate setting GBBopen parameters
;;;           in a user's <home-dir>gbbopen-init.lisp file.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  09-28-05 Added *preferred-browser*.  (Corkill)
;;;  03-11-06 Added *mini-module-compile-verbose* and 
;;;           *mini-module-load-verbose*.  (Corkill)
;;;  04-07-06 Added gbbopen-modules directory support.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ===========================================================================
;;;  Preferred browser setting (defaults) 

(defvar *preferred-browser* 
    ;; On Mac OSX we defer to the OS default browser:
    #+(or macosx darwin)
    "open"
    ;; Lispworks (non-Windows) and SBCL do not search PATH for programs, so
    ;; the path must be explicitly included in the preferred browser setting:
    #+(or (and lispworks (not win32)) sbcl) "/usr/bin/firefox"
    #-(or macosx darwin (and lispworks (not win32)) sbcl) "firefox")

;;; ---------------------------------------------------------------------------
;;;  Load required Corman Common Lisp patches

#+cormanlisp
(load (make-pathname :name "corman-patches"
                     :type "lisp" 
                     :defaults *load-truename*))

;;; ---------------------------------------------------------------------------
;;;  Bootstrap-load the mini-module system from its location relative to this
;;;  file.

(let ((startup-file-truename *load-truename*))
  (load (make-pathname
         :name "mini-module-loader"
         :type "lisp"
         :directory `(,@(pathname-directory startup-file-truename)
                        "source" "mini-module")
         :version :newest
         :defaults startup-file-truename)))

;;; ---------------------------------------------------------------------------
;;;  Define and load the remaining GBBopen module definitions from
;;;  modules.lisp:

(mini-module:define-module :gbbopen-modules
  (:requires :mini-module)
  (:directory nil :up)
  (:files ("modules" :source)))

(mini-module:load-module :gbbopen-modules)

;;; ---------------------------------------------------------------------------
;;;  Define the :gbbopen-tools and :gbbopen packages here, to allow a user to
;;;  set GBBopen parameters in their <user-homedir>gbbopen-init.lisp file
;;;  (loaded below):

(unless (find-package :gbbopen-tools)
  (defpackage :gbbopen-tools 
    (:use :common-lisp :mini-module)))

(unless (find-package :gbbopen)
  (defpackage :gbbopen 
    (:use :common-lisp :mini-module :gbbopen-tools)))

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-init.lisp file (or compiled version) in the
;;;  users "home" directory, load it now:

(load (namestring (make-pathname :name "gbbopen-init"
				 :type #-(or clisp ecl) ':unspecific 
				       #+(or clisp ecl) nil
				 :version ':newest
				 :defaults (user-homedir-pathname)))
      :if-does-not-exist nil)

;;; ===========================================================================
;;;  Load gbbopen-modules-directory processing, if needed:

(unless (fboundp 'process-gbbopen-modules-directory)
  (let ((truename *load-truename*))
    (load (make-pathname 
	   :name "gbbopen-modules-directory"
	   :type "lisp"
	   :defaults truename))))

;;; ---------------------------------------------------------------------------
;;;  If there is a gbbopen-modules directory in the users "home" directory,
;;;  load the modules.lisp file (if present) from each module directory
;;;  that is linked from the gbbopen-modules directory:

(process-gbbopen-modules-directory "modules")

;;; ---------------------------------------------------------------------------
;;;  Record that GBBopen's startup file has been loaded:

(declaim (special *gbbopen-startup-loaded*))
(setq *gbbopen-startup-loaded* 't)

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
