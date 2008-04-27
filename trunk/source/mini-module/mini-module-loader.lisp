;;;; -*- Mode:Common-Lisp; Package:MINI-MODULE; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/mini-module/mini-module-loader.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr 27 14:15:20 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Mini-Module System Bootstrap Loader
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill (incorporating some original ideas by 
;;;                          Kevin Gallagher and Zack Rubinstein)
;;;
;;; Copyright (C) 2002-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Stand-alone Use:
;;;
;;;    To use the Mini-Module System separate from the GBBopen Project
;;;    software tree, do the following:
;;;      1. Create a "root" directory to contain the mini-module software tree
;;;         (e.g., $ mkdir my-tree)
;;;      2. Create the mini-module portion of the source tree in that 
;;;         "root" directory  (e.g., $ cd my-tree; mkdir -p source/mini-module)
;;;      3. Copy the mini-module-loader.lisp, mini-module.lisp, and
;;;         mini-module-user.lisp files into the source/mini-module directory
;;;      4. Start your CL and then load the mini-module-loader.lisp file:
;;;          > (load "my-tree/source/mini-module/mini-module-loader")
;;;      5. Compile the :mini-module and :mini-module-user modules:
;;;          > (mini-module:compile-module :mini-module-user
;;;                                        :create-dirs :propagate)
;;;    The Mini-Module Facility can now be used stand-alone by loading
;;;    source/mini-module-loader.lisp as part of your Common Lisp
;;;    initialization.
;;;
;;; Porting Notice:
;;;
;;;    The defparameter forms (in this file) for:
;;;       *compiled-directory-name*
;;;       *compiled-file-type*
;;;    must be extended when porting to a new CL implementation.
;;;
;;;  This file defines the :mini-module package and the global variables
;;;  *compiled-directory-name* and *compiled-file-type*.  It then loads either
;;;  the source or compiled mini-module file (whichever is more recent).
;;;
;;;  This file is used as source only.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-11-04 Split from startup.lisp to support stand-alone use.  (Corkill)
;;;  05-22-05 Added ECL support.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  09-27-06 Added Intel Mac *compiled-directory-name* features (sometimes
;;;           best guesses). (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':mini-module)
    (make-package ':mini-module 
                  :use '(:common-lisp))))

(in-package :mini-module)

;;; ---------------------------------------------------------------------------

(defun must-port (entity &optional ask-for-features-p)
  (error "You must specify ~s for ~a running on ~a.~
          ~@[~%(Please send this error message and end the result of ~
                (pprint *features*) to bugs@gbbopen.org.)~]"
         entity
         (lisp-implementation-type) 
         (machine-type)
         ask-for-features-p))

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

;;; ===========================================================================
;;; The mini-module system uses a separate compiled directory tree for each CL
;;; implementation and version.  The following form creates a unique name for
;;; the root of this tree for a number of CL implementations.  If you use the
;;; mini-module system with another CL implementation, you should add that
;;; implementation to the *compiled-directory-name* form and e-mail the
;;; modified form to the GBBopen Project (bugs@GBBopen.org) for inclusion in
;;; future releases.

(defparameter *compiled-directory-name*
    (or
     ;; Franz Allegro:
     #+allegro
     (format nil "~a-allegro~:[~;-m~]-~a"
             (or
              #+alpha "alpha"
              #+prism "prism"
              #+sgi "sgi"
              #+sparc "sparc"
              #+rs6000 "rs6000"
              #+(and x86 linux86) "linux86"
              #+(and x86-64 linux) "linux86-64" ; Thanks to Raymond de Lacaze
              #+(and x86 macosx) "macosx86"
              #+(and x86 (not linux86)) "windows"
	      #+powerpc "darwin"
              #-(or alpha prism sgi sparc rs6000 powerpc
                    (and x86 linux86)
                    (and x86-64 linux) 
                    (and x86 macosx)
                    (and x86 (not linux86)))
              (must-port '*compiled-directory-name* 't))
	     (eq excl:*current-case-mode* ':case-sensitive-lower) 
             excl::*common-lisp-version-number*)
     ;; CLISP:
     #+clisp
     (format nil "clisp-~a"
             (let ((version (lisp-implementation-version)))
               (subseq version 0 (position #\Space version))))
     ;; CMUCL:
     #+cmu
     (format nil "~a-cmucl-~a" 
             (or 
              #+(and (not x86) darwin) "darwin"
              #+(and x86 darwin) "macosx86"
              #+sparc "sparc"
              #+(and x86 linux) "linux86"
              #+(and x86 (not linux)) "windows"
              #-(or darwin sparc x86)
              (must-port '*compiled-directory-name* 't))
             (let ((version (lisp-implementation-version)))
               (subseq version 0 (position #\Space version))))	       
     ;; Clozure Common Lisp:
     #+clozure
     (format nil "~a-clozure-~a.~a"
             (or
              #+darwinppc-target "darwin"
              #+darwinx8664-target "macosx86-64"
              #+linuxx8664-target "linux86-64" ; Thanks to Matthew Danish
              #-(or darwin darwinx8664-target linuxx8664-target) 
              (must-port '*compiled-directory-name* 't))
             ccl::*openmcl-major-version*
             ccl::*openmcl-minor-version*)
     ;; Corman Common Lisp:
     #+cormanlisp
     (format nil "windows-corman-~a"
	     (lisp-implementation-version))
     ;; Digitool MCL:
     #+digitool-mcl
     (format nil "~a-mcl-~a"
             (or #+powerpc "darwin" 
                 #-powerpc
                 (must-port '*compiled-directory-name* 't))
             (ccl::lisp-implementation-short-version))
     ;; ECL (Embedable Common Lisp):
     #+ecl
     (format nil "~a-ecl-~a"
             (or #+(and (or pentium3 pentium4) linux) "linux86" 
                 #+(and (or pentium3 pentium4) darwin) "macosx86" 
                 #+(and (or pentium3 pentium4) (not (or linux darwin))) "windows"
                 #+(and (not (or pentium3 pentium4)) darwin) "darwin"
		 #-(or pentium3 pentium4)
                 (must-port '*compiled-directory-name* 't))
             ;; Strip away any CVS info:
             (let ((full-version (lisp-implementation-version)))
               (subseq full-version 0 (position '#\space full-version))))
     ;; GCL:
     #+gcl
     (format nil "~a-gcl-~s.~s"
             (or #+linux "linux86" 
		 #+sparc "sparc"
		 #-(or linux sparc)
                 (must-port '*compiled-directory-name* 't))
	     system::*gcl-major-version*
	     system::*gcl-minor-version*)
     ;; Lispworks:
     #+lispworks
     (format nil "~a-lispworks-~s.~s" 
             (or 
              #+alpha "alpha"
              #+prism "prism"
              #+sparc "sparc"
              #+(and iapx386 linux) "linux86"
              #+(and iapx386 (not (or linux darwin))) "windows"
	      #+(and (not iapx386) darwin) "darwin"
              #+(and iapx386 darwin) "macosx86"
              #-(or alpha darwin prism sparc iapx386)
              (must-port '*compiled-directory-name* 't))
             system::*major-version-number*
             system::*minor-version-number*)
     ;; SBCL:
     #+sbcl
     (format nil "~a-sbcl-~a" 
             (or 
              #+(and (not x86) darwin) "darwin"
              #+(and x86 darwin) "macosx86"
              #+sparc "sparc"
              #+(and x86 linux) "linux86"
              #+(and x86-64 linux) "linux86-64" ; Thanks to Eric Menard
              #+(and x86 (not linux)) "windows"
              #-(or darwin sparc x86 (and x86-64 linux))
              (must-port '*compiled-directory-name* 't))
             (lisp-implementation-version))
     ;; The Scieneer CL:
     #+scl
     (format nil "~(~a~)-scl-~a-~(~a~)"
	     (c:backend-name c:*backend*)
	     #+linux "linux"
	     #+solaris "solaris"
	     #+hpux "hpux"
	     ext:*case-mode*
             (lisp-implementation-version))
     ;; Unknown CL:
     #-(or allegro 
           clisp 
           clozure
           cmu
           cormanlisp 
           digitool-mcl
           ecl
           lispworks
	   sbcl 
           scl)
     (must-port '*compiled-directory-name* 't)))

;;; ===========================================================================
;;;  Compiled File Type
;;;
;;; The mini-module system needs to know the file type of compiled files.  The
;;; following form specifies the compiled-file type for a number of CL
;;; implementations. If you use the mini-module system with another CL
;;; implementation, you should add that implementation to the
;;; *compiled-file-type* form and e-mail the modified form to the GBBopen
;;; Project (bugs@GBBopen.org) for inclusion in future releases.

(defparameter *compiled-file-type*
    (or
     ;; Franz Allegro:
     #+allegro
     excl:*fasl-default-type* 
     ;; CLISP:
     #+clisp
     (car custom:*compiled-file-types*)
     ;; Clozure Common Lisp:
     #+clozure
     (pathname-type ccl:*.fasl-pathname*)
     ;; CMUCL:
     #+cmu
     (c:backend-fasl-file-type c:*backend*)
     ;; Corman Common Lisp:
     #+cormanlisp
     "fasl"
     ;; Digitool MCL:
     #+digitool-mcl
     (pathname-type ccl:*.fasl-pathname*)
     ;; ECL (Embedable Common Lisp):
     #+ecl
     "fas"
     ;; GCL
     #+gcl
     "o"
     ;; Lispworks:
     #+lispworks
     compiler:*fasl-extension-string*
     ;; SBCL:
     #+sbcl
     sb-fasl:*fasl-file-type*
     ;; The Scieneer CL:
     #+scl
     (c:backend-fasl-file-type c:*backend*)
     ;; Unknown CL:
     #-(or allegro 
           clisp
           clozure
           cmu
           cormanlisp
           digitool-mcl
           ecl
           lispworks
	   sbcl
           scl)
     (must-port '*compiled-file-type*)))

;;; ===========================================================================
;;;  Load the mini-module system (source or compiled file)

(let* ((this-file-truename *load-truename*)
       (root-pathname
        ;; CLISP, CormanLisp, and ECL don't handle :unspecific (support is not
        ;; required by the ANSI standard, but it does provide desirable
        ;; "filled" pathname merging behavior)
        (make-pathname
         :name #-(or clisp cormanlisp ecl) :unspecific 
               #+(or clisp cormanlisp ecl) nil
         :type #-(or clisp cormanlisp ecl) :unspecific 
               #+(or clisp cormanlisp ecl) nil
         :version :newest
         :directory (butlast (pathname-directory this-file-truename) 2)
         :defaults this-file-truename)))
  (flet ((load-source-or-compiled-file (name)
           (let* ((source-path 
                   (make-pathname
                    :name name
                    :type "lisp"
                    :directory `(,@(pathname-directory root-pathname)
                                   "source" "mini-module")
                    :version :newest
                    :defaults root-pathname))
                  (compiled-path
                   (make-pathname
                    :type *compiled-file-type*
                    :directory `(,@(pathname-directory root-pathname)
                                   ,*compiled-directory-name* 
                                   "mini-module")
                    :defaults source-path))
                  (source-file-date 
                   (or (file-write-date source-path) 0))
                  (compiled-file-date 
                   (or (and (probe-file compiled-path)
                            (file-write-date compiled-path))
                       0)))
             ;; Load the compiled file unless the source file is newer:
             (load (if (> compiled-file-date source-file-date)
                       compiled-path
                       source-path)))))
    (load-source-or-compiled-file "mini-module")))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


