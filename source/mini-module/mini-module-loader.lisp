;;;; -*- Mode:Common-Lisp; Package:MINI-MODULE; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/mini-module/mini-module-loader.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Oct 10 05:01:28 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

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
;;; Copyright (C) 2002-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
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

(unless (find-package :mini-module)
  (defpackage :mini-module (:use :common-lisp)))

(in-package :mini-module)

;;; ---------------------------------------------------------------------------

(defun must-port (entity)
  (error "You must specify ~s for ~a running on ~a."
         entity
         (lisp-implementation-type) 
         (machine-type)))

;;; ===========================================================================
;;; Add a single feature to identify sufficiently new Digitool MCL
;;; implementations (at present, both Digitool MCL and OpenMCL include
;;; the feature mcl):

#+(and digitool ccl-5.1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :digitool-mcl *features*))

;;; ===========================================================================
;;;  Source/Compiled Directory Names

(defparameter *source-directory-name* "source")

;;; ---------------------------------------------------------------------------
;;; The mini-module system uses a separate compiled directory tree for each CL
;;; implementation and version.  The following form creates a unique name for
;;; the root of this tree for a number of CL implementations.  If you use the
;;; mini-module system with another CL implementation, you should add that
;;; implementation to the *compiled-directory-name* form and e-mail the
;;; modified form to the GBBopen Project for inclusion in future releases.

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
              #+(and x86 macosx) "macosx86"
              #+(and x86 (not linux86)) "windows"
	      #+powerpc "darwin"
              #-(or alpha prism sgi sparc rs6000 x86 powerpc)
              (must-port '*compiled-directory-name*))
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
              (port-need '*compiled-directory-name*))
             (let ((version (lisp-implementation-version)))
               (subseq version 0 (position #\Space version))))	       
     ;; Corman Common Lisp:
     #+cormanlisp
     (format nil "windows-corman-~a"
	     (lisp-implementation-version))
     ;; Digitool MCL:
     #+digitool-mcl
     (format nil "~a-mcl-~a"
             (or #+powerpc "darwin" 
                 #-powerpc
                 (must-port '*compiled-directory-name*))
             (ccl::lisp-implementation-short-version))
     ;; ECL (Embedable Common Lisp):
     #+ecl
     (format nil "~a-ecl-~a"
             (or #+(and pentium3 linux) "linux86" 
                 #+(and pentium3 (not linux)) "windows"
                 #+(and (not pentium3) darwin) "darwin"
                 #+(and pentium3 darwin) "macosx86"
		 #-(or pentium3)
                 (must-port '*compiled-directory-name*))
             ;; Strip away any CVS info:
             (let ((full-version (lisp-implementation-version)))
               (subseq full-version 0 (position '#\space full-version))))
     ;; GCL:
     #+gcl
     (format nil "~a-gcl-~s.~s"
             (or #+linux "linux86" 
		 #+sparc "sparc"
		 #-(or linux sparc)
                 (must-port '*compiled-directory-name*))
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
              (must-port '*compiled-directory-name*))
             system::*major-version-number*
             system::*minor-version-number*)
     ;; OpenMCL:
     #+openmcl
     (format nil "~a-openmcl-~a.~a"
             (or
              #+darwin "darwin"
              #-darwin
              (must-port '*compiled-directory-name*))
             ccl::*openmcl-major-version*
             ccl::*openmcl-minor-version*)
     ;; SBCL:
     #+sbcl
     (format nil "~a-sbcl-~a" 
             (or 
              #+(and (not x86) darwin) "darwin"
              #+(and x86 darwin) "macosx86"
              #+sparc "sparc"
              #+(and x86 linux) "linux86"
              #+(and x86-64 linux) "linux86-64" ;; Thanks to Eric Menard
              #+(and x86 (not linux)) "windows"
              #-(or darwin sparc x86 (and x86-64 linux))
              (must-port '*compiled-directory-name*))
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
           cmu
           cormanlisp 
           digitool-mcl
           ecl
           lispworks
           openmcl
	   sbcl 
           scl)
     (must-port '*compiled-directory-name*)))

;;; ===========================================================================
;;;  Compiled File Type
;;;
;;; The mini-module system needs to know the file type of compiled files.
;;; The following form specifies the compiled-file type for a number of CL
;;; implementations. If you use the mini-module system with another CL
;;; implementation, you should add that implementation to the
;;; *compiled-file-type* form and e-mail the modified form to the GBBopen
;;; Project for inclusion in future releases.

(defparameter *compiled-file-type*
    (or
     ;; Franz Allegro:
     #+allegro
     excl:*fasl-default-type* 
     ;; CLISP:
     #+clisp
     (car custom:*compiled-file-types*)
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
     ;; OpenMCL:
     #+openmcl
     (pathname-type ccl:*.fasl-pathname*)
     ;; SBCL:
     #+sbcl
     sb-fasl:*fasl-file-type*
     ;; The Scieneer CL:
     #+scl
     (c:backend-fasl-file-type c:*backend*)
     ;; Unknown CL:
     #-(or allegro 
           clisp
           cmu
           cormanlisp
           digitool-mcl
           ecl
           lispworks
           openmcl
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
                                   ,*source-directory-name* 
                                   "mini-module")
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


