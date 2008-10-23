;;;; -*- Mode:Common-Lisp; Package:MODULE-MANAGER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/module-manager/module-manager-loader.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Oct 22 18:47:22 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Module-Manager System Bootstrap Loader
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
;;;    To use the Module-Manager System separate from the GBBopen Project
;;;    software tree, do the following:
;;;      1. Create a "root" directory to contain the module-manager software tree
;;;         (e.g., $ mkdir my-tree)
;;;      2. Create the module-manager portion of the source tree in that 
;;;         "root" directory  (e.g., $ cd my-tree; mkdir -p source/module-manager)
;;;      3. Copy the module-manager-loader.lisp, module-manager.lisp, and
;;;         module-manager-user.lisp files into the source/module-manager directory
;;;      4. Start your CL and then load the module-manager-loader.lisp file:
;;;          > (load "my-tree/source/module-manager/module-manager-loader")
;;;      5. Compile the :module-manager and :module-manager-user modules:
;;;          > (module-manager:compile-module :module-manager-user
;;;                                        :create-dirs :propagate)
;;;    The Module-Manager Facility can now be used stand-alone by loading
;;;    source/module-manager-loader.lisp as part of your Common Lisp
;;;    initialization.
;;;
;;; Porting Notice:
;;;
;;;    The defparameter forms (in this file) for:
;;;       *compiled-directory-name*
;;;       *compiled-file-type*
;;;    must be extended when porting to a new CL implementation.
;;;
;;;  This file defines the :module-manager package and the global variables
;;;  *compiled-directory-name* and *compiled-file-type*.  It then loads either
;;;  the source or compiled module-manager file (whichever is more recent).
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

(unless (find-package ':module-manager)
  (make-package ':module-manager 
                :use '(:common-lisp)
                :nicknames '(:mini-module)))

(in-package :module-manager)

(export '(need-to-port                  ; not documented
          ))

;;; ===========================================================================
;;;  Need-to-port reporting

(defun need-to-port-warning/error (entity error)
  (funcall (if error 'error 'warn)
           "~s needs to be defined for ~a~@[ running on ~a~].~
            ~@[~%(Please send this error message and the result of ~
               ~% evaluating (pprint *features*) to bugs@gbbopen.org.)~]"
           entity
           (lisp-implementation-type) 
           (machine-type)
           error))

;;; ---------------------------------------------------------------------------

(defmacro need-to-port (entity &optional only-warn)
  ;; Generate compile-time warnings of needed porting:
  (need-to-port-warning/error entity nil)
  ;; Error if called at run time:
  (unless only-warn
    `(need-to-port-warning/error ',entity t)))

;;; ===========================================================================
;;; Add a single feature to identify sufficiently new Digitool MCL
;;; implementations (both Digitool MCL and pre-1.2 Clozure CL include the
;;; feature mcl):

#+(and digitool ccl-5.1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew ':digitool-mcl *features*))

;;; ---------------------------------------------------------------------------
;;; Add clozure feature to legacy OpenMCL:

#+(and openmcl (not clozure))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew ':clozure *features*))

;;; ===========================================================================
;;; The module-manager system uses a separate compiled directory tree for each CL
;;; implementation and version.  The following form creates a unique name for
;;; the root of this tree for a number of CL implementations.  If you use the
;;; module-manager system with another CL implementation, you should add that
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
              (need-to-port *compiled-directory-name*))
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
              (need-to-port *compiled-directory-name*))
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
              (need-to-port *compiled-directory-name*))
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
                 (need-to-port *compiled-directory-name*))
             (ccl::lisp-implementation-short-version))
     ;; ECL (Embedable Common Lisp):
     #+ecl
     (format nil "~a-ecl-~a"
             (or #+(and (or pentium3 pentium4) linux) "linux86" 
                 #+(and (or pentium3 pentium4) darwin) "macosx86" 
                 #+(and (or pentium3 pentium4) (not (or linux darwin))) "windows"
                 #+(and (not (or pentium3 pentium4)) darwin) "darwin"
		 #-(or pentium3 pentium4)
                 (need-to-port *compiled-directory-name*))
             ;; Strip away any CVS info:
             (let ((full-version (lisp-implementation-version)))
               (subseq full-version 0 (position '#\space full-version))))
     ;; GCL:
     #+gcl
     (format nil "~a-gcl-~s.~s"
             (or #+linux "linux86" 
		 #+sparc "sparc"
		 #-(or linux sparc)
                 (need-to-port *compiled-directory-name*))
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
              (need-to-port *compiled-directory-name*))
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
              (need-to-port *compiled-directory-name*))
             (lisp-implementation-version))
     ;; The Scieneer CL:
     #+scl
     (let ((case-mode ext::*case-mode*))
       (format nil "~a-scl~:[~;-~(~a~)~]-~a"
               (or #+(and x86 linux (not 64-bit)) "linux86"
                   #+(and x86 linux 64-bit) "linux86-64"
                   #+(and x86 solaris (not 64-bit)) "solaris-x86"
                   #+(and x86 solaris 64-bit) "solaris-x86-64"
                   #+(and sparc solaris (not 64-bit)) "sparc"
                   #+(and sparc solaris 64-bit) "sparc64"
                   #+(and hpux (not 64-bit)) "hpux"
                   #+(and hpux 64-bit) "hpux-64"
                   (need-to-port *compiled-directory-name*))
               (not (eq case-mode ':upper))
               case-mode
               (lisp-implementation-version)))
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
     (need-to-port *compiled-directory-name*)))

;;; ===========================================================================
;;;  Compiled File Type
;;;
;;; The module-manager system needs to know the file type of compiled files.  The
;;; following form specifies the compiled-file type for a number of CL
;;; implementations. If you use the module-manager system with another CL
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
     (need-to-port *compiled-file-type*)))

;;; ===========================================================================
;;;  Load the module-manager system (source or compiled file)

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
                                   "source" "module-manager")
                    :version :newest
                    :defaults root-pathname))
                  (compiled-path
                   (make-pathname
                    :type *compiled-file-type*
                    :directory `(,@(pathname-directory root-pathname)
                                   ,*compiled-directory-name* 
                                   "module-manager")
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
    (load-source-or-compiled-file "module-manager")))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


