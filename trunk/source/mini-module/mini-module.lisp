;;;; -*- Mode:Common-Lisp; Package:MINI-MODULE; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/mini-module/mini-module.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr 27 14:06:08 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         Mini Module System
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill (incorporating some original ideas by 
;;;                          Kevin Gallagher and Zachary Rubinstein)
;;;
;;; Copyright (C) 2002-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;
;;;    The directory probing function, probe-directory, that is defined in
;;;    this file must be extended when porting to a new CL implementation.
;;;
;;; --------------------------------------------------------------------------
;;;
;;;  This Mini Module system provides a lightweight and easy to use
;;;  mechanism for maintaining (compiling and loading) module files.
;;;
;;;  This file assumes the global variables *compiled-directory-name* and
;;;  *compiled-file-type* have been defined by loading
;;;  mini-module-loader.lisp.
;;;
;;;  The Mini Module system supports the following directory layout:
;;;
;;;                             <root-directory>
;;;                               /          \
;;;                              /            \
;;;                           source    <compiled-cl-1>   ...
;;;                            / \            / \
;;;                           /  ..          ..  \
;;;                     mini-module           mini-module
;;;                         /                      \
;;;                        /                        \
;;;                mini-module.lisp            mini-module.<fasl>
;;;
;;;  This file can be used as a stand-alone system (when loaded by its
;;;  companion file, mini-module-loader.lisp).  Instructions for stand-alone
;;;  usage of the Mini Module system are provided in the
;;;  mini-module-startup.lisp file.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-13-02 File created.  (Corkill)
;;;  01-12-04 Added :create-dirs option to compile-module.  (Corkill)
;;;  01-29-04 Exported module-loaded-p.  (Corkill)
;;;  02-01-04 Support use of existing root-directory in define-root-directory.
;;;           (Corkill)
;;;  03-19-04 Added top-level Mini Module commands for Lispworks.  (Corkill)
;;;  03-19-04 Added file-options checking.  (Corkill)
;;;  06-10-04 Added proper :forces-recompile date checking and warning
;;;           messages.  (Corkill)
;;;  06-11-04 Moved to separate package (for stand-alone use).  (Corkill)
;;;  08-10-04 Removed make-directory in favor of ensure-directories-exist.
;;;           (Corkill)
;;;  08-18-04 Add missing slot-definition documentation method for Digitool
;;;           MCL.  (Corkill)
;;;  02-06-05 Added load-module-file.  (Corkill)
;;;  02-08-05 Added describe-module and brief-date-and-time.  (Corkill)
;;;  05-22-05 Added ECL support.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  06-18-05 Added module-directories.  (Corkill)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  04-11-06 Added *load-pathname* relative :directory option to
;;;           define-module.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  11-21-06 Added get-directory.  (Corkill)
;;;  06-06-07 Added :after-form support for modules (somewhat reluctantly,
;;;           as putting forms in a module's files is preferable to having
;;;           them in the module definition).  (Corkill)
;;;  07-14-07 Added subdirectories support to define-root-directory.  (Corkill)
;;;  07-14-07 Added :noautorun compile/load-module option.  (Corkill)
;;;  12-19-07 Added module-relative support to compute-relative-directory and
;;;           incremented version to 1.2.  (Corkill)
;;;  01-05-08 Skip undefined modules when performing compatiblity-ordering
;;;           check of a module.  (Corkill) 
;;;  03-29-08 Added :nopropagate (:propagate canceling) compile/load-module 
;;;           option.  (Corkill)
;;;  04-16-08 Support "Source" and "SOURCE" directory-name conventions (in
;;;           addition to conventional "source").  (Corkill)
;;;  04-19-08 Added application-version-identifier support to 
;;;           DEFINE-ROOT-DIRECTORY and incremented version to 1.3.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':mini-module)
    (error "This file should be loaded using the file ~
            mini-module-loader.lisp")))
         
(in-package :mini-module)

;;; ---------------------------------------------------------------------------
;;;   Check if we are good to go:

(flet ((check-var (var)
         (unless (boundp var)
           (error "~s is not defined.~
                   (This file should be loaded using the file ~
                    mini-module-loader.lisp)"
                  var))))
  (check-var '*compiled-directory-name*)
  (check-var '*compiled-file-type*))

;;; ===========================================================================
;;;   Imports to support using extended REPL commands:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*current-system-name*
            common-lisp-user::define-repl-command
            ;; deprecated, remove in 1.1:
            common-lisp-user::define-tll-command
            common-lisp-user::with-system-name)))

(declaim (special *current-system-name*))
(unless (boundp '*current-system-name*)
  (setf *current-system-name* nil))

;;; ===========================================================================
;;;   CL-User Global Variables
;;;
;;; Some CL implementations generate redefinition warnings when performing a
;;; compile/load/compile bootstrap sequence, so we don't use defvar's here to 
;;; set default values.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*automatically-create-missing-directories*
            common-lisp-user::*autorun-modules*
            common-lisp-user::*mini-module-compile-verbose*
            common-lisp-user::*mini-module-load-verbose*)))

;;; ---------------------------------------------------------------------------
;;;  Controls whether the Mini Module system automatically creates missing 
;;;  directories (without asking the user):

(declaim (special *automatically-create-missing-directories*))
(unless (boundp '*automatically-create-missing-directories*)
  (setf *automatically-create-missing-directories* nil))

;;; ---------------------------------------------------------------------------
;;;  When true, the Mini Module system will generate its own compile & load
;;;  messages if the corresponding *compile-verbose* or *load-verbose* values
;;;  are nil.

(declaim (special *mini-module-compile-verbose*))
(unless (boundp '*mini-module-compile-verbose*)
  (setf *mini-module-compile-verbose* nil))

(declaim (special *mini-module-load-verbose*))
(unless (boundp '*mini-module-load-verbose*)
  (setf *mini-module-load-verbose* nil))

;;; ---------------------------------------------------------------------------
;;;  Controls whether modules (such as GBBopen example and tests) autorun
;;;  themselves.

(declaim (special *autorun-modules*))
(unless (boundp '*autorun-modules*)
  (setf *autorun-modules* 't))

;;; ===========================================================================
;;;  Implementation-Specific Package & Feature Adjustments

;; Allow use of CMUCL package nicknames with SBCL:
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-impl::enter-new-nicknames (find-package "SB-PCL") '("PCL"))
  (sb-impl::enter-new-nicknames (find-package "SB-UNIX") '("UNIX")))

;;; ===========================================================================
;;;  Export user-level Mini Module names.  (Some of these names could collide
;;;  with similar names in other packages, but we export them anyway.)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*automatically-create-missing-directories*  ; re-exported from
                                                        ; :cl-user
            *autorun-modules*           ; re-exported from :cl-user
            *current-system-name*       ; re-exported from :cl-user
            *mini-module-compile-verbose* ; not yet documented
            *mini-module-load-verbose*  ; not yet documented
            *month-precedes-date*
            brief-date-and-time         ; part of tools, but placed here
            check-all-module-requires-orderings ; not yet documented
            compile-module
            compute-relative-directory  ; not documented
            define-relative-directory
            define-root-directory
            define-repl-command         ; re-exported from :cl-user
            define-tll-command          ; deprecated, remove in 1.1
            define-module
            describe-module
            dotted-conc-name            ; part of tools, but placed here; not
                                        ; documented
            get-directory
            get-root-directory          ; not yet documented
            list-modules                ; not yet documented
            load-module
            load-module-file
            mini-module-implementation-version ; not documented
            module-directories          ; not yet documented
            module-loaded-p
            need-to-port                ; not documented
            show-defined-directories
            show-modules                ; not yet documented
            undefine-directory          ; not yet documented
            undefine-module             ; not yet documented
            with-system-name            ; re-exported from :cl-user
            with-module-redefinitions   ; not yet documented
            )))

;;; ===========================================================================

(defun mini-module-implementation-version ()
  "1.3")

;;; Added to *features* at the end of this file:
(defparameter *mini-module-version-keyword* 
    ;; Support cross-case mode CLs:
    (read-from-string (format nil ":mini-module-~a" 
                              (mini-module-implementation-version))))

;;; ---------------------------------------------------------------------------

(defun print-mini-module-herald ()
  (format t "~%;;; ~72,,,'-<-~>
;;;  Mini-Module System ~a~@
;;;
;;;    Developed and supported by the GBBopen Project (http:/GBBopen.org/)
;;;    (See http://GBBopen.org/downloads/LICENSE for license details.)
;;; ~72,,,'-<-~>~2%"
          (mini-module-implementation-version)))
  
(eval-when (:load-toplevel)
  (print-mini-module-herald))

;;; ===========================================================================
;;;  Add missing slot-definition documentation method to Digitool MCL:

#+digitool-mcl 
(defmethod documentation ((object ccl::standard-slot-definition)
                          &optional doc-type)
  (declare (ignore doc-type))
  (when (and (slot-exists-p object 'documentation)
             (slot-boundp object 'documentation))
    (slot-value object 'documentation)))

;;; ===========================================================================
;;;  Dotted-conc-name

(defun dotted-conc-name (symbol)
  ;; Support reader-case-preserving CLs
  (concatenate 'simple-string (symbol-name symbol) "."))

;;; ===========================================================================
;;;  Brief-date-and-time
;;;
;;;  Returns formatted date/time string (brief, Unix ls-like form)--actually
;;;  part of the GBBopen-tools module.  It is placed here to use it with the
;;;  :mini-module package.
;;;
;;; ------------------------------------------------------------------------

(defvar *month-precedes-date* 't)

(defvar *month-name-vector* 
    (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun"
            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun brief-date-and-time (&optional time time-zone include-seconds)
  (let ((current-time (get-universal-time))
        time-difference)
    (if time
        (setf time-difference (abs (- current-time time)))
        (setf time current-time 
              time-difference 0))
    (multiple-value-bind (second minute hour date month year)
        (if time-zone 
            (decode-universal-time time time-zone)
            (decode-universal-time time))
      (declare (fixnum year))
      (let ((month-name (svref *month-name-vector* (1- month))))
        (if (< time-difference
               ;; 120 days:
               #.(* 60 60 24 120))
            (if *month-precedes-date*
                (format nil "~a ~2d ~2,'0d:~2,'0d~:[~;:~2,'0d~]"
                        month-name
                        date
                        hour
                        minute
                        include-seconds
                        second)
                (format nil "~2d ~a ~2,'0d:~2,'0d~:[~;:~2,'0d~]"
                        date
                        month-name
                        hour
                        minute
                        include-seconds
                        second))
            (if *month-precedes-date*     
                (format nil "~a ~2d, ~a~@[   ~]"
                        month-name
                        date
                        year
                        include-seconds)
                (format nil "~2d ~a, ~a~@[   ~]"
                        date
                        month-name
                        year
                        include-seconds)))))))

;;; ===========================================================================
;;;  Need-to-port reporting

(defun need-to-port-warning/error (obj &optional error)
  (funcall (if error 'error 'warn)
           "~s needs to be defined for ~a~@[ running on ~a~]."
           obj
           (lisp-implementation-type) 
           (machine-type)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro need-to-port (obj)
    ;;; Generate compile-time warnings of needed porting:
    (need-to-port-warning/error obj)
    ;; Error if called at run time:
    `(need-to-port-warning/error ',obj t)))

;;; ===========================================================================
;;;  Directories and modules hash tables

(defvar *mm-directories* (make-hash-table :test 'eq))
(defvar *mm-modules* (make-hash-table))

;;; ===========================================================================
;;;  Module Directories

(defstruct (mm-directory
            (:conc-name #.(dotted-conc-name 'mm-directory))
            (:copier nil))
  name
  (system-name *current-system-name*))

(defstruct (mm-root-directory
            (:include mm-directory)
            (:conc-name #.(dotted-conc-name 'mm-root-directory))
            (:copier nil))
  path
  application-version-modifier)

(defstruct (mm-relative-directory
            (:include mm-directory)
            (:conc-name #.(dotted-conc-name 'mm-relative-directory))
            (:copier nil))
  root
  subdirectories)

;;; ===========================================================================
;;;  Directory operators
;;;
;;; Unlike probe-file, probe-directory returns false on a non-directory file.
;;; It should return true for a symbolic link to a directory.

(defun probe-directory (path)
  #+allegro
  (excl:file-directory-p path)
  #+clisp
  (ignore-errors
   ;; CLISP's probe-directory function signals an error if path is not a
   ;; directory:
   (ext:probe-directory path))
  #+clozure
  (let ((pathname (probe-file path)))
    (and pathname
         (null (pathname-name pathname))
         (null (pathname-type pathname))))       
  #+(and cmu unix)
  (let ((dir (namestring 
              (make-pathname :name nil :type nil :defaults path))))
    (eq (unix::unix-file-kind dir) :directory))
  #+cormanlisp
  (cormanlisp:directory-p path)
  #+digitool-mcl
  (let ((pathname (probe-file path)))
    (and pathname
         (null (pathname-name pathname))
         (null (pathname-type pathname))))       
  #+ecl
  (let ((pathname (probe-file path)))
    (and pathname
         (null (pathname-name pathname))
         (null (pathname-type pathname))))       
  #+gcl
  ;; GCL's probe-file returns nil on directories, but directory returns
  ;; the directory (on linux, at least):
  (and (not (probe-file path))
       (directory path))
  #+lispworks
  (system::file-directory-p path)
  #+(and sbcl unix)
  (let ((dir (namestring 
              (make-pathname :name nil :type nil :defaults path))))
    (eq (sb-unix::unix-file-kind dir) :directory))
  #+(and scl unix)
  (ext:unix-namestring (make-pathname :name nil :type nil :version nil
                                      :defaults path))
  #-(or allegro
        clisp
        clozure
        (and cmu unix)
        cormanlisp 
        digitool-mcl
        ecl
        gcl
        lispworks 
        (and sbcl unix)
        (and scl unix))
  (need-to-port probe-directory))

;;; ---------------------------------------------------------------------------

(defun non-keyword-directory-name-error (name)
  (error "Directory name, ~s, must be a keyword." name))

;;; ---------------------------------------------------------------------------

(defun non-keyword-root/relative-directory-name-error (name)
  (error "Root or relative directory name, ~s, must be a keyword."
         name))

;;; ---------------------------------------------------------------------------

(defun get-mm-root-directory (name)
  (let ((mm-dir (gethash name *mm-directories*)))
    (typecase mm-dir
      (mm-root-directory mm-dir)
      (t (error "Root directory ~s is not defined." name)))))

;;; ---------------------------------------------------------------------------

(defun append-subdirectories (directory &rest subdirectory-lists)
  ;; Process pathname-directory :up keywords ourselves, to keep things pretty
  ;; on CLs that don't normalize aggressively (we'll leave :back
  ;; keywords--should anyone use them--to CL to deal with):
  (declare (dynamic-extent subdirectory-lists))
  (let ((directory (reverse directory)))
    (dolist (subdirectories subdirectory-lists)
      (dolist (subdirectory subdirectories)
        (cond ((and (eq subdirectory ':up) (stringp (first directory)))
               (pop directory))
              (t (push subdirectory directory)))))
    (nreverse directory)))

;;; ---------------------------------------------------------------------------

(defun compute-root-directory (spec subdirectories)
  (flet ((compute-it (spec)
           (let ((root-pathname
                  (etypecase spec
                    (pathname
                     (make-pathname :name nil :type nil :defaults spec))
                    (string (pathname spec))
                    (mm-root-directory (mm-root-directory.path spec)))))
             (make-pathname 
              :directory (append-subdirectories
                          (pathname-directory root-pathname)
                          subdirectories)
              :defaults root-pathname))))
    (typecase spec
      (symbol (compute-it
               (if (keywordp spec)
                   (get-mm-root-directory spec)
                   (symbol-value spec))))
      (otherwise (compute-it spec)))))

;;; ---------------------------------------------------------------------------

(defun define-root-directory (name spec &rest subdirectories)
  (declare (dynamic-extent subdirectories))
  (let ((application-version-modifier nil))
    (when (consp name)
      (setf application-version-modifier (second name))
      (setf name (first name)))
    (unless (keywordp name)
      (non-keyword-directory-name-error name))
    (let ((root-directory-path (compute-root-directory spec subdirectories)))
      (setf (gethash name *mm-directories*)
            (make-mm-root-directory
             :name name
             :path root-directory-path
             :application-version-modifier application-version-modifier)))))

;;; ---------------------------------------------------------------------------

(defun define-relative-directory (name root &rest subdirectories)
  (unless (keywordp name)
    (non-keyword-directory-name-error name))
  (unless (keywordp root)
    (non-keyword-root/relative-directory-name-error root))
  (setf (gethash name *mm-directories*)
        (make-mm-relative-directory
         :name name
         :root root
         :subdirectories subdirectories)))

;;; ---------------------------------------------------------------------------

(defun make-and-check-directory-pathname (name subdirectories compiled?
                                          application-version-modifier)
  ;;; Used by compute-relative-directory to handle various "source" 
  ;;; directory-name conventions:
  (labels ((make-directory-pathname (subtree-name 
                                     &optional skip-subdirectories?)
             (make-pathname 
              :directory (append-subdirectories (pathname-directory name)
                                                (list subtree-name)
                                                (unless skip-subdirectories?
                                                  subdirectories))
              :defaults name)))
    (cond
     ;; If compiled?, make and return the pathname (concatenating
     ;; application-version modifier, if appropriate):
     (compiled? 
      (make-directory-pathname
       (if application-version-modifier
           ;; Concatenate the version modifier to the compiled-directory name:
           (concatenate 'simple-string 
             *compiled-directory-name* "-" application-version-modifier)
           *compiled-directory-name*)))
     ;; Source directory: check what "source" directory name is needed before
     ;; making and returning the pathname:
     (t (cond
         ;; Regular "source":
         ((probe-directory (make-directory-pathname "source" t))
          (make-directory-pathname "source"))
         ;; "Source":
         ((probe-directory (make-directory-pathname "Source" t))
          (make-directory-pathname "Source"))
         ;; "SOURCE":
         ((probe-directory (make-directory-pathname "SOURCE" t))
          (make-directory-pathname "SOURCE"))
         ;; Otherwise, we'll just use "source":
         (t (make-directory-pathname "source")))))))

;;; ---------------------------------------------------------------------------

(declaim (ftype (function (mm-module) (values t &optional)) 
                mm-module.directory
                mm-module.subdirectories))

(defun compute-relative-directory (name subdirectories compiled?)
  (cond
   ((null name) nil)
   ;; `Name' can be a pathname if a *load-truename*-relative :directory
   ;; option was used in define-module:
   ((pathnamep name)
    (make-and-check-directory-pathname name subdirectories compiled? nil))
   (t (let ((mm-dir (gethash name *mm-directories*)))
        (typecase mm-dir
          (mm-relative-directory
           (compute-relative-directory
            (mm-relative-directory.root mm-dir)
            (append-subdirectories
             (mm-relative-directory.subdirectories mm-dir)
             subdirectories)
            compiled?))
          (mm-root-directory
           (let ((root-path (mm-root-directory.path mm-dir))
                 (application-version-modifier
                  (mm-root-directory.application-version-modifier mm-dir)))
             (make-and-check-directory-pathname 
              root-path subdirectories compiled?
              application-version-modifier)))
          (otherwise
           (let ((module 
                  ;; Check if we have a module reference (look without the
                  ;; get-module error check):
                  (gethash name *mm-modules*)))
             (cond 
              ;; The reference is module relative:
              (module
               (when (eq name (mm-module.directory module))
                 (error "Directory ~s is defined in terms of itself" name))
               (compute-relative-directory
                (mm-module.directory module)
                (append-subdirectories 
                 (mm-module.subdirectories module)
                 subdirectories)
                compiled?))
              (t (error "Directory ~s is not defined." name))))))))))
           
;;; ---------------------------------------------------------------------------

(defun undefine-directory (name)
  (if (gethash name *mm-directories*)
      (remhash name *mm-directories*)
      (warn "Directory ~s is not defined." name)))

;;; ---------------------------------------------------------------------------

(defun show-defined-directories ()
  (cond
   ((zerop (the fixnum (hash-table-count *mm-directories*)))
    (format t "~& No directories are defined.~%"))
   (t (let ((directories nil))
        (maphash #'(lambda (key directory)
                     (declare (ignore key)) 
                     (push directory directories))
                 *mm-directories*)
        (dolist (directory (sort directories #'string-lessp
                                 :key #'mm-directory.name))
          (typecase directory
            (mm-root-directory
             (format t "~&~s~%~4tRoot: ~a"
                     (mm-directory.name directory)
                     (mm-root-directory.path directory)))
            (t (let ((root-name (mm-relative-directory.root directory)))
                 (format t "~&~s~%~4tRelative to~:[ module~;~] ~(~s~)~
                            ~%~4tsubdirectories: ~s"
                         (mm-directory.name directory)
                         (gethash root-name *mm-directories*)
                         root-name
                         (mm-relative-directory.subdirectories directory))))))
        (terpri))))
  (terpri)
  (values))

;;; ===========================================================================
;;;  Modules

(defvar *skip-requires-ordering-check* nil)
(defvar *deferred-requires-ordering-check-module-names* nil)

;;; ---------------------------------------------------------------------------

(defstruct (mm-module
            (:conc-name #.(dotted-conc-name 'mm-module))
            (:copier nil))
  name
  (directory nil)
  (subdirectories)
  (requires nil)
  (files nil)
  (files-loaded nil)
  (load-completed? nil)
  (latest-forces-recompiled-date 0)
  ;; undocumented (used for compile-gbbopen exit):
  (after-form nil)
  (system-name *current-system-name*))

;;; ---------------------------------------------------------------------------

(defmacro with-module-redefinitions (&body body)
  ;; skip requires-ordering checks as we go:
  `(let ((*skip-requires-ordering-check* 't)
         (*deferred-requires-ordering-check-module-names* nil))
     ,@body
     ;; but do check them at the end:
     (check-all-module-requires-orderings 
      :module-names 
      ;; Check them in "as seen" order:
      (nreverse *deferred-requires-ordering-check-module-names*))))

;;; ---------------------------------------------------------------------------

(defmacro define-module (name &body args)
  (unless (keywordp name)
    (error "Module name, ~s, must be a keyword." name))
  (let ((directory nil)
        (directory-seen? nil)
        (subdirectories nil)
        (requires nil)
        (requires-seen? nil)
        (files nil)
        (files-seen? nil)
        (after-form nil)
        (after-form-seen? nil))
    (dolist (option args)
      (unless (and (consp option)
                   (keywordp (first option)))
        (error "Badly formed option, ~s, in module ~s.~_~
                Each option must be a list of the form (<option> ...)."
               option name))
      (case (first option)
        (:directory 
         (when directory-seen?
           (error "Multiple :directory options supplied in module ~s."
                  name))
         (setf directory-seen? 't)
         (setf directory (second option))
         (setf subdirectories (cddr option))
         (unless (or (not directory)
                     (keywordp directory))
           (error "The :directory specification supplied in module ~s ~_~
                   must begin with a keyword root or relative directory ~
                   name or nil: ~_~s"
                  name
                  option)))
        (:files
         (when files-seen?
           (error "Multiple :files options supplied in module ~s."
                  name))
         (setf files-seen? 't)
         (setf files (rest option)))
        (:requires 
         (when requires-seen?
           (error "Multiple :requires options supplied in module ~s."
                  name))
         (setf requires-seen? 't)
         (setf requires (rest option)))
        (:after-form
         (when after-form-seen?
           (error "Multiple :after-form options supplied in module ~s."
                  name))
         (setf after-form-seen? 't)
         (when (cddr option)
           (error "Only a single :after-form form can be specified:~{ ~s~}"
                  option))
         (setf after-form (second option)))
        (t (error "Unsupported option, ~s, in module ~s."
                  option name))))    
    (when (and files (not directory))
      (let ((truename *load-truename*))
        (if truename
            (setf directory 
              (make-pathname
               :name nil
               :type nil
               :defaults truename))
            (error "A ~s-relative :directory specification to ~s cannot ~
                    be evaluated outside of a load context."
                   '*load-truename*
                   'define-module))))
    `(ensure-module ',name ',directory ',subdirectories ',requires 
                    ',files
                    ',after-form)))

;;; ---------------------------------------------------------------------------

(defun get-module (name &optional (errorp t))
  (or (gethash name *mm-modules*)
      (when errorp
        (error "Module ~s is not defined." name))))

;;; ---------------------------------------------------------------------------

(defun determine-modules (module-names &optional skip-undefined-modules-p)
  (let ((result nil))
    (labels ((maybe-add-module (name)
               (let ((module (get-module name (not skip-undefined-modules-p))))
                 (when module
                   (dolist (name (mm-module.requires module))
                     (maybe-add-module name))
                   (pushnew module result :test #'eq :key #'mm-module.name)))))
      (dolist (name module-names)
        (maybe-add-module name)))
    ;; Maintain precedence order...
    (nreverse result)))

;;; ---------------------------------------------------------------------------

(defun compatible-ordering-p (seq1 seq2)
  ;; Returns true if all items in `seq1' that are also present in `seq2'
  ;; are in the same relative order in both sequences; nil otherwise.
  (let ((pos 0))
    (declare (fixnum pos))
    (dolist (item seq1 't)
      (let ((item-pos (position item seq2)))
        (when item-pos
          (locally (declare (fixnum item-pos))
            (cond ((< item-pos pos)
                   (return nil))
                  ((> item-pos pos)
                   (setf pos item-pos)))))))))

;;; ---------------------------------------------------------------------------

(defun check-requires-ordering (new-module-name new-module-requires)
  ;; Require `new-module-requires' to expand into a complete module
  ;; :requires list that is compatible with all existing module definitions.
  ;; This requirement ensures that module files will not be recompiled
  ;; solely due to a different relative ordering among defined modules.
  (let ((new-requires-list (determine-modules new-module-requires 't)))
    (maphash
     #'(lambda (name module)
         (unless (eq name new-module-name)
           (let ((requires-list (determine-modules
                                 (mm-module.requires module) 't)))
             (unless (compatible-ordering-p new-requires-list requires-list)
               ;; TODO: Someday add a wizard to suggest a compatible
               ;;       :requires list for the new module...
               (error "Module ~s is being defined with a fully expanded ~
                       :requires module order: ~:@_~s which is incompatible ~
                       with the fully expanded :requires order: ~:@_~s~
                       ~:@_of the defined module ~s. ~:@_The :requires ~
                       value that was specified for module ~s was: ~:@_~s."
                      new-module-name
                      (mapcar #'mm-module.name new-requires-list)
                      (mapcar #'mm-module.name requires-list)
                      name
                      new-module-name
                      new-module-requires)))))
     *mm-modules*)))

;;; ---------------------------------------------------------------------------

(defun check-all-module-requires-orderings (&key module-names silent)
  (maphash
   #'(lambda (name module)
       (when (or (not module-names)
                 (member name module-names :test #'eq))
         (check-requires-ordering name (mm-module.requires module))))
   *mm-modules*)
  (unless silent
    (format t "~&;; The :requires option in all module definitions are ~
                    consistent.~%")))

;;; ---------------------------------------------------------------------------

(defun ensure-module (name directory subdirectories requires files after-form)
  (let ((existing-module (gethash name *mm-modules*)))
    (if *skip-requires-ordering-check*
        (check-requires-ordering name requires)
        (push name *deferred-requires-ordering-check-module-names*))
    (setf (gethash name *mm-modules*)
          (make-mm-module 
           :name name 
           :directory directory
           :subdirectories subdirectories
           :requires requires
           :files files 
           :files-loaded 
             (when (and existing-module
                        ;; if the files specification has changed at all,
                        ;; reload them all...
                        (equal files (mm-module.files existing-module)))
               (mm-module.files-loaded existing-module))
           :after-form after-form)))
  ;; Add an ADSF component definition, if gbbopen.asd has been loaded:
  (when (fboundp 'mm-component-defsystem)
    (funcall 'mm-component-defsystem name))
  ;; Return the module name (returned by define-module):
  name)

;;; ---------------------------------------------------------------------------

(defun undefine-module (name)
  (get-module name)                     ; check that it is defined.
  ;; Remove ADSF component definition, if gbbopen.asd has been loaded:
  (when (fboundp 'mm-component-undefsystem)
    (funcall 'mm-component-undefsystem name))
  (remhash name *mm-modules*))

;;; ===========================================================================
;;;   Module compile/load functions

;; Dynamic binding used in support of :forces-recompile file option:
(defvar *latest-forces-recompile-date*)

;;; ---------------------------------------------------------------------------

(defparameter *compile-module-options*
    '(:create-dirs
      :create-directories               ; full-name synonym for :create-dirs
      :noautorun
      :nopropagate
      :print
      :propagate
      :recompile
      :reload 
      :source))

;;; ---------------------------------------------------------------------------

(defparameter *load-module-options*
    '(:noautorun
      :nopropagate
      :print
      :propagate
      :reload 
      :source))

;;; ---------------------------------------------------------------------------

(defparameter *compile/load-file-options*
    '(:recompile :reload :source :forces-recompile :noload))

;;; ---------------------------------------------------------------------------

(defun module-source/compiled-directories (module)
  (let* ((directory (mm-module.directory module))
         (subdirectories (mm-module.subdirectories module))
         (source-directory
          (compute-relative-directory directory subdirectories nil))
         (compiled-directory
          (compute-relative-directory directory subdirectories 't)))
    (values source-directory compiled-directory)))

;;; ---------------------------------------------------------------------------

(defun maybe-update-forces-recompile-date (new-date)
  (when (> new-date *latest-forces-recompile-date*)
    (setf *latest-forces-recompile-date* new-date)))

;;; ---------------------------------------------------------------------------

(defun load-file (path print?)
  ;; Generate our own load-verbose message:
  (when (and (not *load-verbose*)
             *mini-module-load-verbose*)
    (format t "~&;;; loading file ~a...~%"
            (namestring path)))
  (load path :print print?))

;;; ---------------------------------------------------------------------------

(defun compile/load-module-files-helper (module source-directory
                                         compiled-directory compile?
                                         recompile? reload? source? print?
                                         propagate?)
  (setf (mm-module.load-completed? module) nil)
  (with-compilation-unit ()
    (dolist (file (mm-module.files module))
      (let* ((file-options (when (consp file) 
                             (rest file)))
             (bad-options (set-difference file-options 
                                          *compile/load-file-options*
                                          :test #'eq))
             (file-name (if (consp file) (first file) file))
             (source-path (make-pathname
                           :name file-name
                           :type "lisp"
                           :defaults source-directory))
             (source-file-date (or (and (probe-file source-path)
                                        (file-write-date source-path)) 0))
             (compiled-path (make-pathname
                             :name file-name
                             :type *compiled-file-type*
                             :defaults compiled-directory))
             (compiled-file-date
              (or (and (probe-file compiled-path)
                       (file-write-date compiled-path))
                  -1))
             (files-loaded (mm-module.files-loaded module))
             (file-loaded-acons (assoc file-name files-loaded
                                       :test #'string=)))
        (when bad-options
          (warn "Invalid file option~p for ~s in module ~s: ~s"
                bad-options
                file-name
                (mm-module.name module)
                bad-options))           
        (flet ((load-it (path date)
                 (when (or reload?
                           (member ':reload file-options :test #'eq)
                           (not file-loaded-acons)
                           (> date (cdr file-loaded-acons)))
                   (load-file path print?)
                   (when (member ':forces-recompile file-options :test #'eq)
                     (let ((latest-source/compiled-file-date 
                            (max source-file-date compiled-file-date)))
                       (maybe-update-forces-recompile-date 
                        latest-source/compiled-file-date))
                     (setf (mm-module.latest-forces-recompiled-date module)
                           (max compiled-file-date
                                (mm-module.latest-forces-recompiled-date
                                 module))))
                   (if file-loaded-acons
                       ;; update the date in the existing acons:
                       (setf (cdr file-loaded-acons) date)
                       ;; add file and date as a new acons in files-loaded:
                       (setf (mm-module.files-loaded module)
                             (acons file-name date files-loaded))))
                 ;; warn that recompilation is needed:
                 (when (and (plusp compiled-file-date)
                            (> *latest-forces-recompile-date*
                               compiled-file-date)
                            (not (member ':source file-options :test #'eq)))
                   (format t "~&; File ~a in ~s needs to be recompiled.~%"
                           file-name (mm-module.name module)))))
          (when (and (not (member ':source file-options :test #'eq))
                     (or recompile? 
                         (member ':recompile file-options :test #'eq)
                         (and compile?
                              (or (> source-file-date compiled-file-date)
                                  (> *latest-forces-recompile-date* 
                                     compiled-file-date)))))
            ;; Delete the old compiled file, if it exists:
            (when (plusp compiled-file-date)
              (delete-file compiled-path))
            ;; Generate our own compile-verbose message:
            (when (and (not *compile-verbose*)
                       *mini-module-compile-verbose*)
              (format t "~&;;; Compiling file ~a...~%"
                      (namestring source-path)))
            (compile-file source-path 
                          :print print?
                          :output-file compiled-path)
            (setf compiled-file-date 
              (or (and (probe-file compiled-path)
                       (file-write-date compiled-path))
                  ;; Compiled file can be missing if compilation was
                  ;; aborted:
                  -1))
            (when (member ':forces-recompile file-options :test #'eq)
              (maybe-update-forces-recompile-date compiled-file-date)
              (setf (mm-module.latest-forces-recompiled-date module)
                    (max compiled-file-date
                         (mm-module.latest-forces-recompiled-date module)))
              (setf recompile? 't propagate? 't)))
          (unless (member ':noload file-options :test #'eq)
            (if (or source? (> source-file-date compiled-file-date))
                (load-it source-path source-file-date)
                (load-it compiled-path compiled-file-date)))))))
  (setf (mm-module.load-completed? module) 't)
  (maybe-update-forces-recompile-date 
   (mm-module.latest-forces-recompiled-date module))
  ;; return recompile? & propagate? values to use with remaining modules:
  (values recompile? propagate?))

;;; ---------------------------------------------------------------------------

(defun compile-module-files (module recompile? reload? source? print? 
                             propagate?)
  (multiple-value-bind (source-directory compiled-directory)
      (module-source/compiled-directories module)
    (when compiled-directory
      ;; Check if the compiled-directory exists; create it if automatically
      ;; creating missing directories  or if the user so directs:
      (unless (probe-directory compiled-directory)
        (when (or *automatically-create-missing-directories*
                  (restart-case
                      (error "Directory ~a in module ~s doesn't exist."
                             compiled-directory (mm-module.name module))
                    (create-it ()
                        :report "Create this directory."
                      't)
                    (create-all ()
                        :report #.(format nil 
                                          "Create this directory and ~
                                           any future missing directories.")
                      (setf *automatically-create-missing-directories* 't))))
          (ensure-directories-exist compiled-directory))))
    (compile/load-module-files-helper 
     module source-directory compiled-directory
     't recompile? reload? source? print? propagate?)))

;;; ---------------------------------------------------------------------------

(defun load-module-files (module reload? source? print?)
  (multiple-value-bind (source-directory compiled-directory)
      (module-source/compiled-directories module)
    (compile/load-module-files-helper 
     module source-directory compiled-directory
     nil nil reload? source? print? nil)))

;;; ---------------------------------------------------------------------------

(defun compile-module (module-names &rest options) 
  ;;; Compiles and loads a module.  Each compiled file is loaded immediately
  ;;; after compilation (unless :source is specified).  Options are keyword
  ;;; flags (not keyword-value pairs):
  ;;;
  ;;; Options:
  ;;;   :create-dirs Creates directories that are missing in the
  ;;;                compiled-file tree (also full :create-directories)
  ;;;   :noautorun   Sets *autorun-modules* to nil during loading
  ;;;   :nopropagate Ignores a specified :propagate option
  ;;;   :print       Enables form-level print during compiling/loading
  ;;;   :propagate   Applies the specified options to all required modules
  ;;;   :recompile   Compiles even if the compiled file is newer than the
  ;;;                source file
  ;;;   :reload      Loads even if already loaded
  ;;;   :source      Loads source even if the file is compiled
  ;;;                (implies :reload)
  (declare (dynamic-extent options))
  (when (keywordp module-names) (setf module-names (list module-names)))
  (dolist (option options)
    (unless (member option *compile-module-options* :test #'eq)
      (warn "Unrecognized compile-module option ~s, ignored." option)))
  (let* ((recompile? (member ':recompile options :test #'eq))
         (reload? (member ':reload options :test #'eq))
         (propagate? (and (member ':propagate options :test #'eq)
                          (not (member ':nopropagate options :test #'eq))))
         (source? (member ':source options :test #'eq))
         (print? (member ':print options :test #'eq))
         (*automatically-create-missing-directories*
          (or *automatically-create-missing-directories*
              (member ':create-dirs options :test #'eq)
              ;; For those who hate abbreviations:
              (member ':create-directories options :test #'eq)))
         (*autorun-modules*
          (if (member ':noautorun options :test #'eq) nil *autorun-modules*))
         (modules-to-load (determine-modules module-names))
         (*latest-forces-recompile-date* 0))
    ;; specifying :source implies :reload
    (when source? (setf reload? 't))        
    (dolist (module modules-to-load)
      (let ((specified-module? 
             (member (mm-module.name module) module-names :test #'eq)))
        (if (or propagate? specified-module?)              
            (multiple-value-setq (recompile? propagate?)
              (compile-module-files module recompile? reload? source? 
                                    print? propagate?))
            (load-module-files
             module 
             (and reload? propagate?) 
             (and source? (or propagate? specified-module?))
             print?)))
      (let ((after-form (mm-module.after-form module)))
        (when after-form (eval after-form))))))

;;; ---------------------------------------------------------------------------

(defun load-module (module-names &rest options)
  ;;; Loads a module.  By default, loads the newest of the source or compiled
  ;;; version of each file.  Options are keyword flags (not keyword-value
  ;;; pairs):
  ;;;
  ;;; Options:
  ;;;   :noautorun   Sets *autorun-modules* to nil during loading
  ;;;   :nopropagate Ignores a specified :propagate option
  ;;;   :print       Enables form-level print during compiling/loading
  ;;;   :propagate   Applies the specified options to all required modules
  ;;;   :recompile   Ignored by load-module
  ;;;   :reload      Loads even if already loaded
  ;;;   :source      Loads source (implies :reload)
  (declare (dynamic-extent options))
  (when (keywordp module-names) (setf module-names (list module-names)))
  (dolist (option options)
    (unless (member option *load-module-options* :test #'eq)
      (warn "Unrecognized load-module option ~s, ignored." option)))
  (let ((reload? (member ':reload options :test #'eq))
        (propagate? (and (member ':propagate options :test #'eq)
                         (not (member ':nopropagate options :test #'eq))))
        (source? (member ':source options :test #'eq))
        (print? (member ':print options :test #'eq))
        (*autorun-modules*
         (if (member ':noautorun options :test #'eq) nil *autorun-modules*))
        (modules-to-load (determine-modules module-names))
        (*latest-forces-recompile-date* 0))
  ;; specifying :source implies :reload:
    (when source? (setf reload? 't))        
    (dolist (module modules-to-load)
      (let ((specified-module? 
             (member (mm-module.name module) module-names :test #'eq)))
        (load-module-files 
         module 
         (and reload? 
              (or propagate? specified-module?))
         (and source? 
              (or propagate? specified-module?))
         print?))
      (let ((after-form (mm-module.after-form module)))
        (when after-form (eval after-form))))))

;;; ---------------------------------------------------------------------------

(defun load-module-file (module-name file-name &rest file-options)
  ;;; Specified loading of a single file in a module.  Always reloads
  ;;; the latest source/compiled file.
  ;;;
  ;;; Options:
  ;;;   :noautorun   Sets *autorun-modules* to nil during loading
  ;;;   :print       Enables form-level print during loading
  ;;;   :source      Loads source

  (declare (dynamic-extent file-options))
  (let ((module (get-module module-name)))
    (multiple-value-bind (source-directory compiled-directory)
        (module-source/compiled-directories module)
      (let* ((source-path (make-pathname
                           :name file-name
                           :type "lisp"
                           :defaults source-directory))
             (source-file-date (or (and (probe-file source-path)
                                        (file-write-date source-path)) 0))
             (compiled-path (make-pathname
                             :name file-name
                             :type *compiled-file-type*
                             :defaults compiled-directory))
             (compiled-file-date (or (and (probe-file compiled-path)
                                          (file-write-date compiled-path)) 
                                     -1))
             (files-loaded (mm-module.files-loaded module))
             (file-loaded-acons (assoc file-name files-loaded
                                       :test #'string=))
             (print? (member ':print file-options :test #'eq))
             (*autorun-modules*
              (if (member ':noautorun file-options :test #'eq)
                  nil
                  *autorun-modules*)))
        (flet ((load-it (path date)
                 (load-file path print?)
                 (if file-loaded-acons
                     ;; update the date in the existing acons:
                     (setf (cdr file-loaded-acons) date)
                     ;; add file and date as a new acons in files-loaded:
                     (setf (mm-module.files-loaded module)
                           (acons file-name date files-loaded)))
                 ;; Return the file path:
                 path))
          (if (or (member ':source file-options :test #'eq)
                  (> source-file-date compiled-file-date))
              (load-it source-path source-file-date)
              (load-it compiled-path compiled-file-date)))))))

;;; ---------------------------------------------------------------------------

(defun module-fully-loaded? (module)
  ;;; Internal function that returns true if `module' is fully loaded
  (let ((files-loaded (mm-module.files-loaded module)))
    (and (mm-module.load-completed? module)
         ;; Check that no new files have been specified for the module since
         ;; we last compiled/loaded:
         (every #'(lambda (file) 
                    (assoc (if (consp file) (first file) file)
                           files-loaded :test #'string=))
                (mm-module.files module)))))
      
;;; ---------------------------------------------------------------------------

(defun module-loaded-p (module-name)
  (module-fully-loaded? (get-module module-name)))

;;; ---------------------------------------------------------------------------
;;;
;;; SBCL's namestring functions (filesys.lisp) fail on pathnames containing
;;; :UNSPECIFIC names/types -- we "fix" it here.  (Note that CMUCL now has
;;; been "fixed" officially.)

#+sbcl
(sb-ext::without-package-locks
 (defun sb-impl::unparse-unix-piece (thing)
   (etypecase thing
     ((member :wild) "*")
     ((member :unspecific)              ; Added by DDC
      ;; CLHS 19.2.2.2.3.1 says "That is, both nil and :unspecific
      ;; cause the component not to appear in the namestring."
      "")
     (simple-string
      (let* ((srclen (length thing))
             (dstlen srclen))
        (dotimes (i srclen)
          (case (schar thing i)
            ((#\* #\? #\[)
             (incf dstlen))))
        (let ((result (make-string dstlen))
              (dst 0))
          (dotimes (src srclen)
            (let ((char (schar thing src)))
              (case char
                ((#\* #\? #\[)
                 (setf (schar result dst) #\\)
                 (incf dst)))
              (setf (schar result dst) char)
              (incf dst)))
          result)))
     (sb-impl::pattern
      (sb-impl::collect ((strings))
         (dolist (piece (sb-impl::pattern-pieces thing))
           (etypecase piece
             (simple-string
              (strings piece))
             (symbol
              (ecase piece
                (:multi-char-wild
                 (strings "*"))
                (:single-char-wild
                 (strings "?"))))
             (cons
              (case (car piece)
                (:character-set
                 (strings "[")
                 (strings (cdr piece))
                 (strings "]"))
                (t (error "invalid pattern piece: ~S" piece))))))
         (apply #'concatenate
                'simple-base-string
                (strings)))))))

;;; ---------------------------------------------------------------------------

(defun module-directories (module-name)
  (let ((module (get-module module-name)))
    (module-source/compiled-directories module)))

;;; ---------------------------------------------------------------------------

(defun describe-module (module-name)
  (let* ((module (get-module module-name))
         (files-loaded (mm-module.files-loaded module))
         (forces-recompile-date
          (mm-module.latest-forces-recompiled-date module)))
    (multiple-value-bind (source-directory compiled-directory)
        (module-source/compiled-directories module)
      (format t "~&Module ~s (~:[not ~;~]loaded)~
                 ~%  Requires: ~w~
                 ~%  Fully expanded requires: ~w~
                 ~%  Source directory: ~a~
                 ~%  Compiled directory: ~a~
                 ~%  Forces recompile date: ~a~
                 ~%  Files:"
              module-name
              (mm-module.load-completed? module)
              (mm-module.requires module)
              (mapcar #'mm-module.name 
                      (determine-modules (mm-module.requires module) 't))
              (if source-directory
                  (namestring source-directory)
                  "[Undefined]")
              (if compiled-directory
                  (namestring compiled-directory)
                  "[Undefined]")
              (if (zerop forces-recompile-date)
                  "None"
                  (brief-date-and-time forces-recompile-date))))
    (dolist (file (mm-module.files module))
      (multiple-value-bind (file-name options)
          (if (consp file) (values (car file) (cdr file)) file)
        (let ((loaded-date (cdr (assoc file-name files-loaded 
                                       :test #'string=))))
          (format t "~9t~@[~a~]~22t~a ~@[~w~]~%" 
                  (and loaded-date (brief-date-and-time loaded-date)) 
                  file-name
                  options)))))
  (values))

;;; ---------------------------------------------------------------------------

(defun show-modules (&optional all-modules?)
  (cond 
   ((zerop (the fixnum (hash-table-count *mm-modules*)))
    (format t "~& No modules are defined.~%"))
   (t (let ((modules nil))
        (maphash #'(lambda (key module)
                     (declare (ignore key))
                     (when (or all-modules?
                               (module-fully-loaded? module))
                       (push module modules)))
                 *mm-modules*)
        (cond 
         (modules
          (if all-modules?
              (format t "~&Defined Modules:")
              (format t "~&Loaded Modules:"))
          (dolist (module (sort modules #'string-lessp :key #'mm-module.name))
            (format t "~%  ~s~:[~; [~a]~]"
                    (mm-module.name module)
                    all-modules?
                    (if (module-fully-loaded? module)
                        "Loaded"
                        "Not loaded")))
          (terpri))
         (t (format t "~&No modules are loaded.~%"))))))
  (terpri)
  (values))

;;; ---------------------------------------------------------------------------

(defun list-modules (&optional all-modules?)
  (loop for module being each hash-value in *mm-modules*
      when (or all-modules?
                (module-fully-loaded? module))
      collect (mm-module.name module)))

;;; ===========================================================================
;;;  Get directory

(defun get-directory (name &rest subdirectories)
  (declare (dynamic-extent subdirectories))
  (compute-relative-directory name subdirectories nil))

;;; ---------------------------------------------------------------------------
;;;  Get root directory

(defun get-root-directory (name)
  ;;; Returns the root directory of `name', where name can be a root
  ;;; directory, a relative directory, or a module-relative directory.
  (let ((mm-dir (gethash name *mm-directories*)))
    (typecase mm-dir
      (mm-relative-directory
       (get-root-directory (mm-relative-directory.root mm-dir)))
      (mm-root-directory (mm-root-directory.path mm-dir))
      (otherwise
       (let ((module 
              ;; Check if we have a module reference (look without the
              ;; get-module error check):
              (gethash name *mm-modules*)))
         (cond 
          ;; The reference is module relative:
          (module
           (when (eq name (mm-module.directory module))
             (error "Directory ~s is defined in terms of itself" name))
           (get-root-directory (mm-module.directory module)))
          (t (error "Directory ~s is not defined." name))))))))
 
;;; ===========================================================================
;;;  System listing and deleting

(defun list-all-systems ()
  (let ((result nil))
    (loop 
        for directory being each hash-value in *mm-directories*
        for system-name = (mm-directory.system-name directory)
        when system-name do
          (pushnew system-name result))
    (loop 
        for module being each hash-value in *mm-modules*
        for system-name = (mm-module.system-name module)
        when system-name do
          (pushnew system-name result))
    result))

;;; ---------------------------------------------------------------------------

(defun undefine-system-directories-and-modules (system-name)
  ;;; Deletes all directory and module definitions that are tagged with
  ;;; `system-name'.  
  (loop   ;; delete directories:
      for directory-name being each hash-key in *mm-directories*
      using (hash-value directory)
      when (eq system-name (mm-directory.system-name directory))
      do (undefine-directory directory-name))
  (loop   ;; delete modules:
      for module-name being each hash-key in *mm-modules*
      using (hash-value module)
      when (eq system-name (mm-module.system-name module))
      do (undefine-module module-name)))

;;; ===========================================================================
;;;  Define the Mini Module directory root and the :mini-module and
;;;  :mini-module-user modules

(let ((*current-system-name* ':mini-module))
    
  (define-root-directory :mini-module-root *load-truename* :up :up)
  
  (define-module :mini-module
    (:directory :mini-module-root "mini-module")
    (:files "mini-module"))
  
  (define-module :mini-module-user
    (:requires :mini-module)
    (:directory :mini-module-root "mini-module")
    (:files "mini-module-user")))

;;; ---------------------------------------------------------------------------
;;;  Record this file as loaded in the Mini Module hash table (due to bootstrap
;;;  loading)

(let* ((mini-module (gethash :mini-module *mm-modules*))
       (this-file (or *load-truename*
                      ;; CormanLisp doesn't bind *load-truename* properly 
                      ;; during bootstrapping, so we hardcode the pathname
                      ;; during compilation:
                      #+cormanlisp
                      #.*compile-file-truename*))
       (this-file-name (pathname-name this-file))
       (files-loaded (mm-module.files-loaded mini-module))
       (file-loaded-acons (assoc this-file-name files-loaded 
                                 :test #'string=))
       (date (file-write-date this-file)))
  (if file-loaded-acons
      (setf (rest file-loaded-acons) date)
      (setf (mm-module.files-loaded mini-module)
            (acons this-file-name date files-loaded))))
  
;;; ===========================================================================
;;;  :mini-module-user Module

(load-module :mini-module-user)

;;; ===========================================================================
;;;  Mini Module REPL Commands (requires extended-REPL command support)

(defvar *last-lm/cm-module* nil)
(defvar *last-lm-options* nil)
(defvar *last-cm-options* nil)

(defun do-mini-module-repl-command (cmd module-and-options)
  (let ((recalled-options nil))
    (destructuring-bind (&optional module-name &rest options)
        module-and-options
      ;; No module-name was specified, but one was remembered:
      (when (and (not module-name) 
                 *last-lm/cm-module*)
        (setf recalled-options 't)
        (setf module-name *last-lm/cm-module*)
        (setf options (if (eq cmd ':cm)
                          *last-cm-options*
                          *last-lm-options*)))
      (cond 
       ;; New module-name or options were specified:
       (module-name
        (let ((fn (ecase cmd
                    (:cm 'compile-module)
                    (:lm 'load-module))))
          (setf *last-lm/cm-module* module-name)
          (cond 
           ;; :cm command:
           ((eq cmd ':cm)
            (setf *last-cm-options* options)
            (setf *last-lm-options* (intersection options *load-module-options*
                                                  :test #'eq)))
           ;; :lm command:
           (t (setf *last-lm-options* options)))
          (when recalled-options
            (format *trace-output* "~&;; ~(~s ~s~)~{ ~(~s~)~}~%"
                    cmd module-name options))
          (apply fn module-name options)))
       ;; No module was ever specified:
       (t (format *trace-output* 
                  "~&;; ~(~s~) -- No previous module specified."
                  cmd))))))

;;; ---------------------------------------------------------------------------

(when (fboundp 'define-repl-command)
  (let ((*current-system-name* ':mini-module))
    
    (define-repl-command (:lm :add-to-native-help) 
        (&rest module-name-and-options)
      "Load module"
      (do-mini-module-repl-command ':lm module-name-and-options))
  
    (define-repl-command (:cm :add-to-native-help)
        (&rest module-name-and-options)
      "Compile and load module"
      (do-mini-module-repl-command ':cm module-name-and-options))))
  
;;; ===========================================================================
;;;   Mini Module system is fully loaded:

(pushnew :mini-module *features*)
(pushnew *mini-module-version-keyword* *features*)

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


