;;;; -*- Mode:Common-Lisp; Package:MINI-MODULE; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/mini-module/mini-module.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jul  1 05:56:09 2008 *-*
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
;;;  01-29-04 Exported MODULE-LOADED-P.  (Corkill)
;;;  02-01-04 Support use of existing root-directory in DEFINE-ROOT-DIRECTORY.
;;;           (Corkill)
;;;  03-19-04 Added top-level Mini Module commands for Lispworks.  (Corkill)
;;;  03-19-04 Added file-options checking.  (Corkill)
;;;  06-10-04 Added proper :forces-recompile date checking and warning
;;;           messages.  (Corkill)
;;;  06-11-04 Moved to separate package (for stand-alone use).  (Corkill)
;;;  08-10-04 Removed MAKE-DIRECTORY in favor of ENSURE-DIRECTORIES-EXIST.
;;;           (Corkill)
;;;  08-18-04 Add missing slot-definition documentation method for Digitool
;;;           MCL.  (Corkill)
;;;  02-06-05 Added LOAD-MODULE-FILE.  (Corkill)
;;;  02-08-05 Added DESCRIBE-MODULE and BRIEF-DATE-AND-TIME.  (Corkill)
;;;  05-22-05 Added ECL support.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  06-18-05 Added MODULE-DIRECTORIES.  (Corkill)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  04-11-06 Added *load-pathname* relative :directory option to
;;;           DEFINE-MODULE.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  11-21-06 Added GET-DIRECTORY.  (Corkill)
;;;  06-06-07 Added :after-form support for modules (somewhat reluctantly,
;;;           as putting forms in a module's files is preferable to having
;;;           them in the module definition).  (Corkill)
;;;  07-14-07 Added subdirectories support to DEFINE-ROOT-DIRECTORY.  (Corkill)
;;;  07-14-07 Added :noautorun compile/load-module option.  (Corkill)
;;;  12-19-07 Added module-relative support to COMPUTE-RELATIVE-DIRECTORY and
;;;           incremented Mini Module version to 1.2.  (Corkill)
;;;  01-05-08 Skip undefined modules when performing compatiblity-ordering
;;;           check of a module.  (Corkill) 
;;;  03-29-08 Added :nopropagate (:propagate canceling) compile/load-module 
;;;           option.  (Corkill)
;;;  04-16-08 Support "Source" and "SOURCE" directory-name conventions (in
;;;           addition to conventional "source").  (Corkill)
;;;  04-19-08 Added application-version-identifier support to 
;;;           DEFINE-ROOT-DIRECTORY and incremented version to 1.3.  (Corkill)
;;;  05-15-08 Added PARSE-DATE.  (Corkill)
;;;  06-23-08 Added BRIEF-DATE.  (Corkill)
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
;;;  Allow-redefinition (placed here for very early use)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro allow-redefinition (&body body)
    ;;; Still need to support CMUCL, ECL, SBCL, and SCL
    `(#+allegro excl:without-redefinition-warnings
      #+clisp let
      #+clisp ((custom:*suppress-check-redefinition* 't))
      #+closure let
      #+closure ((ccl:*warn-if-redefine*) nil)
      #+digitool-mcl let
      #+digitool-mcl ((ccl:*warn-if-redefine*) nil)
      #+lispworks system::without-warning-on-redefinition
      #-(or allegro clisp closure digitool-mcl lispworks)
      progn
      (progn ,@body))))

;;; ===========================================================================
;;;   Imports to support using extended REPL commands:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*current-system-name*
            common-lisp-user::define-repl-command
            common-lisp-user::with-system-name)))

(declaim (special *current-system-name*))
(unless (boundp '*current-system-name*)
  (setf *current-system-name* nil))

(unless (macro-function 'with-system-name)
  ;; This is a copy of the definition in ../../extended-repl.lisp.  It is
  ;; needed to support startup.lisp only invocation.
  (allow-redefinition
   (defmacro with-system-name ((&optional system-name) &body body)
     (unless (keywordp system-name)
       (error "System name, ~s, must be a keyword." system-name))
     `(let ((*current-system-name* ',system-name))
        ,@body))))

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
            common-lisp-user::*mini-module-load-verbose*
            common-lisp-user::*patches-only*)))

;;; ---------------------------------------------------------------------------
;;;  Controls whether the Mini Module system automatically creates missing 
;;;  directories (without asking the user):

(declaim (special *automatically-create-missing-directories*))
(unless (boundp '*automatically-create-missing-directories*)
  (setf *automatically-create-missing-directories* nil))

;;; ---------------------------------------------------------------------------
;;;  Controls whether the Mini Module system compiles/loads patches only:

(declaim (special *patches-only*))
(unless (boundp '*patches-only*)
  (setf *patches-only* nil))

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
            *current-module*            ; not documented
            *current-system-name*       ; re-exported from :cl-user
            *mini-module-compile-verbose* ; not yet documented
            *mini-module-load-verbose*  ; not yet documented
            *month-precedes-date*       ; part of tools, but placed here
            *patches-only*              ; re-exported from :cl-user
            allow-redefinition          ; part of tools, but placed here
            brief-date                  ; part of tools, but placed here
            brief-date-and-time         ; part of tools, but placed here
            check-all-module-requires-orderings ; not yet documented
            compile-module
            compute-relative-directory  ; not documented
            continue-patch
            define-relative-directory
            define-root-directory
            define-repl-command         ; re-exported from :cl-user
            define-module
            describe-module
            describe-patches
            dotted-conc-name            ; part of tools, but placed here; not
                                        ; documented
            finish-patch
            get-directory
            get-patch-description
            get-root-directory          ; not yet documented
            list-modules                ; not yet documented
            load-module
            load-module-file
            mini-module-implementation-version ; not documented
            module-directories          ; not yet documented
            module-loaded-p
            need-to-port                ; not documented
            parse-date                  ; part of tools, but placed here
            patch
            patch-loaded-p
            printv                      ; part of tools, but placed here
            show-defined-directories
            show-modules                ; not yet documented
            start-patch
            undefine-directory          ; not yet documented
            undefine-module             ; not yet documented
            with-system-name            ; re-exported from :cl-user
            with-module-redefinitions   ; not yet documented
            )))

;;; ===========================================================================

(allow-redefinition
 (defun mini-module-implementation-version ()
   "1.4"))

;;; Added to *features* at the end of this file:
(defparameter *mini-module-version-keyword* 
    ;; Support cross-case mode CLs:
    (read-from-string (format nil ":mini-module-~a" 
                              (mini-module-implementation-version))))

;;; ---------------------------------------------------------------------------

(allow-redefinition
 (defun print-mini-module-herald ()
   (format t "~%;;; ~72,,,'-<-~>
;;;  Mini-Module System ~a~@
;;;
;;;    Developed and supported by the GBBopen Project (http:/GBBopen.org/)
;;;    (See http://GBBopen.org/downloads/LICENSE for license details.)
;;; ~72,,,'-<-~>~2%"
           (mini-module-implementation-version)))
 
 (eval-when (:load-toplevel)
   (print-mini-module-herald)))

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
;;;  Printv
;;;
;;;  A handy debugging macro
;;;
;;; Placed here to make this macro available ASAP

(defmacro printv (&rest forms)
  (let ((values (gensym)))
    `(let* ((*print-readably* nil)
            (,values (list ,@(mapcar #'(lambda (form)
					 `(multiple-value-list ,form))
				     forms))))
       (declare (dynamic-extent ,values))
       (loop
           for form in ',forms
	   and value in ,values
	   do (typecase form
		(keyword (format *trace-output* "~&;; ~s~%" form))
		(string (format *trace-output* "~&;; ~a~%" form))
		(t (format *trace-output* 
			   "~&;;  ~w =>~{ ~w~^;~}~%" form value))))
       (force-output *trace-output*)
       (values-list (first (last ,values))))))

;;; ===========================================================================
;;;  Dotted-conc-name

(defun dotted-conc-name (symbol)
  ;; Support reader-case-preserving CLs
  (concatenate 'simple-string (symbol-name symbol) "."))

;;; ===========================================================================
;;;  Declared numerics that are used in this file

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro & (arg)
    ;;; Wraps (the fixnum ...) around `arg'
    `(the #-full-safety fixnum #+full-safety t ,arg))

  ;; CLISP can't handle compile-time (flet (...) (defmacro ...)) forms, so we
  ;; define fixnum-up as a full function here:
  (allow-redefinition
   (defun fixnum-op (op args &optional result values-types)
     ;; Builds a form declaring all the arguments to OP to be fixnums.  If
     ;; `result' is true then the result of the operation is also a fixnum
     ;; unless `values-types' is specified.
     (let ((form `(,op ,@(mapcar #'(lambda (x) `(& ,x)) args))))
       (if result
           `(the #-full-safety ,(if values-types
                                    (cons 'values values-types)
                                    'fixnum)
                 #+full-safety t
                 ,form)
           form))))
  
  (defmacro +& (&rest args) (fixnum-op '+ args t))
  (defmacro 1-& (&rest args) (fixnum-op '1- args t))
  (defmacro *& (&rest args) (fixnum-op '* args t))
  (defmacro =& (&rest args) (fixnum-op '= args))
  (defmacro <& (&rest args) (fixnum-op '< args))
  (defmacro >=& (&rest args) (fixnum-op '>= args))
  (defmacro min& (&rest args) (fixnum-op 'min args t))
  (defmacro truncate& (&rest args)
    (fixnum-op 'truncate args t '(fixnum fixnum)))

  (define-modify-macro incf& (&optional (increment 1)) +&))

;;; ===========================================================================
;;;  BRIEF-DATE, BRIEF-DATE-AND-TIME, and PARSE-DATE--part of the
;;;  GBBopen-tools module.  They are placed here to use with the :mini-module
;;;  package.

(defvar *month-precedes-date* 't)

(defparameter *month-name-vector*
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defparameter *month-full-name-vector*
    #("January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November" "December"))

;;; ---------------------------------------------------------------------------

(defun brief-date (&optional time time-zone (destination nil))
  ;;;  Returns formatted date string
  (unless time (setf time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (if time-zone 
          (decode-universal-time time time-zone)
          (decode-universal-time time))
    (declare (ignore second minute hour))
    (let ((month-name (svref *month-name-vector* (1-& month))))
      (if *month-precedes-date*     
          (format destination "~a ~2d, ~s"
                  month-name
                  date
                  year)
          (format destination "~2d ~a, ~s"
                  date
                  month-name
                  year)))))

;;; ---------------------------------------------------------------------------

(defun brief-date-and-time (&optional time time-zone include-seconds
                                      (destination nil))
  ;;;  Returns formatted date/time string (brief, Unix ls-like form)
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
      (let ((month-name (svref *month-name-vector* (1-& month))))
        (if (<& time-difference
                ;; 120 days:
                #.(* 60 60 24 120))
            (if *month-precedes-date*
                (format destination "~a ~2d ~2,'0d:~2,'0d~:[~;:~2,'0d~]"
                        month-name
                        date
                        hour
                        minute
                        include-seconds
                        second)
                (format destination "~2d ~a ~2,'0d:~2,'0d~:[~;:~2,'0d~]"
                        date
                        month-name
                        hour
                        minute
                        include-seconds
                        second))
            (if *month-precedes-date*     
                (format destination "~a ~2d, ~a~@[   ~]"
                        month-name
                        date
                        year
                        include-seconds)
                (format destination "~2d ~a, ~a~@[   ~]"
                        date
                        month-name
                        year
                        include-seconds)))))))

;;; ---------------------------------------------------------------------------

(defun parse-date (string &key (start 0) 
                               (end (length string))
                               (junk-allowed nil)
                               (separators "-/ ,")
                               (month-precedes-date *month-precedes-date*))
  ;;; Parses many intuitive date formats (sensitive to month-precedes-date,
  ;;; if needed):
  (declare (simple-string string))
  (let (date month year month-preceded-date)
    (flet ((process-date ()
             (multiple-value-setq (date start)
               (parse-integer string :start start :end end :junk-allowed t)))
           (process-month ()
             (cond
              ;; Numeric month:
              ((digit-char-p (schar string start))
               (multiple-value-setq (month start)
                 (parse-integer string :start start :end end :junk-allowed t)))
              ;; Month name:
              (t (let* (month-string
                        (month-equal-fn
                         #'(lambda (month-name)
                             (when (string-equal 
                                    string month-name
                                    :start1 start
                                    :end1 (min& end
                                                (+& start 
                                                    (length month-name))))
                               (setf month-string month-name)))))
                   (setf month (or (position-if month-equal-fn 
                                                *month-full-name-vector*)
                                   (position-if month-equal-fn
                                                *month-name-vector*)))
                   (unless month
                     (error "Unable to determine the month in ~s" string))
                   (incf& month)
                   (incf& start (length month-string))))))
           (skip-separators ()
             (loop 
                 while (and (<& start end)
                            (find (schar string start) separators))
                 do (incf& start))))
      (skip-separators)
      ;; If we have a month name, then we know the month-date order; otherwise
      ;; we'll assume month-first for until we process the second field:
      (when (alpha-char-p (schar string start))
        (setf month-preceded-date 't))
      (process-month)
      (skip-separators)
      (cond
       ((and (not month-preceded-date)
             (or
              ;; We have a month name in the second field:
              (alpha-char-p (schar string start))
              ;; Use the month-precedes-date value to decide the order:
              (not month-precedes-date)))
        ;; We actually have the date value from the first field (rather than
        ;; the month), so set the date from the assumed month value and then
        ;; proceed with month processing:
        (setf date month)
        (process-month))
       ;; Simply continue, as we have month then date order:
       (t (process-date)))
      (skip-separators))
    (check-type month (integer 1 12))
    (check-type date (integer 1 31))
    ;; Process year:
    (cond 
     ;; Assumed year, if omitted:
     ((=& start end)
      (multiple-value-bind (seconds minutes hours 
                            current-date current-month current-year)
          (get-decoded-time)
        (declare (ignore seconds minutes hours))
        (setf year current-year)
        ;; Assume next year, if the date is past in the current year:
        (when (or (<& month current-month)
                  (and (=& month current-month)
                       (<& date current-date)))
          (incf& year))))
     ;; Otherwise, process the specified year:
     (t (multiple-value-setq (year start)
          (parse-integer string :start start :end end 
                         :junk-allowed junk-allowed))))
    (check-type year (integer 0 #.most-positive-fixnum))
    ;; Upgrade YY to YYYY -- YY assumed within +/- 50 years from current time
    ;; (if year < 100):
    (setf year (cond 
                ;; No year upgrade needed:
                ((>=& year 100) year)
                ;; Do the upgrade:
                (t (let ((current-century
                          (*& 100 (truncate& (nth-value 5 (get-decoded-time))
                                             100))))
                     (if (>=& year 50) 
                         (+& year current-century -100)
                         (+& year current-century))))))
    (values date month year)))

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
                                          application-version-modifier patches?)
  ;;; Used by compute-relative-directory to handle various "source" 
  ;;; directory-name conventions:
  (labels ((make-directory-pathname (subtree-name 
                                     &optional skip-subdirectories?)
             (make-pathname 
              :directory (append-subdirectories 
                          (pathname-directory name)
                          (list subtree-name)
                          (unless skip-subdirectories?
                            (if patches?
                                (append subdirectories '("patches"))
                                subdirectories)))
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

;; CMUCL 19e complains about the following declaration:
#-cmu
(declaim (ftype (function (mm-module) (values t &optional)) 
                mm-module.directory
                mm-module.subdirectories))

(defun compute-relative-directory (name subdirectories compiled? 
                                   &optional patches?)
  (let ((in-process nil))
    (labels 
        ((compute-it (name subdirectories)
           (cond
            ((null name) nil)
            ;; `Name' can be a pathname if a *load-truename*-relative
            ;; :directory option was used in define-module:
            ((pathnamep name)
             (make-and-check-directory-pathname
              name subdirectories compiled? nil patches?))
            (t (let ((mm-dir (gethash name *mm-directories*)))
                 (typecase mm-dir
                   (mm-relative-directory
                    (when (member name in-process :test #'eq)
                      (error "Circularity in relative-directory relation:~
                             ~%~3t~{~s -> ~}~s"
                             (reverse in-process) name))
                    (push name in-process)
                    (compute-it
                     (mm-relative-directory.root mm-dir)
                     (append-subdirectories
                      (mm-relative-directory.subdirectories mm-dir)
                      subdirectories)))
                   (mm-root-directory
                    (let ((root-path (mm-root-directory.path mm-dir))
                          (application-version-modifier
                           (mm-root-directory.application-version-modifier
                            mm-dir)))
                      (make-and-check-directory-pathname 
                       root-path subdirectories compiled?
                       application-version-modifier patches?)))
                   (otherwise
                    (let ((module 
                           ;; Check if we have a module reference (look
                           ;; without the get-module error check):
                           (gethash name *mm-modules*)))
                      (cond 
                       ;; The reference is module relative:
                       (module
                        (when (eq name (mm-module.directory module))
                          (error "Directory ~s is defined in terms of itself" 
                                 name))
                        (compute-relative-directory
                         (mm-module.directory module)
                         (append-subdirectories 
                          (mm-module.subdirectories module)
                          subdirectories)
                         compiled?
                         patches?))
                       (t (error "Directory ~s is not defined."
                                 name)))))))))))
      (compute-it name subdirectories))))
           
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
  :;; NOTE: Changes to slots must be reflected in ENSURE-MODULE:
  name
  (directory nil)
  (subdirectories)
  (requires nil)
  (files nil)
  (files-loaded nil)
  (patches nil)
  (load-completed? nil)
  (latest-forces-recompiled-date 0)
  ;; undocumented (used for compile-gbbopen exit):
  (after-form nil)
  (system-name *current-system-name*)
  (patch-descriptions nil))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object mm-module) stream)
  (cond
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (object stream :type t)
        (format stream "~s"
                (mm-module.name object)))
      ;; Print-object must return object:
      object)))

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
        (patches nil)
        (patches-seen? nil)
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
        (:patches
         (when patches-seen?
           (error "Multiple :patches options supplied in module ~s."
                  name))
         (setf patches-seen? 't)
         (setf patches (rest option)))
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
    (when (and (or files patches) (not directory))
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
                    ',files ',after-form ',patches)))

;;; ---------------------------------------------------------------------------

(defun get-module (name &optional (errorp t))
  (or (gethash name *mm-modules*)
      (when errorp
        (error "Module ~s is not defined." name))))

;;; ---------------------------------------------------------------------------

(defun determine-modules (module-names &optional skip-undefined-modules-p)
  (let ((result nil)
        (in-process nil))
    (labels ((maybe-add-module (name)
               (let ((module (get-module name (not skip-undefined-modules-p))))
                 (when module
                   (when (member name in-process :test #'eq)
                     (error "Circularity in :requires option:~
                             ~%~3t~{~s -> ~}~s"
                            (reverse in-process) name))
                   (push name in-process)
                   (dolist (name (mm-module.requires module))
                     (maybe-add-module name))
                   (pushnew module result :test #'eq :key #'mm-module.name)
                   (setf in-process (delete name in-process :test #'eq))))))
      (dolist (module-name module-names)
        (maybe-add-module module-name)))
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

(defun check-requires-ordering (module-name)
  ;; Check that the :requires list of `module-name' is compatible with all
  ;; existing module definitions.  This requirement ensures that module files
  ;; will not be recompiled solely due to a different relative ordering among
  ;; defined modules.
  (let ((module-requires-list (determine-modules (list module-name) 't)))
    (maphash
     #'(lambda (name module)
         (unless (eq name module-name)
           (let ((requires-list 
                  (determine-modules (mm-module.requires module) 't)))
             (unless (compatible-ordering-p module-requires-list requires-list)
               ;; TODO: Someday add a wizard to suggest a compatible
               ;;       :requires list for the new module...
               (error "Module ~s is being defined with a fully expanded ~
                       :requires module order: ~:@_~s which is incompatible ~
                       with the fully expanded :requires order: ~:@_~s~
                       ~:@_of the defined module ~s. ~:@_The :requires ~
                       value that was specified for module ~s was: ~:@_~s."
                      module-name
                      (mapcar #'mm-module.name module-requires-list)
                      (mapcar #'mm-module.name requires-list)
                      name
                      module-name
                      (mapcar #'mm-module.name
                              (mm-module.requires 
                               (get-module module-name t))))))))
     *mm-modules*)))

;;; ---------------------------------------------------------------------------

(defun check-all-module-requires-orderings (&key module-names silent)
  (maphash #'(lambda (name module)
               (declare (ignore module))
               (when (or (not module-names)
                         (member name module-names :test #'eq))
                 (check-requires-ordering name)))
           *mm-modules*)
  (unless silent
    (format t "~&;; The :requires option in all module definitions are ~
                    consistent.~%")))

;;; ---------------------------------------------------------------------------

(defun ensure-module (name directory subdirectories requires files after-form
                      patches)
  (unless (every #'keywordp requires)
    (error "The ~s option for module ~s contains a non-keyword module name."
           (cons ':requires requires)
           name))
  (let ((existing-module (gethash name *mm-modules*)))
    (cond 
     ;; Update existing module definition:
     (existing-module
      ;; if the files specification has changed at all, reload them all...
      (unless (equal files (mm-module.files existing-module))
        (setf (mm-module.files-loaded existing-module) nil))
      ;; Update module with the given arguments:      
      (setf (mm-module.directory existing-module) directory)
      (setf (mm-module.subdirectories existing-module) subdirectories)
      (setf (mm-module.requires existing-module) requires)
      (setf (mm-module.files existing-module) files)
      (setf (mm-module.patches existing-module) patches)
      (setf (mm-module.after-form existing-module) after-form)
      ;; Also update system-name:
      (setf (mm-module.system-name existing-module) *current-system-name*))
     ;; Create a new module definition:
     (t (setf (gethash name *mm-modules*)
              (make-mm-module 
               :name name 
               :directory directory
           :subdirectories subdirectories
           :requires requires
           :files files 
           :patches patches
           :after-form after-form)))))
  ;; Check requires ordering for consistency with other modules:
  (if *skip-requires-ordering-check*
      (push name *deferred-requires-ordering-check-module-names*)
      (check-requires-ordering name))
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
;;;  Patch primitives (placed here to avoid forward references)

(defvar *current-module* nil)
;; Set to the file-name of the file being loaded:
(defvar *current-file-name* nil)
;; True when compiling a file:
(defvar *compiling-file* nil)
;; Holds the description of the patch currently being loaded:
(defvar *loading-patch* nil)

;;; ---------------------------------------------------------------------------

(defstruct (patch-description
            (:conc-name #.(dotted-conc-name 'patch-description))
            (:copier nil))
  id 
  date
  date-loaded 
  author 
  description
  ;; Used to maintain module reference when loading patch:
  module
  ;; Remember the file containing the patch:
  file-name)

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object patch-description) stream)
  (cond
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (object stream :type t)
        (format stream "~a~@[ ~s~]"
                (patch-description.id object)
                (let ((module (patch-description.module object)))
                  (when (mm-module-p module)
                    (mm-module.name module)))))
      ;; Print-object must return object:
      object)))

;;; ---------------------------------------------------------------------------

(defun %find-patch-desc (id &optional (module *current-module*))
  (find id (mm-module.patch-descriptions module) 
        :test #'equal
        :key #'patch-description.id))

;;; ---------------------------------------------------------------------------

(defun %make-patch (id date author description &optional reload)
  (declare (ignore reload))             ; reload is deprecated, remove soon!!!
  (cond
   (*compiling-file*)
   (t (when (%find-patch-desc id)
        (format t "~&;; Reloading previously loaded patch ~s to module ~s~%"
                id
                (mm-module.name *current-module*)))
      (setf *loading-patch* (make-patch-description
                             :id id
                             :date date
                             :date-loaded (get-universal-time)
                             :author author
                             :description description
                             :module *current-module*
                             :file-name *current-file-name*)))))

;;; ---------------------------------------------------------------------------

(defun %commit-patch ()
  (when (patch-description-p *loading-patch*)
    (unless *compiling-file*
      (let* ((module (patch-description.module *loading-patch*))
             (id (patch-description.id *loading-patch*))
             (existing-description-sublist 
              (member id (mm-module.patch-descriptions module)
                      :test #'equal
                      :key #'patch-description.id)))
        (if existing-description-sublist
            ;; Replace the existing patch description:
            (setf (car existing-description-sublist)
                  *loading-patch*)
            ;; Add a new patch description to the end:
            (setf (mm-module.patch-descriptions module)
                  (nconc (mm-module.patch-descriptions module)
                         (list *loading-patch*))))
        (format t "~&;; Applied patch ~s to module ~s~%"
                id
                (mm-module.name module))))
    (setf *loading-patch* nil)))

;;; ---------------------------------------------------------------------------

(defun %check-patch ()
  ;; Checks that there is an open patch
  (if (or *compiling-file* *loading-patch*)
      ;; Return value is deprecated, remove on next release (change to unless)!
      't
      (error "No patch has been started.")))

;;; ---------------------------------------------------------------------------

(defun %check-no-patch ()
  ;; Checks that there no patch open
  (when *loading-patch*
    (unless *compiling-file* 
      (warn "Patch ~a of module ~s was not completed"
            (patch-description.id *loading-patch*)
            (mm-module.name (patch-description.module *loading-patch*))))
    (setf *loading-patch* nil)))

;;; ---------------------------------------------------------------------------

(defun %module-fully-loaded? (module)
  ;;; Internal function that returns true if `module' is fully loaded
  (let ((files-loaded (mm-module.files-loaded module)))
    (flet ((check-file (file) 
             (assoc (if (consp file) (first file) file)
                    files-loaded :test #'string=)))
      (and (mm-module.load-completed? module)
           ;; Check that no new files have been specified for the module since
           ;; we last compiled/loaded:
           (every #'check-file (mm-module.files module))
           ;; or new patches:
           (every #'check-file (mm-module.patches module))))))
      
;;; ---------------------------------------------------------------------------

(defun module-loaded-p (module-name)
  (%module-fully-loaded? (get-module module-name)))

;;; ===========================================================================
;;;   Module compile/load functions

;; Dynamic binding used in support of :forces-recompile file option:
(defvar *latest-forces-recompile-date*)

;;; ---------------------------------------------------------------------------

(defparameter *compile-module-options*
    '(:create-dirs
      :create-directories               ; full-name synonym for :create-dirs
      :noautorun
      :nopatches
      :nopropagate
      :patches-only
      :print
      :propagate
      :recompile
      :reload 
      :source))

;;; ---------------------------------------------------------------------------

(defparameter *load-module-options*
    '(:noautorun
      :nopatches
      :nopropagate
      :patches-only
      :print
      :propagate
      :reload 
      :source))

;;; ---------------------------------------------------------------------------

(defparameter *compile/load-file-options*
    ;; A patch files can also have the option :developing, which is added
    ;; contextually in COMPILE/LOAD-MODULE-FILES-HELPER:
    '(:recompile :reload :source :forces-recompile :noload))

;;; ---------------------------------------------------------------------------

(defun module-source/compiled-directories (module &optional patches?)
  (let* ((directory (mm-module.directory module))
         (subdirectories (mm-module.subdirectories module))
         (source-directory
          (compute-relative-directory directory subdirectories nil patches?))
         (compiled-directory
          (compute-relative-directory directory subdirectories 't patches?)))
    (values source-directory compiled-directory)))

;;; ---------------------------------------------------------------------------

(defun maybe-update-forces-recompile-date (new-date)
  (when (> new-date *latest-forces-recompile-date*)
    (setf *latest-forces-recompile-date* new-date)))

;;; ---------------------------------------------------------------------------

(defun %load-file (file-name path print?)
  ;; Generate our own load-verbose message:
  (when (and (not *load-verbose*)
             *mini-module-load-verbose*)
    (format t "~&;;; loading file ~a...~%"
            (namestring path)))
  (let ((*loading-patch* nil)
        (*current-file-name* file-name))
    (prog1 (load path :print print?)
      (%check-no-patch))))

;;; ---------------------------------------------------------------------------

(defun compile/load-module-files-helper (*current-module*
                                         source-directory
                                         compiled-directory compile?
                                         recompile? reload? source? print?
                                         propagate? patches?
                                         &aux (module *current-module*))
  (when (and *patches-only* (not (%module-fully-loaded? *current-module*)))
    (error "Module ~s has not been loaded and ~s is true."
           (mm-module.name *current-module*)
           '*patches-only*))
  (with-compilation-unit ()
    (dolist (file (if patches?
                      (mm-module.patches module)
                      (mm-module.files module)))
      (let* ((file-options (when (consp file) 
                             (rest file)))
             (bad-options (set-difference file-options 
                                          (if patches?
                                              (cons ':developing *compile/load-file-options*)
                                              *compile/load-file-options*)
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
        (labels ((consider-file-p (compile? date)
                   (if patches? 
                       (or (member ':developing file-options :test #'eq)
                           ;; Patch hasn't been loaded?
                           (not (member-if
                                 #'(lambda (patch-description)
                                     (equal (patch-description.file-name
                                             patch-description)
                                            file-name))
                                 (mm-module.patch-descriptions module)))
                           ;; Warn that we are skipping this patch:
                           (when (and date (> date (cdr file-loaded-acons)))
                             (format t "~&;; Not ~:[reloading~;recompiling~] ~
                                             patch file ~s in module ~s~%"
                                     compile?
                                     file-name
                                     (mm-module.name *current-module*))
                             ;; return nil:
                             nil))
                       (not *patches-only*)))
                 (load-it (path date)
                   (when (and (consider-file-p nil date)
                              (or reload?
                                  (member ':reload file-options :test #'eq)
                                  (not file-loaded-acons)
                                  (> date (cdr file-loaded-acons))))
                     (%load-file file-name path print?)
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
          (let ((recompile-needed 
                 (and compile?
                      (or (> source-file-date compiled-file-date)
                          (> *latest-forces-recompile-date* 
                             compiled-file-date)))))
            (when (and (consider-file-p 't (when recompile-needed source-file-date))
                       (not (member ':source file-options :test #'eq))
                       (or recompile? 
                           (member ':recompile file-options :test #'eq)
                           recompile-needed))
              ;; Delete the old compiled file, if it exists:
              (when (plusp compiled-file-date)
                (delete-file compiled-path))
              ;; Generate our own compile-verbose message:
              (when (and (not *compile-verbose*)
                         *mini-module-compile-verbose*)
                (format t "~&;;; Compiling file ~a...~%"
                        (namestring source-path)))
              (let ((*compiling-file* 't)
                    (*loading-patch* nil))
                (compile-file source-path 
                              :print print?
                              :output-file compiled-path)
                (%check-no-patch))
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
                  (load-it compiled-path compiled-file-date))))))))
  (setf (mm-module.load-completed? module) 't)
  (maybe-update-forces-recompile-date 
   (mm-module.latest-forces-recompiled-date module))
  ;; return recompile? & propagate? values to use with remaining modules:
  (values recompile? propagate?))

;;; ---------------------------------------------------------------------------

(defun compile-module-files (module recompile? reload? source? print? 
                             propagate? patches?)
  (multiple-value-bind (source-directory compiled-directory)
      (module-source/compiled-directories module patches?)
    (when compiled-directory
      ;; Check if the compiled-directory exists or if it is not needed.  If it
      ;; is needed and missing, create it if automatically creating missing
      ;; directories or if the user so directs:
      (unless (or (probe-directory compiled-directory)
                  ;; Compiled directory is not needed:
                  (flet ((all-source-p (file-specs)
                           (every #'(lambda (file-spec)
                                      (let ((file-options (when (consp file-spec) 
                                                            (rest file-spec))))
                                        (member ':source file-options :test #'eq)))
                                  file-specs)))
                    (if patches?
                        (all-source-p (mm-module.patches module))
                        (all-source-p (mm-module.files module)))))
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
     't recompile? reload? source? print? propagate? patches?)))

;;; ---------------------------------------------------------------------------

(defun load-module-files (module reload? source? print? patches?)
  (multiple-value-bind (source-directory compiled-directory)
      (module-source/compiled-directories module patches?)
    (compile/load-module-files-helper 
     module source-directory compiled-directory
     nil nil reload? source? print? nil patches?)))

;;; ---------------------------------------------------------------------------

(defun compile-module (module-names &rest options) 
  ;;; Compiles and loads a module.  Each compiled file is loaded immediately
  ;;; after compilation (unless :source is specified).  Options are keyword
  ;;; flags (not keyword-value pairs):
  ;;;
  ;;; Options:
  ;;;   :create-dirs   Creates directories that are missing in the
  ;;;                  compiled-file tree (also full :create-directories)
  ;;;   :noautorun     Sets *autorun-modules* to nil during loading
  ;;;   :nopatches     Does not compile or load any patches
  ;;;   :nopropagate   Ignores a specified :propagate option
  ;;;   :patches-only  Does not compile or reload any non-patch files
  ;;;   :print         Enables form-level print during compiling/loading
  ;;;   :propagate     Applies the specified options to all required modules
  ;;;   :recompile     Compiles even if the compiled file is newer than the
  ;;;                  source file
  ;;;   :reload        Loads even if already loaded
  ;;;   :source        Loads source even if the file is compiled
  ;;;                  (implies :reload)
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
         (nopatches? (member ':nopatches options :test #'eq))
         (*patches-only* (or *patches-only*
                             (member ':patches-only options :test #'eq)))
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
    ;; Compile & load files as needed (in module order):
    (dolist (module modules-to-load)
      (let ((specified-module? 
             (member (mm-module.name module) module-names :test #'eq)))
        (if (or propagate? specified-module?)              
            (multiple-value-setq (recompile? propagate?)
              (compile-module-files module recompile? reload? source? 
                                    print? propagate? nil))
            (load-module-files
             module 
             (and reload? propagate?) 
             (and source? (or propagate? specified-module?))
             print? nil))))
    ;; Compile & load patches as needed (in module order):
    (unless nopatches?
      (dolist (module modules-to-load)
        (let ((specified-module? 
               (member (mm-module.name module) module-names :test #'eq)))
          (if (or propagate? specified-module?)              
              (multiple-value-setq (recompile? propagate?)
                (compile-module-files module recompile? reload? source? 
                                      print? propagate? 't))
              (load-module-files
               module 
               (and reload? propagate?) 
               (and source? (or propagate? specified-module?))
               print? 't)))))
    ;; Now do any after forms (in module order):
    (dolist (module modules-to-load)
      (let ((after-form (mm-module.after-form module)))
        (when after-form (eval after-form))))))

;;; ---------------------------------------------------------------------------

(defun load-module (module-names &rest options)
  ;;; Loads a module.  By default, loads the newest of the source or compiled
  ;;; version of each file.  Options are keyword flags (not keyword-value
  ;;; pairs):
  ;;;
  ;;; Options:
  ;;;   :noautorun     Sets *autorun-modules* to nil during loading
  ;;;   :nopatches     Does not load any patches
  ;;;   :nopropagate   Ignores a specified :propagate option
  ;;;   :patches-only  Does not reload any non-patch files
  ;;;   :print         Enables form-level print during compiling/loading
  ;;;   :propagate     Applies the specified options to all required modules
  ;;;   :recompile     Ignored by load-module
  ;;;   :reload        Loads even if already loaded
  ;;;   :source        Loads source (implies :reload)
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
        (nopatches? (member ':nopatches options :test #'eq))
        (*patches-only* (or *patches-only*
                            (member ':patches-only options :test #'eq)))
        (*autorun-modules*
         (if (member ':noautorun options :test #'eq) nil *autorun-modules*))
        (modules-to-load (determine-modules module-names))
        (*latest-forces-recompile-date* 0))
    ;; specifying :source implies :reload:
    (when source? (setf reload? 't))        
    ;; Load files as needed (in module order):
    (dolist (module modules-to-load)
      (let ((specified-module? 
             (member (mm-module.name module) module-names :test #'eq)))
        (load-module-files 
         module 
         (and reload? 
              (or propagate? specified-module?))
         (and source? 
              (or propagate? specified-module?))
         print? nil)))
    ;; Load patches as needed (in module order):
    (unless nopatches?
      (dolist (module modules-to-load)
        (let ((specified-module? 
               (member (mm-module.name module) module-names :test #'eq)))
          (load-module-files
           module 
           (and reload? 
                (or propagate? specified-module?))
           (and source? 
                (or propagate? specified-module?))
           print? 't))))
    ;; Now do any after forms (in module order):
    (dolist (module modules-to-load)
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
                 (%load-file file-name path print?)
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
;;;
;;; SBCL's namestring functions (filesys.lisp) fail on pathnames containing
;;; :UNSPECIFIC names/types -- we "fix" it here.  (Note that CMUCL now has
;;; been "fixed" officially.)

#+sbcl
(sb-ext::without-package-locks
 (allow-redefinition
  (defun sb-impl::unparse-unix-piece (thing)
    (etypecase thing
      ((member :wild) "*")
      ((member :unspecific)             ; Added by DDC
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
                (strings))))))))

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
                 ~%  Files:  "          ; 2 trailing spaces req'd
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
                  (brief-date-and-time forces-recompile-date)))
      (flet ((show-file (file-name options)
               (let ((loaded-date (cdr (assoc file-name files-loaded 
                                              :test #'string=))))
                 (format t "~11t~@[~a~]~@[*~*~]~25t~a~@[ ~w~]~%" 
                         (and loaded-date (brief-date-and-time loaded-date)) 
                         (when loaded-date
                           (let* ((source-path (make-pathname
                                                :name file-name
                                                :type "lisp"
                                                :defaults source-directory))
                                  (source-file-date 
                                   (or (and (probe-file source-path)
                                            (file-write-date source-path)) 0)))
                             (> source-file-date loaded-date)))
                         file-name
                         options))))
        (dolist (file (mm-module.files module))
          (multiple-value-bind (file-name options)
              (if (consp file)
                  (values (car file) (cdr file))
                  file)
            (show-file file-name options)))
        (let ((patches (mm-module.patches module)))
          (when patches
            (format t "~&  Patches:")
            (setf source-directory (module-source/compiled-directories module 't))
            (dolist (patch patches)
              (multiple-value-bind (file-name options)
                  (if (consp patch) 
                      (values (car patch) (cdr patch))
                      patch)
                (show-file file-name options))))))))
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
                               (%module-fully-loaded? module))
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
                    (if (%module-fully-loaded? module)
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
                (%module-fully-loaded? module))
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
;;;  Patch entities

(defmacro start-patch ((id date &key (author "Anonymous") description reload)
                       &body body)
  (declare (ignore reload))             ; :reload is deprecated, remove soon!!
  (let ((date (multiple-value-call 
                  #'encode-universal-time 
                ;; noon on the given date
                0 0 12 (parse-date date))))
    `(progn
       (%check-no-patch)
       (%make-patch ,id ,date ,author ,description)
       ,@body)))

;;; ---------------------------------------------------------------------------

(defmacro continue-patch (&body body)
  `(progn (%check-patch)
          ,@body))

;;; ---------------------------------------------------------------------------

(defmacro finish-patch (&body body)
  `(progn (%check-patch)
          ,@body
          (%commit-patch)))

;;; ---------------------------------------------------------------------------

(defmacro patch ((&rest info) &body body)
  `(progn (start-patch ,info ,@body)
          (finish-patch)))

;;; ---------------------------------------------------------------------------

(defun get-patch-description (id module-name)
  (let* ((module (get-module module-name))
         (description (%find-patch-desc id module)))
    (when description
      (values (patch-description.id description)
              (patch-description.date description)
              (patch-description.author description)
              (patch-description.description description)
              (patch-description.date-loaded description)
              (patch-description.file-name description)))))

;;; ---------------------------------------------------------------------------

(defun patch-loaded-p (id module-name)
  (when (get-patch-description id module-name) 't))

;;; ---------------------------------------------------------------------------

(defun describe-patches (module-name)
  (let ((module (get-module module-name)))
    (cond 
     ((mm-module.load-completed? module)
      (dolist (patch-description (mm-module.patch-descriptions module))
        (format t "~&;; ~a~10t~a ~a (loaded ~a)~%"
                (patch-description.id patch-description)
                (brief-date
                 (patch-description.date patch-description))
                (patch-description.author patch-description)
                (brief-date-and-time 
                 (patch-description.date-loaded patch-description)))
        (let ((description (patch-description.description patch-description)))
          (dolist (description (if (listp description)
                                   description
                                   (list description)))
            (format t "~&;;~13t~a~%" description)))))
     (t (format t "~&;; Module ~s is not loaded~%"
                (mm-module.name module)))))
  (values))
  
;;; ===========================================================================
;;;  Define the Mini Module directory root and the :mini-module and
;;;  :mini-module-user modules

(let ((*current-system-name* ':mini-module))
    
  (define-root-directory :mini-module-root *load-truename* :up :up)
  
  (define-module :mini-module
    (:directory :mini-module-root "mini-module")
    (:files ("mini-module" :forces-recompile)))
  
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
;;;  Mini Module REPL-commands support

(defvar *last-lm/cm-module* nil)
(defvar *last-lm-options* nil)
(defvar *last-cm-options* nil)

(defun do-mini-module-repl-command (cmd module-and-options 
                                    &optional dont-remember)
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
          (unless dont-remember
            (setf *last-lm/cm-module* module-name)
            (cond 
             ;; :cm command:
             ((eq cmd ':cm)
              (setf *last-cm-options* options)
              (setf *last-lm-options* 
                    (intersection options *load-module-options* :test #'eq)))
             ;; :lm command:
             (t (setf *last-lm-options* options))))
          (when recalled-options
            (format *trace-output* "~&;; ~(~s ~s~)~{ ~(~s~)~}~%"
                    cmd module-name options))
          (apply fn module-name options)))
       ;; No module was ever specified:
       (t (format *trace-output* 
                  "~&;; ~(~s~) -- No previous module specified."
                  cmd))))))

;;; ===========================================================================
;;;   Mini Module system is fully loaded:

(pushnew :mini-module *features*)
(pushnew *mini-module-version-keyword* *features*)

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


