;;;; -*- Mode:Common-Lisp; Package:MINI-MODULE; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/mini-module/mini-module.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jan 10 22:43:07 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         Mini-Module Facility
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill (incorporating some original ideas by 
;;;                          Kevin Gallagher and Zachary Rubinstein)
;;;
;;; Copyright (C) 2002-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;
;;;    The directory probing function, probe-directory, that is defined in
;;;    this file must be extended when porting to a new CL implementation.
;;;
;;; --------------------------------------------------------------------------
;;;
;;;  This mini-module facility provides a lightweight and easy to use
;;;  mechanism for maintaining (compiling and loading) module files.  The
;;;  mini-module facility is sufficient for many situations, and if not, there
;;;  are more complex open-source defsystem packages (such as ASDF) that are
;;;  available.
;;;
;;;  This file assumes the global variables *compiled-directory-name*,
;;;  *compiled-file-type*, and *project-root-pathname* have been defined by
;;;  loading mini-module-loader.lisp.
;;;
;;;  The mini-module-faclity supports the following directory layout:
;;;
;;;                          *project-root-pathname*
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
;;;  This file can be used as a stand-alone utility (when loaded by its
;;;  companion file, mini-module-loader.lisp).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-13-02 File created.  (Corkill)
;;;  01-12-04 Added :create-dirs option to compile-module.  (Corkill)
;;;  01-29-04 Exported module-loaded-p.  (Corkill)
;;;  02-01-04 Support use of existing root-directory in 
;;;           define-root-directory.  (Corkill)
;;;  03-19-04 Added port-needed error function.  (Corkill)
;;;  03-19-04 Added top-level mini-module commands for Lispworks.  (Corkill)
;;;  03-19-04 Added file-options checking.  (Corkill)
;;;  06-10-04 Added proper :forces-recompile date checking and warning
;;;           messages.  (Corkill)
;;;  06-11-04 Moved to separate package (for stand-alone use).  (Corkill)
;;;  08-10-04 Removed make-directory in favor of ensure-directories-exist.
;;;           (Corkill)
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
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :mini-module)
    (error "This file should be loaded using the file ~
            mini-module-loader.lisp")))
	 
(in-package :mini-module)

(flet ((check-var (var)
	 (unless (boundp var)
	   (error "~s is not defined.~
                   (This file should be loaded using the file ~
                    mini-module-loader.lisp)"
		  var))))
  (check-var '*compiled-directory-name*)
  (check-var '*compiled-file-type*)
  (check-var '*project-root-pathname*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*automatically-create-missing-directories*
	    common-lisp-user::*mini-module-compile-verbose*
	    common-lisp-user::*mini-module-load-verbose*)))

;; Not required; default is not to create missing directories automatically
;; (some CL implementations generate redefinition warnings when performing a
;; compile/load/compile bootstrap sequence, so we don't use defvar here):
(declaim (special *automatically-create-missing-directories*))
(unless (boundp '*automatically-create-missing-directories*)
  (setq *automatically-create-missing-directories* nil))

;;; ===========================================================================
;;;  Implementation-Specific Package & Feature Adjustments

;;; Use CMUCL package nicknames with SBCL:
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-impl::enter-new-nicknames (find-package "SB-PCL") '("PCL"))
  (sb-impl::enter-new-nicknames (find-package "SB-UNIX") '("UNIX")))

;;; ===========================================================================
;;;  Export user-level mini-module names.  (Some of these names could collide
;;;  with similar names in other packages, but we export them anyway.)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*month-preceeds-date*
	    compile-module
	    compute-relative-directory	; not documented
	    define-relative-directory
	    define-root-directory
	    define-module
	    describe-module
	    dotted-conc-name		; part of tools, but placed here
            get-directory               ; not yet documented
            list-modules                ; not yet documented
	    load-module
	    load-module-file
            ;; deprecated, remove soon!
	    loaded-modules              ; never documented
	    module-directories		; not yet documented
	    module-loaded-p
	    need-to-port		; not documented
	    port-needed			; not documented
	    brief-date-and-time		; part of tools, but placed here
	    show-defined-directories
	    show-modules                ; not yet documented
	    undefine-directory          ; not yet documented
	    undefine-module             ; not yet documented
            )))

;;; ===========================================================================
;;;  Dotted-conc-name

(defun dotted-conc-name (symbol)
  ;; Supports case-sensitive CLs
  (concatenate 'simple-string (symbol-name symbol) "."))

;;; ===========================================================================
;;;  Brief-date-and-time
;;;
;;;  Returns formatted date/time string (brief, Unix ls-like form)--actually
;;;  part of the GBBopen-tools module.  It is placed here to use it with the
;;;  mini-module package.
;;;
;;; ------------------------------------------------------------------------

(defvar *month-preceeds-date* 't)

(defvar *month-name-vector* 
    (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun"
	    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun brief-date-and-time (&optional time time-zone include-seconds)
  (let ((current-time (get-universal-time))
	time-difference)
    (if time
	(setq time-difference (abs (- current-time time)))
	(setq time current-time 
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
	    (if *month-preceeds-date*
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
	    (if *month-preceeds-date*	  
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
;;;  Port needed reporting

(defun port-needed (obj)
  (error "You must define ~s on ~a~@[ running on ~a~]."
         obj
         (lisp-implementation-type) 
         (machine-type))) 

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro need-to-port (obj)
    ;;; Used to generate compile-time porting errors!
    (port-needed obj)))

;;; ===========================================================================
;;;  Module Directories

(defstruct (mm-directory
            (:conc-name #.(dotted-conc-name 'mm-directory))
            (:copier nil))
  name)

(defstruct (mm-root-directory
            (:include mm-directory)
            (:conc-name #.(dotted-conc-name 'mm-root-directory))
            (:copier nil))
  path)

(defstruct (mm-relative-directory
            (:include mm-directory)
            (:conc-name #.(dotted-conc-name 'mm-relative-directory))
            (:copier nil))
  root
  sub-directories)

(defvar *mm-directories* (make-hash-table :test 'eq))

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
  #+(and cmu unix)
  (let ((dir (namestring 
              (make-pathname :name nil :type nil :defaults path))))
    (eq (unix::unix-file-kind dir) :directory))
  #+cormanlisp
  (cormanlisp:directory-p path)
  ;; Directory-pathname-p doesn't work for this on MCL, so we must use the
  ;; probe-file approach for now:
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
  #+openmcl
  (ccl:directory-pathname-p path)
  #+(and sbcl unix)
  (let ((dir (namestring 
              (make-pathname :name nil :type nil :defaults path))))
    (eq (sb-unix::unix-file-kind dir) :directory))
  #+(and scl unix)
  (ext:unix-namestring (make-pathname :name nil :type nil :version nil
				      :defaults path))
  #-(or allegro clisp (and cmu unix) cormanlisp digitool-mcl ecl gcl
	lispworks openmcl (and sbcl unix) (and scl unix))
  (need-to-port 'probe-directory))

;;; ---------------------------------------------------------------------------

(defun non-keyword-directory-name-error (name)
  (error "Directory name, ~s, must be a keyword." name))

;;; ---------------------------------------------------------------------------

(defun non-keyword-root/relative-directory-name-error (name)
  (error "Root or relative directory name, ~s, must be a keyword."
	 name))

;;; ---------------------------------------------------------------------------

(defun get-directory (name &rest subdirectories)
  ;; Get-directory is for direct directory specifications; there is no
  ;; source/compiled subtree handling
  (let ((mm-dir (gethash name *mm-directories*)))
    (typecase mm-dir
      (mm-root-directory 
       (let ((path (mm-root-directory.path mm-dir)))
         (if subdirectories
             (make-pathname 
              :directory (append (pathname-directory path)
                                 subdirectories)
              :defaults path)
             path)))
      (mm-relative-directory
       (let ((root-path (mm-root-directory.path
                         (gethash 
                          (mm-relative-directory.root mm-dir)
                          *mm-directories*))))
         (make-pathname 
          :directory (append (pathname-directory root-path)
                             (mm-relative-directory.sub-directories mm-dir)
                             subdirectories)
          :defaults root-path)))
      (otherwise (error "Directory ~s is not defined." name)))))

;;; ---------------------------------------------------------------------------

(defun get-mm-root-directory (name)
  (let ((mm-dir (gethash name *mm-directories*)))
    (typecase mm-dir
      (mm-root-directory mm-dir)
      (t (error "Root directory ~s is not defined." name)))))

;;; ---------------------------------------------------------------------------

(defun compute-root-directory (spec)
  (flet ((compute-it (spec)
           (etypecase spec
             (pathname (make-pathname :name nil :type nil :defaults spec))
             (string (pathname spec))
             (mm-root-directory (mm-root-directory.path spec)))))
    (typecase spec
      (symbol (compute-it
               (if (keywordp spec)
                   (get-mm-root-directory spec)
                   (symbol-value spec))))
      (otherwise (compute-it spec)))))

;;; ---------------------------------------------------------------------------

(defun define-root-directory (name spec)
  (unless (keywordp name)
    (non-keyword-directory-name-error name))
  (let ((root-directory-path (compute-root-directory spec)))
    (setf (gethash name *mm-directories*)
          (make-mm-root-directory
           :name name
           :path root-directory-path))))

;;; ---------------------------------------------------------------------------

(defun define-relative-directory (name root &rest sub-directories)
  (unless (keywordp name)
    (non-keyword-directory-name-error name))
  (unless (keywordp root)
    (non-keyword-root/relative-directory-name-error root))
  (setf (gethash name *mm-directories*)
        (make-mm-relative-directory
         :name name
         :root (or root
		   (let ((truename *load-truename*))
		     (if truename
			 (make-pathname
			  :name nil
			  :type nil
			  :defaults truename)
			 (error "A ~s-relative ~s root-directory ~
                                 specification cannot be evaluated outside ~
                                 of a load context."
				'*load-truename*
				'define-relative-directory))))
         :sub-directories sub-directories)))

;;; ---------------------------------------------------------------------------

(defun compute-relative-directory (name sub-directories compiled?)
  (cond
   ((null name) nil)
   ;; `Name' can be a pathname if a *load-truename*-relative :directory
   ;; option was used in define-module:
   ((pathnamep name)
    (make-pathname 
     :directory
     (append (pathname-directory name)
             (list (if compiled?
                       *compiled-directory-name*
                       *source-directory-name*))
             sub-directories)
     :defaults name))      
   (t (let ((mm-dir (gethash name *mm-directories*)))
        (typecase mm-dir
          (mm-relative-directory
           (compute-relative-directory
            (mm-relative-directory.root mm-dir)
            (append (mm-relative-directory.sub-directories mm-dir)
                    sub-directories)
            compiled?))
          (mm-root-directory
           (let ((root-path (mm-root-directory.path mm-dir)))
             (make-pathname 
              :directory
              (append (pathname-directory root-path)
                      (list (if compiled?
                                *compiled-directory-name*
                                *source-directory-name*))
                      sub-directories)
              :defaults root-path)))
          (otherwise
           (error "Directory ~s is not defined." name)))))))
           
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
            (t (format t "~&~s~%~4tRelative to ~(~s~)~
                          ~%~4tSub-directories: ~s"
                       (mm-directory.name directory)
                       (mm-relative-directory.root directory)
                       (mm-relative-directory.sub-directories directory)))))
        (terpri))))
  (terpri)
  (values))

;;; ===========================================================================
;;;  Modules

(defstruct (mm-module
            (:conc-name #.(dotted-conc-name 'mm-module))
            (:copier nil))
  name
  (directory nil)
  (sub-directories)
  (requires nil)
  (files nil)
  (files-loaded nil)
  (load-completed? nil)
  (latest-forces-recompiled-date 0))

(defvar *mm-modules* (make-hash-table))

;;; ---------------------------------------------------------------------------

(defmacro define-module (name &body args)
  (unless (keywordp name)
    (error "Module name, ~s, must be a keyword." name))
  (let ((directory nil)
        (directory-seen? nil)
        (sub-directories nil)
        (requires nil)
        (requires-seen? nil)
        (files nil)
        (files-seen? nil))
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
         (setq directory-seen? 't)
         (setq directory (second option))
         (setq sub-directories (cddr option))
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
         (setq files-seen? 't)
         (setq files (rest option)))
        (:requires 
         (when requires-seen?
           (error "Multiple :requires options supplied in module ~s."
                  name))
         (setq requires-seen? 't)
         (setq requires (rest option)))
        (t (error "Unsupported option, ~s, in module ~s."
                  option name))))    
    (when (and files (not directory))
      (let ((truename *load-truename*))
	(if truename
	    (setq directory 
	      (make-pathname
	       :name nil
	       :type nil
	       :defaults truename))
	    (error "A ~s-relative :directory specification to ~s cannot ~
                    be evaluated outside of a load context."
		   '*load-truename*
		   'define-module))))
    `(ensure-module ',name ',directory ',sub-directories ',files ',requires)))

;;; ---------------------------------------------------------------------------

(defun get-module (name)
  (or (gethash name *mm-modules*)
      (error "Module ~s is not defined." name)))

;;; ---------------------------------------------------------------------------

(defun determine-modules (module-names &aux result)
  (labels ((maybe-add-module (name)
             (let ((module (get-module name)))
               (dolist (name (mm-module.requires module))
                 (maybe-add-module name))
               (pushnew module result :test #'eq :key #'mm-module.name))))
    (dolist (name module-names)
      (maybe-add-module name)))
  ;; Maintain precedence order...
  (nreverse result))

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
		   (setq pos item-pos)))))))))

;;; ---------------------------------------------------------------------------

(defun check-requires-ordering (new-module-name new-module-requires)
  ;; Require `new-module-requires' to expand into a complete module
  ;; :requires list that is compatible with all existing module definitions.
  ;; This requirement ensures that module files will not be recompiled
  ;; solely due to a different relative ordering among defined modules.
  (let ((new-requires-list (determine-modules new-module-requires)))
    (maphash
     #'(lambda (name module)
	 (unless (eq name new-module-name)
	   (let ((requires-list (determine-modules
				 (mm-module.requires module))))
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

(defun ensure-module (name directory sub-directories files requires)
  (let ((existing-module (gethash name *mm-modules*)))
    (check-requires-ordering name requires)
    (setf (gethash name *mm-modules*)
          (make-mm-module 
           :name name 
           :directory directory
           :sub-directories sub-directories
           :files files 
           :files-loaded 
	     (when (and existing-module
			;; if the files specification has changed at all,
			;; reload them all...
			(equal files (mm-module.files existing-module)))
	       (mm-module.files-loaded existing-module))
	   :requires requires)))
  ;; Return the module name (returned by define-module):
  name)

;;; ---------------------------------------------------------------------------

(defun undefine-module (name)
  (get-module name)                     ; check that it is defined.
  (remhash name *mm-modules*))

;;; ---------------------------------------------------------------------------

(defun unload-modules ()
  ;; Used by :compile-gbbopen module to indicate no files have been loaded:
  (maphash 
   #'(lambda (name module)
       (declare (ignore name))
       (setf (mm-module.load-completed? module) nil)
       (setf (mm-module.files-loaded module) nil))
   *mm-modules*))

;;; ===========================================================================
;;;   Module compile/load functions

;; Dynamic binding used in support of :forces-recompile file option:
(defvar *latest-forces-recompile-date*)

;;; ---------------------------------------------------------------------------

(defparameter *compile/load-module-options*
    '(:create-dirs :propagate :recompile :reload :source))

;;; ---------------------------------------------------------------------------

(defparameter *compile/load-file-options*
    '(:recompile :reload :source :forces-recompile :noload))

;;; ---------------------------------------------------------------------------

(defun module-source/compiled-directories (module)
  (let* ((directory (mm-module.directory module))
         (sub-directories (mm-module.sub-directories module))
         (source-directory
          (compute-relative-directory directory sub-directories nil))
         (compiled-directory
          (compute-relative-directory directory sub-directories 't)))
    (values source-directory compiled-directory)))

;;; ---------------------------------------------------------------------------

(defun maybe-update-forces-recompile-date (new-date)
  (when (> new-date *latest-forces-recompile-date*)
    (setq *latest-forces-recompile-date* new-date)))

;;; ---------------------------------------------------------------------------

(defun load-file (path)
  ;; Generate our own load-verbose message:
  (when (and (not *load-verbose*)
	     *mini-module-load-verbose*)
    (format t "~&;;; loading file ~a...~%"
	    (namestring path)))
  (load path))

;;; ---------------------------------------------------------------------------

(defun compile/load-module-files-helper (module source-directory
                                         compiled-directory compile?
                                         recompile? reload? source? verbose?
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
			   (member :reload file-options :test #'eq)
			   (not file-loaded-acons)
			   (> date (cdr file-loaded-acons)))
                   (load-file path)
		   (when (member :forces-recompile file-options :test #'eq)
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
			    (not (member :source file-options :test #'eq)))
		   (format t "~&; File ~a in ~s needs to be recompiled.~%"
			   file-name (mm-module.name module)))))
	  (when (and (not (member :source file-options :test #'eq))
                     (or recompile? 
                         (member :recompile file-options :test #'eq)
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
                          :print verbose?
                          :output-file compiled-path)
            (setq compiled-file-date 
              (or (and (probe-file compiled-path)
		       (file-write-date compiled-path))
                  ;; Compiled file can be missing if compilation was
                  ;; aborted:
                  -1))
            (when (member :forces-recompile file-options :test #'eq)
	      (maybe-update-forces-recompile-date compiled-file-date)
	      (setf (mm-module.latest-forces-recompiled-date module)
		    (max compiled-file-date
			 (mm-module.latest-forces-recompiled-date module)))
              (setq recompile? 't propagate? 't)))
          (unless (member :noload file-options :test #'eq)
            (if (or source? (> source-file-date compiled-file-date))
                (load-it source-path source-file-date)
                (load-it compiled-path compiled-file-date)))))))
  (setf (mm-module.load-completed? module) 't)
  (maybe-update-forces-recompile-date 
   (mm-module.latest-forces-recompiled-date module))
  ;; return recompile? & propagate? values to use with remaining modules:
  (values recompile? propagate?))

;;; ---------------------------------------------------------------------------

(defun compile-module-files (module recompile? reload? source? verbose? 
                             propagate?)
  (multiple-value-bind (source-directory compiled-directory)
      (module-source/compiled-directories module)
    (when compiled-directory
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
    ;; Pass through recompile? and propagate? values
    (compile/load-module-files-helper 
     module source-directory compiled-directory
     t recompile? reload? source? verbose? propagate?)))

;;; ---------------------------------------------------------------------------

(defun load-module-files (module reload? source?)
  (multiple-value-bind (source-directory compiled-directory)
      (module-source/compiled-directories module)
    (compile/load-module-files-helper 
     module source-directory compiled-directory
     nil nil reload? source? nil nil)))

;;; ---------------------------------------------------------------------------

(defun compile-module (module-names &rest options) 
  ;;; Compiles and loads a module.  Each compiled file is loaded immediately
  ;;; after compilation (unless :source is specified).  Options are keyword
  ;;; flags (not keyword-value pairs):
  ;;;
  ;;; Options:
  ;;;   :recompile   Compiles even if the compiled file is newer than the
  ;;;                source file
  ;;;   :reload      Loads even if already loaded
  ;;;   :source      Loads source even if the file is compiled
  ;;;                (implies :reload)
  ;;;   :propagate   Applies the specified options to all required modules
  ;;;   :create-dirs Creates directories that are missing in the
  ;;;                compiled-file tree
  (declare (dynamic-extent options))
  #+ecl (progn (print "START")
               (compile nil (lambda () nil))
               (print "DONE"))
  (when (keywordp module-names) (setq module-names (list module-names)))
  (dolist (option options)
    (unless (member option *compile/load-module-options* :test #'eq)
      (warn "Unrecognized compile-module option ~s, ignored." option)))
  (let* ((recompile? (member :recompile options :test #'eq))
         (reload? (member :reload options :test #'eq))
         (propagate? (member :propagate options :test #'eq))
         (source? (member :source options :test #'eq))
         (verbose? (member :verbose options :test #'eq))
         (*automatically-create-missing-directories*
          (or *automatically-create-missing-directories*
              (member :create-dirs options :test #'eq)
              ;; For those who hate abbreviations:
              (member :create-directories options :test #'eq)))
         (modules-to-load (determine-modules module-names))
	 (*latest-forces-recompile-date* 0))
    ;; specifying :source implies :reload
    (when source? (setq reload? 't))        
    (dolist (module modules-to-load)
      (let ((specified-module? 
             (member (mm-module.name module) module-names :test #'eq)))
        (if (or propagate? specified-module?)              
            (multiple-value-setq (recompile? propagate?)
              (compile-module-files module recompile? reload? source? 
                                    verbose? propagate?))
            (load-module-files
             module 
             (and reload? propagate?) 
             (and source? (or propagate? specified-module?))))))))

;;; ---------------------------------------------------------------------------

(defun load-module (module-names &rest options)
  ;;; Loads a module.  By default, loads the newest of the source or compiled
  ;;; version of each file.  Options are keyword flags (not keyword-value
  ;;; pairs):
  ;;;
  ;;; Options:
  ;;;   :recompile   Ignored by load-module
  ;;;   :reload      Loads even if already loaded
  ;;;   :source      Loads source (implies :reload)
  ;;;   :propagate   Applies the specified options to all required modules
  (declare (dynamic-extent options))
  (when (keywordp module-names) (setq module-names (list module-names)))
  (dolist (option options)
    (unless (member option *compile/load-module-options* :test #'eq)
      (warn "Unrecognized compile-module option ~s, ignored." option)))
  (let ((reload? (member :reload options :test #'eq))
        (propagate? (member :propagate options :test #'eq))
        (source? (member :source options :test #'eq))
        (modules-to-load (determine-modules module-names))
	(*latest-forces-recompile-date* 0))
  ;; specifying :source implies :reload:
    (when source? (setq reload? 't))        
    (dolist (module modules-to-load)
      (let ((specified-module? 
             (member (mm-module.name module) module-names :test #'eq)))
        (load-module-files 
         module 
         (and reload? 
              (or propagate? specified-module?))
         (and source? 
              (or propagate? specified-module?)))))))

;;; ---------------------------------------------------------------------------

(defun load-module-file (module-name file-name &rest file-options)
  ;;; Specified loading of a single file in a module.  Always reloads
  ;;; the latest source/compiled file, but file-options allows :source
  ;;; only
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
                                       :test #'string=)))
	(flet ((load-it (path date)
		 (load-file path)
		 (if file-loaded-acons
		     ;; update the date in the existing acons:
		     (setf (cdr file-loaded-acons) date)
		     ;; add file and date as a new acons in files-loaded:
		     (setf (mm-module.files-loaded module)
			   (acons file-name date files-loaded)))
		 ;; Return the file path:
		 path))
	  (if (or (member :source file-options :test #'eq)
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

;; Deprecated, remove soon:
(defun loaded-modules ()
  (sort (list-modules) #'string-lessp))

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
     ((member :unspecific)		; Added by DDC
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
		      (determine-modules (mm-module.requires module)))
	      (namestring source-directory)
	      (namestring compiled-directory)
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
            (format t "~%~s~:[~; [~a]~]"
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
;;;  Define Basic Directories and Modules (as specified in startup.lisp)

(define-root-directory :project-root *project-root-pathname*)

;;; ---------------------------------------------------------------------------

(define-relative-directory :mini-module :project-root "mini-module")

;;; ---------------------------------------------------------------------------

(define-module :mini-module
  (:directory :mini-module)
  (:files "mini-module"))

;;; ---------------------------------------------------------------------------
;;;  Record this file as loaded in the mini-module hash table (due to bootstrap
;;;  loading)

(let* ((mini-module (gethash :mini-module *mm-modules*))
       (this-file (or *load-truename*
		      ;; CormanLisp doesn't bind *load-truename* properly 
		      ;; during bootstrapping, so we hardcode the filename
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
;;;  Top-Level Mini-Module Command Support 
;;;  (requires gbbopen-top-level-commands)

(defvar *last-lm/cm-module* nil)
(defvar *last-lm-options* nil)
(defvar *last-cm-options* nil)

(defun do-mini-module-tll-command (cmd fn options save-symbol)
  (let ((recalled-options nil))
    (when (and (null options) *last-lm/cm-module*)
      (setq recalled-options 't)
      (setq options (cons *last-lm/cm-module*
			  (symbol-value save-symbol))))
    (cond 
     ;; New module arguments were specified:
     (options
      (setq *last-lm/cm-module* (first options))
      (setf (symbol-value save-symbol) (rest options))
      (when recalled-options
	(format *trace-output* "~&;; ~(~s~)~{ ~(~s~)~}~%"
		cmd options))
      (apply fn options))
     ;; Recall previous module arguments:
     (t (format *trace-output* 
		"~&;; ~(~s~) -- No previous module specified."
		cmd)))))

(defun lm-tll-command (options)
  (do-mini-module-tll-command :lm #'load-module options '*last-lm-options*))

(defun cm-tll-command (options)
  (do-mini-module-tll-command :cm #'compile-module options '*last-cm-options*))

;;; ===========================================================================
;;;   Mini-module is fully loaded

(pushnew :mini-module *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


