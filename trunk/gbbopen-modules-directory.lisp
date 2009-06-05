;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/gbbopen-modules-directory.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jun  5 13:56:28 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                GBBopen-Modules Directory Processing
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;;   This file is loaded by initiate.lisp (and optionally by startup.lisp)
;;;   to process personal gbbopen-modules directory items:
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  12-08-02 Split out from startup.lisp and initiate.lisp.  (Corkill)
;;;  06-20-06 Add "pseudo symbolic-link" support (for operating systems that
;;;           do not provide symbolic links).  (Corkill)
;;;  03-29-08 Add PROCESS-GBBOPEN-MODULES-DIRECTORY rescanning.  (Corkill)
;;;  06-01-09 Add *IGNORED-GBBOPEN-MODULES-DIRECTORY-SUBDIRECTORIES* parameter. 
;;;           (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

#+debugging
(load (make-pathname :name "printv" :defaults *load-truename*))

;;; ---------------------------------------------------------------------------

(defparameter *ignored-gbbopen-modules-directory-subdirectories*
    '(".svn"))

(defvar *loaded-gbbopen-modules-directory-files* nil)

;;; ---------------------------------------------------------------------------

(defun read-target-directory-specification (filename)
  ;;; Return the target-directory-specification (from a *.sym file) as a
  ;;; one-element list; or nil if none is found:
  (with-open-file (file filename)
    (let (line)
      (loop
	(setf line (ignore-errors (read-line file nil nil)))
	(unless line
          (format t "~&;; WARNING: Unable to read the target-directory file ~s~%" 
                (namestring filename))
	  (return-from read-target-directory-specification nil))
        (when (plusp (length line))
          (flet ((trim-spec (string)
                   (string-right-trim 
                    '(#\Space #\Tab #\Return #\Linefeed #\Newline #\Backspace #\Rubout)
                    string)))
            (case (aref line 0)
              ;; Skip comment lines:
              ((#\; #\#))
              ;; The target was specified as a string:
              (#\" (let ((string
                          (ignore-errors (read-from-string line nil nil))))
                     (return-from read-target-directory-specification 
                       (typecase string
                         ;; We got the expected string:
                         (string (list (trim-spec string)))
                         (otherwise 
                          (format t "~&;; WARNING: While reading from ~s: ~
                                          Unable to read target-directory ~
                                          specification \"~a\"~%" 
                                  (namestring file)
                                  line)
                          ;; return nil
                          nil)))))
              (otherwise 
               (return-from read-target-directory-specification 
                 (list (trim-spec line)))))))))))
  
(compile-if-advantageous 'read-target-directory-specification)
  
;;; ---------------------------------------------------------------------------
                  
(defun process-the-gbbopen-modules-directory (modules-dir filename shared?)
  ;;; Common function to process GBBopen's shared-gbbopen-modules directory
  ;;; and a user's personal gbbopen-modules directory.
  #+(and sbcl (not sb-unicode))
  (declare (type simple-base-string filename))
  (let* ((subdirs-pathname
	  #-(or clozure lispworks)
          (make-pathname
	   :directory (append (pathname-directory modules-dir)
			      #-(or allegro cmu scl)
			      '(:wild))
	   :defaults modules-dir)
          #+(or clozure lispworks)
          (make-pathname
           :name ':wild
	   :defaults modules-dir))
	 (pseudo-sym-link-paths
	  (directory 
	   (make-pathname
	    :name ':wild
	    :type "sym"
	    :defaults modules-dir)))
	 (module-dirs
	  (append
           #+allegro
           (directory subdirs-pathname :directories-are-files nil)
	   #+clozure
           (directory subdirs-pathname :directories 't)
	   #-(or allegro clozure)
           (directory subdirs-pathname)
           ;; Add in any *.sym file "pseudo" symbolic links:
	   (mapcan 'read-target-directory-specification
		   pseudo-sym-link-paths)))
         (now (get-universal-time)))
    (let ((message-printed? nil)
          (load-type (cond ((string= filename "modules")
                            "module definitions")
                           ((string= filename "commands")
                            "command definitions")
                           (t filename))))
      (dolist (dir module-dirs)
        (let* ((pathname-directory (pathname-directory dir))
               (dir-name (first (last pathname-directory))))
          (unless (member dir-name 
                          *ignored-gbbopen-modules-directory-subdirectories*
                          :test #'string-equal)
            (let* ((pathname (make-pathname 
                              :name filename
                              :type "lisp"
                              :directory pathname-directory
                              :defaults dir))
                   (previous-load-time-acons 
                    (assoc pathname *loaded-gbbopen-modules-directory-files*
                           :test #'equal))
                   (previous-load-time (cdr previous-load-time-acons)))
              (unless (and previous-load-time
                           (let ((file-write-date (file-write-date pathname)))
                             (and file-write-date
                                  (locally ; avoid SBCL optimization warning: 
                                      (declare (optimize (speed 1)))
                                    (not (> file-write-date 
                                            previous-load-time))))))
                (cond
                 ;; No load-type file in this directory:
                 ((not (probe-file pathname))
                  (format t "~&;; WARNING: Unable to find ~
                                      ~:[personal~;shared~] ~a file ~a~%" 
                          shared?
                          load-type
                          (namestring pathname)))
                 ((not message-printed?)
                  (format t "~&;; Loading ~:[personal~;shared~] ~a from ~
                                      ~a...~%" 
                          shared?
                          load-type
                          (namestring modules-dir))
                  (setf message-printed? 't)))
                (when (load (namestring pathname)
                            :if-does-not-exist nil)
                  (cond 
                   (previous-load-time-acons
                    (setf (cdr previous-load-time-acons) now))
                   (t (setf *loaded-gbbopen-modules-directory-files*
                            (acons 
                             pathname now
                             *loaded-gbbopen-modules-directory-files*))))))))))
      (unless (or message-printed? *gbbopen-startup-loaded*)
        (format t "~&;; No ~:[personal~;shared~] ~a were found in ~a.~%"
                shared?
                load-type 
                (namestring modules-dir))))))

(compile-if-advantageous 'process-the-gbbopen-modules-directory)

;;; ---------------------------------------------------------------------------

(defun process-shared-gbbopen-modules-directory (filename)
  (let ((shared-modules-dir
         (make-pathname
          :name nil
          :type nil
          :directory (append (pathname-directory *gbbopen-install-root*)
                             '("shared-gbbopen-modules"))
          :defaults *gbbopen-install-root*)))
      (process-the-gbbopen-modules-directory shared-modules-dir filename 't)))

(compile-if-advantageous 'process-shared-gbbopen-modules-directory)

;;; ---------------------------------------------------------------------------

(defun process-gbbopen-modules-directory (filename)
  (let* ((user-homedir-pathname
	  ;; CMUCL uses a search list in (user-homedir-pathname), so we must
	  ;; fixate it using truename before proceeding:
	  (make-pathname :defaults (truename (user-homedir-pathname))))
	 (user-modules-dir
	  (make-pathname
	   :directory (append (pathname-directory user-homedir-pathname)
			      '("gbbopen-modules"))
	   :defaults user-homedir-pathname)))
    (process-the-gbbopen-modules-directory user-modules-dir filename nil)))

(compile-if-advantageous 'process-gbbopen-modules-directory)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
