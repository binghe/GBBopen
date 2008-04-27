;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/gbbopen-modules-directory.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr 27 12:45:01 2008 *-*
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
;;; Copyright (C) 2006-2008, Dan Corkill <corkill@GBBopen.org>
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
;;;  03-29-08 Add process-gbbopen-modules-directory rescanning.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

;;; ---------------------------------------------------------------------------

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
	  (return-from read-target-directory-specification nil))
	(locally (declare (simple-string line))
	  (setf line (string-trim '(#\Space #\Tab #\Newline) line))
	  (when (plusp (length line))
	    (case (aref line 0)
	      ;; Skip comment lines:
	      ((#\; #\#))
	      ;;
	      (#\" (let ((string
			  (ignore-errors (read-from-string line nil nil))))
		     (return-from read-target-directory-specification 
		       (typecase string
			 ;; We got the expected string:
			 (string (list string))
			 (otherwise 
			  (warn "While reading from ~s: Unable to read ~
                                 target-directory, ~a, " 
				(namestring file)
				line)
			  ;; return nil
			  nil)))))
	      (otherwise 
	       (return-from read-target-directory-specification 
		 (list line))))))))))

(compile-if-advantageous 'read-target-directory-specification)

;;; ---------------------------------------------------------------------------
                  
(defun process-the-gbbopen-modules-directory (modules-dir filename
                                              load-only-if-new)
  ;;; Common function to process GBBopen's shared-gbbopen-modules directory
  ;;; and a user's personal gbbopen-modules directory.
  (let* ((subdirs-pathname
	  #-clozure
	  (make-pathname
	   :directory (append (pathname-directory modules-dir)
			      #-(or allegro cmu scl)
			      '(:wild))
	   :defaults modules-dir)
	  #+clozure
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
	   #-clozure
           (directory subdirs-pathname)
	   #+clozure
           (directory subdirs-pathname :directories 't)
           ;; Add in any *.sym file "pseudo" symbolic links:
	   (mapcan 'read-target-directory-specification
		   pseudo-sym-link-paths)))
         (now (get-universal-time)))
    (when module-dirs 
      (unless load-only-if-new
        (format t "~&;; Loading ~a from ~a...~%" 
                (cond ((string= filename "modules")
                       "module definitions")
                      ((string= filename "commands")
                       "module command definitions")
                      (t filename))
                (namestring modules-dir)))
      (dolist (dir module-dirs)
	(let* ((dir-pathname-name (pathname-name dir))
               (pathname (make-pathname 
                          :name filename
                          :type "lisp"
                          :directory (if (and dir-pathname-name
                                              (not (eq dir-pathname-name 
                                                       ':unspecific)))
                                         `(,@(pathname-directory dir)
                                             ,(pathname-name dir))
                                         (pathname-directory dir))
                          :defaults dir))
               (previous-load-time-acons 
                (assoc pathname *loaded-gbbopen-modules-directory-files*
                       :test #'equal))
               (previous-load-time (cdr previous-load-time-acons)))
          (unless (and previous-load-time
                       (let ((file-write-date (file-write-date pathname)))
                         (and file-write-date
                              (locally  ; avoid SBCL optimization warning: 
                                  (declare (optimize (speed 1)))
                                (not (> file-write-date 
                                        previous-load-time))))))
            (when (load (namestring pathname)
                        :if-does-not-exist nil)
              (cond 
               (previous-load-time-acons
                (setf (cdr previous-load-time-acons) now))
               (t (setf *loaded-gbbopen-modules-directory-files*
                        (acons pathname
                               now
                               *loaded-gbbopen-modules-directory-files*)))))))
        ))))

(compile-if-advantageous 'process-the-gbbopen-modules-directory)

;;; ---------------------------------------------------------------------------

(let ((truename *load-truename*))
  (defun process-shared-gbbopen-modules-directory (filename
                                                 &optional load-only-if-new)
  (let ((shared-modules-dir
         (make-pathname
          :name nil
          :type nil
          :directory (append (pathname-directory truename)
                             '("shared-gbbopen-modules"))
          :defaults truename)))
    (process-the-gbbopen-modules-directory
     shared-modules-dir filename load-only-if-new))))

;; CMUCL and Lispworks can't compile the interpreted closure:
#-(or cmu lispworks)
(compile-if-advantageous 'process-shared-gbbopen-modules-directory)

;;; ---------------------------------------------------------------------------

(defun process-gbbopen-modules-directory (filename &optional load-only-if-new)
  (let* ((user-homedir-pathname
	  ;; CMUCL uses a search list in (user-homedir-pathname), so we must
	  ;; fixate it using truename before proceeding:
	  (make-pathname :defaults (truename (user-homedir-pathname))))
	 (user-modules-dir
	  (make-pathname
	   :directory (append (pathname-directory user-homedir-pathname)
			      '("gbbopen-modules"))
	   :defaults user-homedir-pathname)))
    (process-the-gbbopen-modules-directory
     user-modules-dir filename load-only-if-new)))

(compile-if-advantageous 'process-gbbopen-modules-directory)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
