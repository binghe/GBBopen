;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/gbbopen-modules-directory.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Mar 25 11:30:07 2008 *-*
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
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :common-lisp-user)

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

;;; ---------------------------------------------------------------------------

(defun process-gbbopen-modules-directory (filename)
  (let* ((user-homedir-pathname
	  ;; CMUCL uses a search list in (user-homedir-pathname), so we must
	  ;; fixate it using truename before proceeding:
	  (make-pathname :defaults (truename (user-homedir-pathname))))
	 (user-modules-dir
	  (make-pathname
	   :directory (append (pathname-directory
			       user-homedir-pathname)
			      '("gbbopen-modules"))
	   :defaults user-homedir-pathname))
	 (subdirs-pathname
	  #-(or clozure openmcl)
	  (make-pathname
	   :directory (append (pathname-directory user-modules-dir)
			      #-(or allegro cmu scl)
			      '(:wild))
	   :defaults user-modules-dir)
	  #+(or clozure openmcl)
	  (make-pathname
	   :name ':wild
	   :defaults user-modules-dir))
	 (pseudo-sym-link-paths
	  (directory 
	   (make-pathname
	    :name ':wild
	    :type "sym"
	    :defaults user-modules-dir)))
	 (module-dirs
	  (append
	   #-(or clozure openmcl)
           (directory subdirs-pathname)
	   #+(or clozure openmcl)
           (directory subdirs-pathname :directories 't)
           ;; Add in any *.sym file "pseudo" symbolic links:
	   (mapcan 'read-target-directory-specification
		   pseudo-sym-link-paths))))
    (when module-dirs
      (format t "~&;; Loading ~a from ~a...~%" 
	      (cond ((string= filename "modules")
		     "module definitions")
		    ((string= filename "commands")
		     "module command definitions")
		    (t filename))
	      (namestring user-modules-dir))
      (dolist (dir module-dirs)
	(let ((pathname-name (pathname-name dir)))
	  (load (namestring 
		 (make-pathname 
		  :name filename
		  :type "lisp"
		  :directory (if (and pathname-name
				      (not (eq pathname-name ':unspecific)))
				 `(,@(pathname-directory dir)
				     ,(pathname-name dir))
				 (pathname-directory dir))
		  :defaults dir))
		:if-does-not-exist nil))))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
