;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/extended-repl.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Apr 17 04:47:32 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    Extended REPL Command Processing
;;;; * for CLISP, CMUCL, SCL, ECL, and SBCL REPL and for SLIME (Emacs->Swank)
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill 
;;;
;;; Copyright (C) 2005-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;
;;;   These extensions add:
;;;    - keyword-command capabilities to the REPL in CLISP, CMUCL, SCL,
;;;      and SBCL
;;;    - interface into CLISP's *user-commands* facility
;;;    - extend ECL's command repertoire
;;;    - add keyword-command support to SLIME's Emacs->Swank interface  
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-04-05 File Created.  (Corkill)
;;;  02-02-06 Added ECL support.  (Corkill)
;;;  02-04-06 Added SLIME (Emacs->Swank) support.  (Corkill)
;;;  04-17-08 Reworked SLIME mechanism.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	 
(in-package :common-lisp-user)

(defvar *extended-repl-commands* nil)

;;; ---------------------------------------------------------------------------
;;;  In ECL, setup an Extended REPL command topic (must be done destructively,
;;;  as si::*tpl-commands* is already lexically bound in the default TLP)

#+ecl
(setf (rest si::*tpl-commands*)
      (adjoin (list "Extended REPL Commands") (cdr si::*tpl-commands*)
	      :key #'car :test #'equal))

;;; ---------------------------------------------------------------------------

(defun get-extended-repl-command (command)
  (assoc command *extended-repl-commands* :test #'eq))

;;; ---------------------------------------------------------------------------

(defmacro define-extended-repl-command (command lambda-list &body body)
  (let ((tlc-sym (gensym))
	(maybe-doc (first body)))
    `(progn
       ;; Define the command function:
       (defun ,tlc-sym ,lambda-list ,@body)
       ;; Always add command to *extended-repl-commands* (for SLIME interface
       ;; and more):
       (pushnew '(,command ,tlc-sym ,(when (stringp maybe-doc) maybe-doc))
		*extended-repl-commands*
		:test #'eq
		:key #'car)
       ;; Add to the CL implemention's top-level, where possible:
       #+allegro
       (unless (top-level::find-command-or-alias ,command :quiet t)
	 (top-level:alias ,(string-downcase (symbol-name command))
	     ,lambda-list ,@body))
       #+lispworks
       (system::define-top-loop-command 
	   ,command
	   ,lambda-list
	 (progn ,@body))
       #+openmcl
       (ccl::define-toplevel-command :global ,command ,lambda-list
				     ,@body)
       #+ecl
       (progn
	 ;; Extend ECL's commands for ECL TPL use:
	 (pushnew '((,command) ,tlc-sym :eval
		    ,@(if (stringp maybe-doc)
			  (list (format nil "~s~16,8t~a"
				 command
				 maybe-doc)
			   maybe-doc)
			  '("" "")))
		  ;; Place in the extended REPL topic area:
		  (cdr (second si::*tpl-commands*))
		  :test #'equal
		  :key #'car)))))

;;; ---------------------------------------------------------------------------
;;;  Interface into CLISP's *user-commands* facility
;;;
;;; Currently there is no way to read remaining values from the RELP read-line
;;; string in CLISP.

#+clisp
(defun user-commands ()
  (mapcan 
   #'(lambda (command-spec)
       (destructuring-bind (command function &optional doc)
	   command-spec
	 (let ((command-name (concatenate 'string 
			       ":" 
			       (string-downcase (symbol-name command)))))
	   (list 
	    ;; the command
	    `(,command-name . ,function)
	    ;; command documentation:
	    (format nil "~%~a~20t~a" command-name doc)))))
   *extended-repl-commands*))

#+clisp
(pushnew #'user-commands custom:*user-commands*)

;;; ---------------------------------------------------------------------------
;;; CMUCL doesn't provide an extension hook in either %top-level or
;;; interactive-eval.  So, we resort to shadowing the original
;;; interactive-eval function:

#+cmu
(setf (fdefinition 'original-interactive-eval)
      (fdefinition 'ext:interactive-eval))
 
#+cmu
(lisp::without-package-locks  
 (defun ext:interactive-eval (form)
   (let ((in *standard-input*)
	 (repl-command (get-extended-repl-command form)))
     (flet ((do-command (symbol-or-fn args)
	      (apply (the function (if (symbolp symbol-or-fn) 
				       (fdefinition symbol-or-fn)
				       symbol-or-fn))
		     args)
	      ;; always return nil
	      nil))
       (cond (repl-command
	      (let ((forms nil))
		(loop 
		  (unless (ext:listen-skip-whitespace in)
		    (return))
		  (push (read in nil) forms))
		(do-command (second repl-command)
		  (nreverse forms)))
	      (values))
	     ;; Support (<command> <arg>*) syntax as well:
	     ((and (consp form)
		   (setf repl-command (get-extended-repl-command (car form))))
	      (do-command (second repl-command) (cdr form))
	      (values))
	     (t (original-interactive-eval form))))))
 
 (compile 'ext:interactive-eval))

;;; ---------------------------------------------------------------------------
;;; The Scieneer CL 1.3 doesn't provide an extension hook in either %top-level
;;; or interactive-eval.  So, we resort to shadowing the original
;;; interactive-eval function:

#+scl
(setf (fdefinition 'original-interactive-eval)
      (fdefinition 'ext:interactive-eval))
 
#+scl
(defun ext:interactive-eval (form)
  (let ((in *standard-input*)
	(repl-command (get-extended-repl-command form)))
    (flet ((do-command (symbol-or-fn args)
	     (apply (the function (if (symbolp symbol-or-fn) 
				      (fdefinition symbol-or-fn)
				      symbol-or-fn))
		    args)
	     ;; always return nil
	     nil))
      (cond (repl-command
	     (let ((forms nil))
	       (loop 
		(unless (ext:listen-skip-whitespace in)
		  (return))
		(push (read in nil) forms))
	       (do-command (second repl-command)
		 (nreverse forms)))
	     (values))
	    ;; Support (<command> <arg>*) syntax as well:
	    ((and (consp form)
		  (setf repl-command (get-extended-repl-command (car form))))
	     (do-command (second repl-command) (cdr form))
	     (values))
	    (t (original-interactive-eval form))))))

#+scl
(compile 'ext:interactive-eval)

;;; ---------------------------------------------------------------------------
;;; Extend SBCL's default form reader with a richer version that provides
;;; keyword-command capabilities:

#+sbcl
(progn
  (defun extended-repl-read-form-fun (in out)
  (declare (type stream in out) (ignore out))
  (let* ((eof-marker (cons nil nil))
	 (form (read in nil eof-marker)))
    (cond 
     ((eq form eof-marker) (quit))
     (t (let ((repl-command (get-extended-repl-command form)))
	  (flet ((do-command (symbol-or-fn args)
		   (apply (the function (if (symbolp symbol-or-fn) 
					    (fdefinition symbol-or-fn)
					    symbol-or-fn))
			  args)
                   ;; always return nil
		   nil))
	    (cond
             (repl-command
              (let ((forms nil))
                (loop 
                  (unless (sb-int:listen-skip-whitespace in)
                    (return))
                  (push (read in nil) forms))
                (do-command (second repl-command) (nreverse forms)))
              '(values))
             ;; Support (<command> <arg>*) syntax as well:
             ((and (consp form)
                   (setf repl-command (get-extended-repl-command (car form))))
              (do-command (second repl-command) (cdr form))
              '(values))
             (t form))))))))
  (compile 'extended-repl-read-form-fun)
  (setf sb-int:*repl-read-form-fun* #'extended-repl-read-form-fun))

;;; ===========================================================================
;;;  SLIME REPL Interface Setup
;;;
;;;  SLIME doesn't provide an easy means of using its *after-init-hook*
;;;  mechanism in advance of loading Swank.  We work around this in the
;;;  current SLIME by creating the :swank-backend package (which is used by
;;;  Swank) and exporting swank-backend:*after-init-hook*, which we have set.
;;;  It would have been much easier if Swank simply imported a better-named
;;;  variable (such as *after-swank-init-hook*) from the :cl-user package,
;;;  allowing straightforward advanced adding of after-init hooks...

(unless (find-package ':swank-backend)
  (defpackage :swank-backend
    (:use :common-lisp)
    (:export #:*after-init-hook*)))

(defvar *slime-extended-repl-file*
    (make-pathname :name "slime-extended-repl"
                   :type "lisp"
                   :defaults *load-truename*))

(defun load-slime-extended-repl ()
  (format t "~&;; Loading extended REPL command processing for SLIME...~%")
  (load *slime-extended-repl-file*))  

(defvar swank-backend:*after-init-hook* nil
  "Hook run after user init files are loaded.")

(pushnew 'load-slime-extended-repl swank-backend:*after-init-hook*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

