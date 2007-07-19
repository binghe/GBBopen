;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/extended-repl.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jul 18 09:30:04 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    Extended REPL Command Processing
;;;; * for CLISP, CMUCL, SCL, ECL, and SBCL REPL and for SLIME (Emacs->swank)
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill 
;;;
;;; Copyright (C) 2005-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;
;;;   These extensions add:
;;;    - keyword-command capabilities to the REPL in CLISP, CMUCL, SCL,
;;;      and SBCL
;;;    - interface into CLISP's *user-commands* facility
;;;    - extend ECL's command repertoire
;;;    - add keyword-command support to SLIME's Emacs->swank interface  
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-04-05 File Created.  (Corkill)
;;;  02-02-06 Added ECL support.  (Corkill)
;;;  02-04-06 Added SLIME (Emacs->swank) support.  (Corkill)
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
;;;  SLIME Interface

(defpackage :swank
  (:use :common-lisp))

(defvar swank::*new-connection-hook*)
(defvar swank::*slime-repl-eval-hooks*)

;;; ---------------------------------------------------------------------------

(defun get-extended-repl-command-with-help (command)
  ;; Used in extended-repl-swank-eval-hook to add SLIME support for :help on
  ;; CLs that already provide their own REPL help command (and is therefore
  ;; not in *extended-repl-commands*)
  (or (assoc command *extended-repl-commands* :test #'eq)
      #+(or allegro ecl)
      (and (member command '(:help :h))
	   #+allegro
	   '(:help tpl::tpl-help-command)
	   #+ecl
	   '(:help si::tpl-help-command))))

;;; ---------------------------------------------------------------------------
;;; We use Swank's eval hook to process command forms coming over the
;;; Emacs->swank connection.

(defun extended-repl-swank-eval-hook (form)
  (let ((repl-command (get-extended-repl-command-with-help form)))
    (flet ((do-command (symbol-or-fn args)
	     (apply (the function (if (symbolp symbol-or-fn) 
				      (fdefinition symbol-or-fn)
				      symbol-or-fn))
		    args)
	     (values)))
      (cond (repl-command
	     (do-command (second repl-command) nil))
	    ;; Support (<command> <arg>*) syntax as well:
	    ((and (consp form)
		  (setf repl-command
                        (get-extended-repl-command-with-help (car form))))
	     (do-command (second repl-command) (cdr form)))
	    ;; Tell swank that we pass (normal eval):
	    (t (funcall 'swank::repl-eval-hook-pass))))))

;;; ---------------------------------------------------------------------------
;;; Swank can be loaded after user-initialization files, so we establish the
;;; extended REPL command processing every time an Emacs->swank connection is
;;; created.

(defun extended-repl-new-swank-connection-hook (connection)
  (declare (ignore connection))
  (when (boundp 'swank::*slime-repl-eval-hooks*)
    (format t "~&;; Adding extended REPL command processing to SLIME...~%")
    (pushnew 'extended-repl-swank-eval-hook swank::*slime-repl-eval-hooks*)))
  
;;; ---------------------------------------------------------------------------

(if (and (boundp 'swank::*new-connection-hook*)
	 swank::*new-connection-hook*)
    ;; We have an existing Emacs->swank connection, so run the hook:
    (extended-repl-new-swank-connection-hook nil)
    ;; Otherwise...
    (if (boundp 'swank::*new-connection-hook*)
	(pushnew 'extended-repl-new-swank-connection-hook
		 swank::*new-connection-hook*)
	(setf swank::*new-connection-hook* 
              (list 'extended-repl-new-swank-connection-hook))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


