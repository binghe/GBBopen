;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/extended-repl.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr 23 03:26:26 2008 *-*
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

(compile-if-advantageous 'get-extended-repl-command)

;;; ---------------------------------------------------------------------------
;;;  In Allegro, we hide non-native help commands by saving the command-name
;;;  strings in *non-native-help-commandss* and fwrapping
;;;  tpl::get-commands-list to remove the commands from its returned value.

#+allegro
(defvar *non-native-help-commands* nil)

#+allegro
(def-fwrapper filtered-get-commands-list-wrap ()
  ;; Remove non-native extended-REPL commands:
  (remove-if #'(lambda (command)
                 (member (first command) *non-native-help-commands*
                         :test #'equal))
             (call-next-fwrapper)))

#+allegro
(fwrap 'top-level::get-commands-list 'extended-repl 
       'filtered-get-commands-list-wrap)

;;; ---------------------------------------------------------------------------

(defmacro define-extended-repl-command (command lambda-list &body body)
  ;;; Internal macro used by initiate.lisp's define-ttl-command macro:
  (let ((tlc-sym (gensym))
	(maybe-doc (first body))
        (options nil))
    ;; Handle (<command> <option>*) syntax:
    (when (consp command)
      (setf options (rest command))
      (setf command (first command)))
    ;; Handle Allegro hidden-help:
    #+allegro
    (unless (member ':add-to-native-help options)
      (pushnew (string-downcase (symbol-name command))
               *non-native-help-commands* :test #'equal))
    ;; Now do the REPL-command definition:
    `(progn
       ;; Define the command function:
       (defun ,tlc-sym ,lambda-list 
         ,@body)
       ;; Always add command to *extended-repl-commands* (for SLIME interface
       ;; and more):
       (pushnew '(,command ,tlc-sym 
                  ;; documentation string:
                  ,(when (stringp maybe-doc) maybe-doc)
                  ;; Help control:
                  ,(or
                    ;; no help:
                    (when (member ':no-help options) 
                         ':no-help)
                    ;; add to native help:
                    (when (member ':add-to-native-help options) 
                         ':add-to-native-help)))
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

(compile-if-advantageous 'define-extended-repl-command)

;;; ---------------------------------------------------------------------------

(defun sorted/filtered-extended-repl-commands ()
  (sort (remove-if
         #'(lambda (command-spec)
             (destructuring-bind (command function doc 
                                  help-control)
                 command-spec
               (declare (ignore command function doc))
               (eq help-control ':no-help)))
         *extended-repl-commands*)
        #'(lambda (a b)
            (string< 
             (the simple-base-string (symbol-name a))
             (the simple-base-string (symbol-name b))))
        :key #'first))

(compile-if-advantageous 'sorted/filtered-extended-repl-commands)

;;; ---------------------------------------------------------------------------

(defun show-all-extended-repl-commands ()
  ;;; Display all extended-REPL (defined tll-commands) that are not marked
  ;;; with :no-help control:
  (dolist (command (sorted/filtered-extended-repl-commands))
    (format t "~&~s~24,4t~@[~a~]~%"
	    (first command)
	    (third command))))

(compile-if-advantageous 'show-all-extended-repl-commands)

;;; ---------------------------------------------------------------------------
;;;  Interface into CLISP's *user-commands* facility
;;;
;;; Currently there is no way to read remaining values from the RELP read-line
;;; string in CLISP (but maybe in 2.45!)

#+clisp
(defun user-commands ()
  (mapcan 
   #'(lambda (command-spec)
       (destructuring-bind (command function doc help-control)
	   command-spec
	 (let* ((command-name (concatenate 'simple-string 
                                ":" 
                                (string-downcase (symbol-name command))))
                (command-fn-pair
                 `(,command-name 
                   . ,#'(lambda (&optional .string.)
                          (let ((args nil))
                            ;; Pre-2.45 CLISP's will not have a
                            ;; .string. argument:
                            (when .string.
                              (with-input-from-string (stream .string.)
                                (loop 
                                  (let ((form (read stream nil stream)))
                                    (when (eq form stream) (return))
                                    (push form args)))))
                            (apply function (nreverse args)))))))
           `(,command-fn-pair
             ;; native-help command documentation:
             ,@(when (eq help-control ':add-to-native-help)
                 (list (format nil "~%~a~24,4t~a" command-name doc)))))))
   (sort (copy-list *extended-repl-commands*)
         #'(lambda (a b)
             (string< 
              (the simple-base-string (symbol-name a))
              (the simple-base-string (symbol-name b))))
         :key #'first)))

#+clisp
(compile-if-advantageous 'user-commands)

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
 
 (compile-if-advantageous 'ext:interactive-eval))

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
(compile-if-advantageous 'ext:interactive-eval)

;;; ---------------------------------------------------------------------------
;;; Extend SBCL's default form reader, SB-IMPL::REPL-READ-FORM-FUN, with a
;;; richer version that provides keyword-command capabilities:

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

  (compile-if-advantageous 'extended-repl-read-form-fun)
  (setf sb-int:*repl-read-form-fun* #'extended-repl-read-form-fun))

;;; ===========================================================================
;;;  SLIME REPL Interface Setup

(defvar *slime-extended-repl-file*
    (make-pathname :name "slime-extended-repl"
                   :type "lisp"
                   :defaults *load-truename*))

(defun load-slime-extended-repl ()
  (format t "~&;; Loading extended REPL command processing for SLIME...~%")
  (load *slime-extended-repl-file*))  

;;;  *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
;;;  SLIME doesn't provide an easy means of using its *after-init-hook*
;;;  mechanism in advance of loading Swank.  The :swank package cannot be
;;;  created in advance, as the existence of the :swank package is used as an
;;;  indicator that Swank has been initialized. We work around this in the
;;;  current SLIME by creating the :swank-backend package (a package that will
;;;  be used by the :swank package, once the :swank package is created),
;;;  setting swank-backend::*after-init-hook*, and exporting
;;;  swank-backend::*after-init-hook* so that it will become Swank's
;;;  *after-init-hook*.  It would have been *SO* much easier if Swank simply
;;;  imported/used a better-named variable (such as *after-swank-init-hook*)
;;;  from the :cl-user package, allowing straightforward specification of
;;;  after-init hooks in advance of Swank loading.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Package :swank is already present:
  (when (find-package ':swank)
    ;; If there is not a :swank-backend package, assume an outdated SLIME
    ;; installation and delete the :swank package to force a reload:
    (unless (find-package ':swank-backend)
      (format t "~&;; Deleting outdated ~s package for SLIME...~%" 
              ':swank)
      (delete-package ':swank)))
  ;; Now define the :swank-backend package, if needed:
  (unless (find-package ':swank-backend)
    (format t "~&;; Predefining ~s package for SLIME...~%" 
            ':swank-backend)
    (make-package ':swank-backend :use '(:common-lisp))))

(cond
 ;; Swank is already present:
 ((find-package ':swank)
  (load-slime-extended-repl))
 ;; The :swank package doesn't exist yet:
 (t (export '(swank-backend::*after-init-hook*) ':swank-backend)
    (locally (declare (special swank-backend::*after-init-hook*))
      (if (boundp 'swank-backend::*after-init-hook*)
          (pushnew 'load-slime-extended-repl swank-backend::*after-init-hook*)
          (setf swank-backend::*after-init-hook* 
                '(load-slime-extended-repl))))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

