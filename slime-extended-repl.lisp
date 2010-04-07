;;;; -*- Mode:Common-Lisp; Package:SWANK; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/slime-extended-repl.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 09:53:27 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                Extended SLIME REPL Command Processing
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill 
;;;
;;; Copyright (C) 2005-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-17-08 Separated out from extended-repl.lisp and rewritten for
;;;           the latest Swank mechanisms.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	 
(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cl-user::compile-if-advantageous)))

;;; ---------------------------------------------------------------------------

(defun get-extended-repl-command-with-help (command)
  ;; Used in extended-repl-swank-eval-hook to add SLIME support for :help on
  ;; CLs that already provide their own REPL help command (and is therefore
  ;; not in *extended-repl-commands*)
  (or (assoc command cl-user::*extended-repl-commands* :test #'eq)
      #+(or allegro ecl)
      (and (member command '(:help :h))
	   #+allegro
	   '(:help tpl::tpl-help-command)
	   #+ecl
	   '(:help si::tpl-help-command))))

(compile-if-advantageous 'get-extended-repl-command-with-help)

;;; ---------------------------------------------------------------------------

(unless (eq *listener-eval-function* 'repl-eval)
  (warn "Swank's ~s is not ~s." '*listener-eval-function* 'repl-eval))

(setf *listener-eval-function* 'extended-repl-eval)

;;; ---------------------------------------------------------------------------

(defun repl-command-form (string)
  (setf string (string-left-trim '(#\space #\tab) string))
  (when (or 
         ;; Check for 'spread' command syntax:
         (eql (char string 0) #\:)
         ;; Quick check for 'list' command syntax::
         (and (eql (char string 0) #\()
              (eql (char string 1) #\:)))
    (with-input-from-string (stream string)
      (let ((form (read stream nil stream)))
        (flet ((do-command (symbol-or-fn args)
                 (apply (the function (if (symbolp symbol-or-fn) 
                                          (fdefinition symbol-or-fn)
                                          symbol-or-fn))
                        args)
                 (values)))
          (cond 
           ;; No form was read:
           ((eq form stream) nil)
           ;; A keyword symbol (possible command) was read:
           ((keywordp form)
            (let ((repl-command (get-extended-repl-command-with-help form)))
              (when repl-command
                (do-command (second repl-command) 
                  (loop 
                      for form = (read stream nil stream)
                      until (eq form stream)
                      collect form))
                ;; bypass normal REPL processing:
                't)))
           ;; Support (<command> <arg>*) syntax as well:
           ((consp form)
            (let ((repl-command
                   (get-extended-repl-command-with-help (car form))))
              (when repl-command
                (do-command (second repl-command) (cdr form))
                ;; bypass normal REPL processing:
                't)))))))))

(compile-if-advantageous 'repl-command-form)

;;; ---------------------------------------------------------------------------

(defun extended-repl-eval (string)
  (unless (repl-command-form string)
    ;; Normal REPL processing:
    (repl-eval string)))

(compile-if-advantageous 'extended-repl-eval)

;;; ---------------------------------------------------------------------------

(defun set-slime-repl-package (package-specifier)
  (when *emacs-connection*
    (let ((package-name 
           (if (packagep package-specifier)
               (package-name package-specifier)
               package-specifier))
          ;; Don't return the results:
          (*send-repl-results-function* #'identity))
      (repl-eval (format nil "(in-package ~s)" package-name))
      (let ((package (find-package package-name)))
        (when package
          (setf *package* package)))
      ;; Return success:
      't)))

(compile-if-advantageous 'set-slime-repl-package)

;;; ---------------------------------------------------------------------------

(format t "~&;; Finished loading extended REPL command processing for SLIME.~%")

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

