;;;; -*- Mode:Common-Lisp; Package:SWANK; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/slime-extended-repl.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Apr 17 05:02:04 2008 *-*
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
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-17-08 Separated out from extended-repl.lisp and rewritten for
;;;           the latest Swank mechanisms.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	 
(in-package :swank)

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

;;; ---------------------------------------------------------------------------

(unless (eq *listener-eval-function* 'repl-eval)
  (warn "Swank's ~s is not ~s." '*listener-eval-function* 'repl-eval))

(setf *listener-eval-function* 'extended-repl-eval)

;;; ---------------------------------------------------------------------------

(defun repl-command-form (string)
  (setq string (string-left-trim '(#\space #\tab) string))
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
           ((keywordp form)
            (let ((repl-command (get-extended-repl-command-with-help form)))
              (when repl-command
                (do-command (second repl-command) nil)
                't)))
           ;; Support (<command> <arg>*) syntax as well:
           ((consp form)
            (let ((repl-command
                   (get-extended-repl-command-with-help (car form))))
              (when repl-command
                (do-command (second repl-command) (cdr form))
                't)))))))))

;;; ---------------------------------------------------------------------------

(defun extended-repl-eval (string)
  (unless (repl-command-form string)
    ;; Normal REPL processing:
    (repl-eval string)))

;;; ---------------------------------------------------------------------------

(format t "~&;; Finished loading extended REPL command processing for SLIME.~%")

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

