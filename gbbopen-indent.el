;;;; -*- Mode:Emacs-Lisp -*-
;;;; *-* File: /home/gbbopen/current/gbbopen-indent.el *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Apr 22 18:44:49 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       GBBopen ELI Indentations 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2005, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-02 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(defun gbbopen:add-indentation ()
  (interactive)
  ;; "Improve" some indentations:
  (put 'if 'fi:lisp-indent-hook 3)
  (put 'setf 'fi:lisp-indent-hook 0)
  ;; GBBopen entity indentations:
  (put 'define-module 'fi:lisp-indent-hook 1)
  (put 'make-space-instance 'fi:lisp-indent-hook 2)
  )

(add-hook 'fi:lisp-mode-hook (function gbbopen:add-indentation))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************

