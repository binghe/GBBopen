;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/compile-all.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Nov 13 10:47:39 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    Compile All GBBopen Modules
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-22-06 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(mini-module::compile-module)))

;;; ---------------------------------------------------------------------------

(defun compile-it (module-name &key (dont-reset nil))
  (format t "~%;;; ~72,,,'-<-~>~%;;; Compiling ~s...~%" module-name)
  (compile-module module-name :propagate :create-dirs)
  (unless dont-reset
    (funcall (intern (symbol-name :reset-gbbopen) :gbbopen))))

;;; ---------------------------------------------------------------------------

(compile-it :gbbopen-test)
(compile-it :agenda-shell-test)
(compile-it :tutorial-example)
(compile-it :http-test :dont-reset 't)
(compile-it :multinode :dont-reset 't)
#-multiprocessing-not-available
(compile-it :portable-threads-test :dont-reset 't)
(compile-it :abort-ks-execution-example)

(format t "~&;;; GBBopen module compilation completed.~%")
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
