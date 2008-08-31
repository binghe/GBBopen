;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/compile-all.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Jul 13 10:40:25 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

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
;;; Copyright (C) 2006-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-22-06 File created.  (Corkill)
;;;  06-06-07 Quit Common Lisp when finished.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

;;; ---------------------------------------------------------------------------

(defun compile-it (module-name &key (dont-reset nil))
  (format t "~%;;; ~72,,,'-<-~>~%;;; Compiling ~s...~%" module-name)
  (compile-module module-name :propagate :create-dirs :noautorun)
  (unless dont-reset
    (funcall (intern (symbol-name :reset-gbbopen) :gbbopen))))

;;; ---------------------------------------------------------------------------

;; Compile :agenda-shell-test first to compile most of the commonly shared
;; modules at once (and reduce :forces-recompile recompilations):
(compile-it :agenda-shell-test)
;; Now do the rest:
(compile-it :gbbopen-test)
(compile-it :tutorial-example)
(compile-it :os-interface-test :dont-reset 't)
(compile-it :http-test :dont-reset 't)
(compile-it :multinode :dont-reset 't)
(compile-it :portable-threads-test :dont-reset 't)
(compile-it :cl-timing :dont-reset 't)
(compile-it :abort-ks-execution-example)

(format t "~2&;;; ~72,,,'-<-~>~
            ~%;;; GBBopen modules compilation completed.~%")
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
