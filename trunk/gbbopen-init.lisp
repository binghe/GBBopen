;;;; -*- Mode:Common-Lisp; Package:Common-Lisp-User; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/gbbopen-init.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Mar 25 11:34:52 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-25-08 Renamed to more intuitive initiate.lisp.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;;; ***  NOTE: This file is going away soon.  Use initiate.lisp directly.  ***

(warn "Use <install-dir>/initiate.lisp in place of ~
       <install-dir>/gbbopen-init.lisp")

;;; ---------------------------------------------------------------------------

(let ((defaults *load-truename*))
  (load (make-pathname 
	 :name "initiate" :defaults defaults)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
