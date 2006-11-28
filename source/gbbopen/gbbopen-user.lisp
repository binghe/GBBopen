;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/gbbopen-user.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Sep 19 01:11:24 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      GBBopen-User Package
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-08-04 File Created.  (Corkill)
;;;  08-05-04 Removed CMUCL-18e workarounds.  (Corkill)
;;;  01-17-06 Use :mini-module package, if available.  (Corkill)
;;;  03-24-06 Use :clos package.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :gbbopen-user)
    (defpackage :gbbopen-user
      (:use :common-lisp :clos :gbbopen-tools :gbbopen))))

(in-package :gbbopen-user)

(let ((mini-module-package (find-package :mini-module)))
  (when mini-module-package
    (use-package (list mini-module-package))))

(pushnew :gbbopen-user *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


