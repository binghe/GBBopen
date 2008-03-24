;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/gbbopen-user.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Mar 24 11:07:35 2008 *-*
;;;; *-* Machine: cyclone.local *-*

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
;;; Copyright (C) 2004-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-08-04 File Created.  (Corkill)
;;;  01-17-06 Use :mini-module package.  (Corkill)
;;;  03-24-06 Use :clos package.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :gbbopen-user)
    (defpackage :gbbopen-user
      (:use :common-lisp :clos :mini-module :gbbopen-tools :gbbopen))))

(in-package :gbbopen-user)

(pushnew :gbbopen-user *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


