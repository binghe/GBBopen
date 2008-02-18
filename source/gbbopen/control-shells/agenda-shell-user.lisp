;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/gbbopen/control-shells/agenda-shell-user.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Feb 16 11:50:10 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      Agenda-Shell-User Package
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
;;;  08-20-05 File Created.  (Corkill)
;;;  02-15-08 Add :portable-threads use.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

;;; This file (and the :agenda-shell-user module), simply adds the
;;; :portable-threads and :agenda-shell package use to the
;;; :gbbopen-user module.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '(:portable-threads :agenda-shell)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


