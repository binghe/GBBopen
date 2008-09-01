;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/control-shells/agenda-shell-user.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Aug 31 16:39:24 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

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
;;;  08-20-05 File created.  (Corkill)
;;;  02-15-08 Add :portable-threads use.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

;;; This file (and the :agenda-shell-user module), simply adds
;;; :portable-threads and :agenda-shell package use to the :gbbopen-user
;;; module.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '(:portable-threads :agenda-shell)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


