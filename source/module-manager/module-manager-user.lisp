;;;; -*- Mode:Common-Lisp; Package:MODULE-MANAGER-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/module-manager/module-manager-user.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Aug 31 14:55:06 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       Module-Manager-User Package
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-17-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':module-manager-user)
    (make-package ':module-manager-user
                  :use '(:common-lisp :module-manager)
                  :nicknames '(:mini-module-user))))

(in-package :module-manager-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((module-manager-package (find-package ':module-manager)))
    (when module-manager-package
      (use-package (list module-manager-package)))))

(pushnew ':module-manager-user *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


