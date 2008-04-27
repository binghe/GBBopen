;;;; -*- Mode:Common-Lisp; Package:MINI-MODULE-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/mini-module/mini-module-user.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr 27 14:10:52 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       Mini-Module-User Package
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
;;;  03-17-08 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':mini-module-user)
    (make-package ':mini-module-user
                  :use '(:common-lisp :mini-module))))

(in-package :mini-module-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((mini-module-package (find-package ':mini-module)))
    (when mini-module-package
      (use-package (list mini-module-package)))))

(pushnew :mini-module-user *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


