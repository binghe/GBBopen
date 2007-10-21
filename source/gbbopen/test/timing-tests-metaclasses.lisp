;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/gbbopen/test/timing-tests-metaclasses.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Oct 15 05:49:01 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *              Metaclasses for GBBopen and CL Timing Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-18-07 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

(define-class testing-metaclass (standard-class)
  ((instance-hash-table :initform (make-hash-table :test 'eq))
   (instance-name-counter :initform 0))
  (:generate-accessors-format :prefix))

;;; ---------------------------------------------------------------------------

(defmethod validate-superclass ((class testing-metaclass) 
                                (superclass standard-class))
  #+ecl (declare (ignore class superclass))
  't)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


