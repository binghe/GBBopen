;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/gbbopen-tools-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jul 17 12:45:30 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                           GBBopen-Tools Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :gbbopen-tools)

;;; ---------------------------------------------------------------------------

(defun gbbopen-tools-tests (&optional verbose)
  (format t "~&;;; Starting GBBopen-Tools tests...~%")
  (basic-llrb-tree-test verbose)
  (format t "~&;;; All GBBopen-Tools tests completed.~%")
  (values))

;;; ---------------------------------------------------------------------------

(when *autorun-modules*
  (gbbopen-tools-tests))
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

