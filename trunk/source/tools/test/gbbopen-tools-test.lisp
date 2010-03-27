;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/gbbopen-tools-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Mar 27 06:51:21 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                           GBBopen-Tools Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2010, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-15-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools-user)

;;; ---------------------------------------------------------------------------

(defun full-date-and-time-test (&optional (destination t))
  (format t "~&;;   Starting full-date-and-time test...~%")
  (let ((output-stream? (and (streamp destination)
                             (output-stream-p destination)))
        (adjustable-string? (and (stringp destination)
                                 (array-has-fill-pointer-p destination))))
    (labels 
        ((separate-it ()
           (cond ((eq destination 't)
                  (terpri))
                 (output-stream? 
                  (terpri destination))
                 (adjustable-string? 
                  (vector-push-extend #\| destination))))
         (test-it (&rest args)
           (declare (dynamic-extent args))
           ;; Year not first:
           (apply #'full-date-and-time nil
                  :destination destination
                  :month-precedes-date 't
                  args)
           (separate-it)
           (apply #'full-date-and-time nil
                  :destination destination
                  :month-precedes-date nil
                  args)
           (separate-it)
           ;; Year first:
           (apply #'full-date-and-time nil
                  :destination destination
                  :year-first 't
                  :month-precedes-date 't
                  args)
           (separate-it)
           (apply #'full-date-and-time nil
                  :destination destination
                  :year-first 't
                  :month-precedes-date nil
                  args)
           (separate-it)))
      (when adjustable-string? 
        (vector-push-extend #\| destination))
      (test-it)
      (test-it :all-numeric 't)
      (test-it :all-numeric 't :separator #\-)
      (test-it :all-numeric 't :12-hour 't)
      (test-it :all-numeric 't :include-time-zone 't)
      (test-it :all-numeric 't :include-time-zone 't :time-zone 0)
      (test-it :all-numeric 't :utc-offset-only 't)
      (test-it :include-seconds 't)
      (test-it :full-names 't)
      ;; Include day:
      (test-it :include-day 't)
      (test-it :include-day 't :all-numeric 't)
      (test-it :include-day 't :all-numeric 't :separator #\-)
      (test-it :include-day 't :all-numeric 't :12-hour 't)
      (test-it :include-day 't :all-numeric 't :include-time-zone 't)
      (test-it :include-day 't :all-numeric 't :include-time-zone 't :time-zone 0)
      (test-it :include-day 't :all-numeric 't :utc-offset-only 't)
      (test-it :include-day 't :include-seconds 't)
      (test-it :include-day 't :full-names 't)))
  (format t "~&;;   Full-date-and-time test copleted.~%")
  (values))

;;; ---------------------------------------------------------------------------

(defun gbbopen-tools-tests (&optional verbose)
  (format t "~&;;; Starting GBBopen-Tools tests...~%")
  (full-date-and-time-test)
  (basic-llrb-tree-test verbose)
  (random-size-llrb-tree-test (min 200000 most-positive-fixnum))
  (format t "~&;;; All GBBopen-Tools tests completed.~%")
  (values))

;;; ---------------------------------------------------------------------------

(when *autorun-modules*
  (gbbopen-tools-tests))
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

