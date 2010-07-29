;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/gbbopen-tools-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri May 28 06:03:23 2010 *-*
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
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
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
  (format t "~&;;   Full-date-and-time test completed.~%")
  (values))

;;; ---------------------------------------------------------------------------

(defun hash-table-extensions-test (&optional (max-size 100000))
  (format t "~&;;   Starting hash-table extensions test...~%")
  (let* ((ht (make-hash-table :size 0))
         #+has-keys-only-hash-tables
         (keys-only-ht (make-keys-only-hash-table-if-supported :size 0))
         (ht-original-size (hash-table-size ht))
         #+has-keys-only-hash-tables
         (keys-only-ht-original-size (hash-table-size keys-only-ht)))
    ;; Resize:
    (resize-hash-table ht max-size)    
    #+has-keys-only-hash-tables
    (resize-hash-table keys-only-ht max-size)
    (let ((ht-resized-size (hash-table-size ht))
          #+has-keys-only-hash-tables
          (keys-only-ht-resized-size (hash-table-size keys-only-ht)))
      ;; Check for resizing:
      (unless (> ht-resized-size ht-original-size)
        (format t "~&;;     Resizing did not expand hash table."))
      #+has-keys-only-hash-tables
      (unless (> keys-only-ht-resized-size keys-only-ht-original-size)
        (format t "~&;;     Resizing did not expand keys-only hash table."))
      ;; Fill the hash-tables:
      (dotimes (i max-size)
        (declare (fixnum i))
        #+has-keys-only-hash-tables
        (setf (gethash i keys-only-ht) 't)
        (setf (gethash i ht) 'i))
      (let ((ht-grown-size (hash-table-size ht))
            #+has-keys-only-hash-tables
            (keys-only-ht-grown-size (hash-table-size keys-only-ht)))
        ;; Delete the entries:
        (dotimes (i max-size)
          (declare (fixnum i))
          #+has-keys-only-hash-tables
          (remhash i keys-only-ht)
          (remhash i ht))
        ;; Check for automatic shrinking:
        (let ((ht-shrunk-amount (- ht-grown-size (hash-table-size ht)))
              #+has-keys-only-hash-tables
              (keys-only-ht-shrunk-amount
               (- keys-only-ht-grown-size (hash-table-size keys-only-ht))))
          (when (plusp ht-shrunk-amount)
            (format t "~&;;     Remhash shrinks hash tables by ~s."
                    ht-shrunk-amount))
          #+has-keys-only-hash-tables
          (when (plusp keys-only-ht-shrunk-amount)
            (format t "~&;;     Remhash shrinks keys-only hash tables by ~s."
                    keys-only-ht-shrunk-amount)))
        ;; Resize:
        (resize-hash-table ht 0)    
        #+has-keys-only-hash-tables
        (resize-hash-table keys-only-ht 0)
        ;; Check for resized shrinking:
        (let ((ht-shrunk-size (hash-table-size ht))
              #+has-keys-only-hash-tables
              (keys-only-ht-shrunk-size (hash-table-size keys-only-ht)))
          (if (plusp (- ht-grown-size ht-shrunk-size))
              (unless (= ht-shrunk-size ht-original-size)
                (format t "~&;;     Resizing did not shrink hash table ~
                                  to its original size."))
              (format t "~&;;     Resizing did not shrink hash table."))
          #+has-keys-only-hash-tables
          (if (plusp (- keys-only-ht-grown-size keys-only-ht-shrunk-size))
              (unless (= keys-only-ht-shrunk-size keys-only-ht-original-size)
                (format t "~&;;     Resizing did not shrink keys-only hash table ~
                                  to its original size."))
              (format t "~&;;     Resizing did not shrink keys-only ~
                                hash table."))))))
  (format t "~&;;   Hash-table extensions test completed.~%"))

;;; ---------------------------------------------------------------------------

(defun gbbopen-tools-tests (&optional verbose)
  (format t "~&;;; Starting GBBopen-Tools tests...~%")
  (full-date-and-time-test)
  (hash-table-extensions-test)
  ;; LLRB tests (from llrb-test.lisp):
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

