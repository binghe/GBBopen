;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/gbbopen-tools-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Oct  1 05:27:17 2010 *-*
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-assumed-date/time-values (args &body body)
    ;; Assumes DATE, MONTH, and YEAR are bound to DECODED-TIME values, that the
    (declare (ignore args))
    `(let ((assumed-july-4th-year
            (if (or (<& month 7)
                    (and (=& month 7)
                         (<& date 4)))
                year
                (1+& year)))
           (assumed-july-1st-year
            (if (<& month 7)
                year
                (1+& year)))
           (assumed-april-7th-year
            (if (or (<& month 4)
                    (and (=& month 4)
                         (<& date 7)))
                year
                (1+& year))))
      ,@body)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro inserted-date-tests ()
    ;; Assumes DAY, MONTH, and YEAR are bound to DECODED-TIME values, that the
    ;; inserted tests are within WITH-ASSUMED-DATE/TIME-VALUES, and that the
    ;; lexical function TEST-IT is defined:
    '(with-assumed-date/time-values ()
      (test-it '(0 0 0 1 4 2010 nil nil 10)
       "1 Apr 2010")
      (test-it '(0 0 0 1 4 2010 nil nil 13)
       "April 1, 2010")
      (test-it '(0 0 0 1 4 2010 nil nil 14)
       "Thu 1 Apr 2010")
      (test-it '(0 0 0 1 4 2010 nil nil 23)
       "Thursday, April 1, 2010")
      (test-it '(0 0 0 4 1 2010 nil nil 6)
       "1-4-10" :month-precedes-date 't)
      (test-it '(0 0 0 1 4 2010 nil nil 6)
       "1-4-10" :month-precedes-date nil)
      (test-it `(0 0 0 4 7 ,assumed-july-4th-year nil nil 5)
       "4 Jul")
      (test-it `(0 0 0 4 7 ,assumed-july-4th-year nil nil 5)
       "Jul 4")
      (test-it `(0 0 0 4 7 ,year nil nil 5)
       "4 Jul" :default-to-current-year 't)
      (test-it `(0 0 0 4 7 ,year nil nil 5)
       "Jul 4" :default-to-current-year 't)
      (test-it `(0 0 0 4 7 ,assumed-july-4th-year nil nil 5)
       "4 Jul" :month-precedes-date 't)
      (test-it `(0 0 0 4 7 ,assumed-july-4th-year nil nil 5)
       "4 Jul" :month-precedes-date nil)
      (test-it `(0 0 0 1 7 2004 nil nil 5)
       "4 Jul" :year-first 't)
      (test-it `(0 0 0 4 7 ,assumed-july-4th-year nil nil 5)
       "Jul 4" :year-first 't)
      (test-it `(0 0 0 4 7 ,assumed-july-4th-year nil nil 5)
       "4/7  " :month-precedes-date nil) 
      (test-it `(0 0 0 7 4 ,assumed-july-4th-year nil nil 3)
       "4/7" :month-precedes-date 't) 
      (test-it `(0 0 0 4 7 ,year nil nil 5)
       "4 Jul" :default-to-current-year 't)
      (test-it `(0 0 0 4 7 ,year nil nil 5)
       "Jul 4" :default-to-current-year 't)
      (test-it `(0 0 0 4 7 ,year nil nil 5)
       "4/7  " :month-precedes-date nil :default-to-current-year 't) 
      (test-it `(0 0 0 7 4 ,year nil nil 3)
       "4/7" :month-precedes-date 't :default-to-current-year 't) 
      (test-it `(0 0 0 1 7 2004 nil nil 3)
       "4/7" :year-first 't) 
      #+FIX-MONTH-DAY-CONFUSION
      (test-it `(0 0 0 7 1 2004 nil nil 3)
       "4/7" :month-precedes-date nil :year-first 't) 
      (test-it `(0 0 0 1 7 2004 nil nil 3)
       "4/7" :month-precedes-date 't :year-first 't) 
      (test-it `(0 0 0 7 4 ,assumed-april-7th-year nil nil 5)
       "4/7  junk" :junk-allowed 't)
      (test-it `(0 0 0 1 7 ,assumed-july-1st-year nil nil 3)
       "Jul")
      (test-it `(0 0 0 1 7 ,assumed-july-1st-year nil nil 5)
       "Jul  " :month-precedes-date 't)
      (test-it `(0 0 0 1 7 ,assumed-july-1st-year nil nil 5)
       "Jul  " :month-precedes-date nil)
      (test-it `(0 0 0 1 7 ,assumed-july-1st-year nil nil 5)
       "Jul  " :month-precedes-date 't :year-first 't)
      (test-it `(0 0 0 1 7 ,assumed-july-1st-year nil nil 5)
       "Jul  " :month-precedes-date nil :year-first 't)
      (test-it `(0 0 0 1 1 2001 nil nil 6)
       "2001  ")
      (test-it `(0 0 0 1 1 2001 nil nil 4)
       "2001" :month-precedes-date 't)
      (test-it `(0 0 0 1 1 2001 nil nil 4)
       "2001" :month-precedes-date nil)
      (test-it `(0 0 0 1 1 2001 nil nil 4)
       "2001" :month-precedes-date 't :year-first 't)
      (test-it `(0 0 0 1 1 2001 nil nil 4)
       "2001" :month-precedes-date nil :year-first 't))))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro inserted-time-tests ()
    ;; Assumes that the inserted tests are within
    ;; WITH-ASSUMED-DATE/TIME-VALUES and that the lexical function TEST-IT is
    ;; defined:
    '(progn
      (test-it `(0 30 10 ,date ,month ,year nil nil 5)
       "10:30")
      (test-it `(0 30 22 ,date ,month ,year nil nil 7)
       "10:30pm")
      (test-it `(0 30 10 ,date ,month ,year 4 t 9)
       "10:30 EDT")
      (test-it `(0 30 10 ,date ,month ,year -11/2 nil 9)
       "10:30 IST")
      (test-it `(0 30 10 ,date ,month ,year 7 nil 11)
       "10:30 UTC-7"))))

;;; ---------------------------------------------------------------------------

(defun parse-date-test ()
  (format t "~&;;   Starting parse-date test...~%")
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (let ((*month-precedes-date* 't))   ; normal default value
      (flet ((test-it (expected-result string &rest args)
               (format t "~&;;     ~s~{ ~s~} => "
                       string args)
               (destructuring-bind (expected-second expected-minute
                                    expected-hour expected-date 
                                    expected-month expected-year
                                    expected-zone expected-daylight-savings-p 
                                    expected-position)
                   expected-result
                 (declare (ignore expected-second expected-minute
                                  expected-hour expected-zone 
                                  expected-daylight-savings-p))
                 (multiple-value-bind (returned-date returned-month returned-year 
                                       returned-position)
                     (apply #'parse-date string args)
                   (flet ((bad-result (field expected-value returned-value)
                            (error "Incorrect ~s ~s result for ~s: ~
                                    ~s expected; ~s returned"
                                   'parse-date
                                   field
                                   string
                                   expected-value
                                   returned-value)))
                     (unless (=& expected-year returned-year)
                       (bad-result 'year expected-year returned-year))
                     (unless (=& expected-month returned-month)
                       (bad-result 'month expected-month returned-month))
                     (unless (=& expected-date returned-date)
                       (bad-result 'date expected-date returned-date))
                     (unless (=& expected-position returned-position)
                       (bad-result 'position expected-position returned-position)))
                   ;; Correct!
                   (very-brief-date
                    (encode-universal-time 
                     0 0 0 
                     returned-date returned-month returned-year)
                    :destination *standard-output*)))))
        (inserted-date-tests))))
  (format t "~&;;   Parse-date test completed.~%"))
  
;;; ---------------------------------------------------------------------------

(defun parse-time-test ()
  (format t "~&;;   Starting parse-time test...~%")
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (flet ((test-it (expected-result string &rest args)
             (format t "~&;;     ~s~{ ~s~} => "
                     string args)
             (destructuring-bind (expected-second expected-minute
                                  expected-hour expected-date 
                                  expected-month expected-year
                                  expected-zone expected-daylight-savings-p 
                                  expected-position)
                 expected-result
               (declare (ignore expected-date expected-month expected-year))
               (multiple-value-bind (returned-second returned-minute
                                     returned-hour returned-zone 
                                     returned-daylight-savings-p returned-position)
                   (apply #'parse-time string args)
                 (flet ((bad-result (field expected-value returned-value)
                          (error "Incorrect ~s ~s result for ~s: ~
                                    ~s expected; ~s returned"
                                 'parse-time
                                 field
                                 string
                                 expected-value
                                 returned-value)))
                   (unless (=& expected-hour returned-hour)
                       (bad-result 'hour expected-hour returned-hour))
                     (unless (=& expected-minute returned-minute)
                       (bad-result 'minute expected-minute returned-minute))
                     (unless (=& expected-second returned-second)
                       (bad-result 'second expected-second returned-second))
                     (unless (eql expected-zone returned-zone)
                       (bad-result 'zone expected-zone returned-zone))
                     (unless (eql expected-daylight-savings-p
                                  returned-daylight-savings-p)
                       (bad-result 'daylight-savings-p 
                                   expected-daylight-savings-p 
                                   returned-daylight-savings-p))
                     (unless (=& expected-position returned-position)
                       (bad-result 'position expected-position returned-position)))
                 (brief-date-and-time
                  (encode-universal-time 
                   returned-second returned-minute returned-hour
                   date month year)
                  :destination *standard-output*)))))
      (inserted-time-tests)))
  (format t "~&;;   Parse-time test completed.~%"))
  
;;; ---------------------------------------------------------------------------

(defun parse-date-and-time-test ()
  (format t "~&;;   Starting parse-date-and-time test...~%")
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (flet ((test-it (expected-result string &rest args)
             (format t "~&;;     ~s~{ ~s~} => "
                     string args)
             (let ((result (multiple-value-list
                            (apply #'parse-date-and-time string args))))
               (if (equalp result expected-result)
                   (destructuring-bind (second hour minute date month year 
                                        time-zone daylight-savings-p position)
                       result
                     (declare (ignore daylight-savings-p position))
                     (full-date-and-time
                      (if time-zone
                          (encode-universal-time 
                           second hour minute date month year time-zone)
                          (encode-universal-time 
                           second hour minute date month year))
                      :destination *standard-output*))
                   (error "Incorrect ~s~{ ~s~} result for ~s: ~s"
                          'parse-date-and-time
                          args
                          string
                          result)))))
      (inserted-date-tests)
      (let ((*time-first* 't))
        (inserted-time-tests))
      (test-it '(0 30 10 1 4 2010 nil nil 19)
               "April 1, 2010 10:30")
      (test-it '(0 30 10 1 4 2010 nil nil 19)
               "10:30 April 1, 2010" :time-first 't)
      (test-it '(0 30 22 1 4 2010 nil nil 14)
               "4/1/10 10:30pm")
      (test-it '(0 30 10 1 4 2010 4 t 20)
               "Apr 1 2010 10:30 EDT")
      (test-it '(0 30 10 1 4 2010 -11/2 nil 20)
               "1 Apr 2010 10:30 IST")
      (test-it '(0 30 10 1 4 2010 7 nil 25)
               "April 1, 2010 10:30 UTC-7")
      (test-it '(0 30 10 1 4 2010 nil nil 29)
               "Thursday, April 1, 2010 10:30")
      (test-it '(46 30 10 1 4 2010 nil nil 32)
               "Thursday, April 1, 2010 10:30:46")
      (test-it '(46 30 10 1 4 2010 nil nil 32)
               "10:30:46 Thursday, April 1, 2010" :time-first 't)))
  (format t "~&;;   Parse-date-and-time test completed.~%"))
  
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
  (parse-date-test)
  (parse-time-test)
  (parse-date-and-time-test)
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

