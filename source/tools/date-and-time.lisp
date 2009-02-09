;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/date-and-time.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Feb  9 11:51:48 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Date and Time Entities
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-16-08 File split from tools.lisp.  (Corkill)
;;;  03-20-04 Added pretty time-interval functions.  (Corkill)
;;;  02-08-09 Added PARSE-TIME-INTERVAL.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(module-manager:*month-precedes-date*
            module-manager::*month-name-vector*
            module-manager:brief-date
            module-manager:brief-date-and-time
            module-manager:parse-date)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*month-precedes-date*       ; these three entities are defined in 
            bried-date                  ; ../module-manager/module-manager.lisp, 
            brief-date-and-time         ; but are part of :gbbopen-tools
            internet-text-date-and-time
            iso8661-date-and-time
            message-log-date-and-time
            parse-date                  ; also from module-manager.lisp
            parse-time-interval
            pretty-time-interval
            pretty-run-time-interval)))

;;; ===========================================================================
;;;  Time parsing and formatting

;;; Defined in ../module-manager/module-manager.lisp:
;;;  (defvar *month-precedes-date* 't) and 
;;;   (defparameter *month-name-vector* ...)

(defparameter *weekday-name-vector*
    #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

;;; ---------------------------------------------------------------------------
;;;  message-log-date-and-time

(defun message-log-date-and-time (&optional (time (get-universal-time))
                                            (destination nil))
  ;; Writes or returns a string representing local time in "message log"
  ;; format: MMM DD HH:MM:SS
  (multiple-value-bind (second minute hour date month)
      (decode-universal-time time)
    (format destination
            "~a ~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            (svref *month-name-vector* (1-& month))
            date
            hour
            minute
            second)))

;;; ---------------------------------------------------------------------------
;;;  ISO8661-date-and-time

(defun iso8661-date-and-time (&optional (time (get-universal-time))
                                        (destination nil))
  ;; Writes or returns a string representing time in ISO8661 (XML dateTime) 
  ;; format
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (format destination
            "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year
            month
            date
            hour
            minute
            second)))

;;; ---------------------------------------------------------------------------
;;;  Internet-text-date-and-time

(defun time-zone-abbreviation (zone daylight-savings-p)
  ;;; Return a time-zone abbreviation string for `zone;'
  ;;; `zone' is an integer with decode-universal-time semantics.
  (cdr (assoc zone                      
              (if daylight-savings-p
                  '((4 . "ADT")
                    (5 . "EDT")
                    (6 . "CDT")
                    (7 . "MDT")
                    (8 . "PDT")
                    (9 . "AKDT")        ; Alaska
                    (-1 . "BST")        ; British Summer (IST is Irish Summer) 
                    (-2 . "CEST")       ; Central Europe
                    (-3 . "EEST"))      ; Eastern Europe
                  '((0 . "GMT")
                    (4 . "AST")
                    (5 . "EST")
                    (6 . "CST")
                    (7 . "MST")
                    (8 . "PST")
                    (9 . "AKST")        ; Alaska
                    (10 . "HST")        ; Hawaii
                    (-1 . "CET")        ; Central Europe
                    (-2 . "EET")        ; Eastern Europe
                    (-10 . "AEST")))))) ; Australian Eastern

;;; ---------------------------------------------------------------------------

(defun internet-text-date-and-time (&optional (time (get-universal-time))
                                              time-zone
                                              (destination nil))
  ;;; Returns a string representing time in Internet Text Message format
  (multiple-value-bind (second minute hour date month year 
                        day daylight-savings-p zone)
      (if time-zone
          (decode-universal-time time time-zone)
          (decode-universal-time time))
    (let ((zone-value (*& -100 (if daylight-savings-p
                                   (1-& zone)
                                   zone))))
      (format destination
              "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d ~a~4,'0d~@[ (~a)~]"
              (svref *weekday-name-vector* day)
              date
              (svref *month-name-vector* (1-& month))
              year
              hour
              minute
              second
              ;; we do want -0000!
              (if (plusp& zone-value) "+" "-")
              (abs& zone-value)
              (time-zone-abbreviation zone daylight-savings-p)))))

;;; ---------------------------------------------------------------------------
;;;   Pretty time-interval conversions

(defun pretty-time-interval (interval-in-seconds 
                             &optional (maximum-fields 5)
                                       (destination nil))
  ;;; Converts `seconds' to a time interval string (rounded to the nearest
  ;;; 100th of a second):
  (check-type maximum-fields (integer 1 5))
  (let ((negative-p nil))
    (when (minusp interval-in-seconds)
      (setf negative-p 't)
      (setf interval-in-seconds (abs interval-in-seconds)))
    (multiple-value-bind (seconds remainder)
        (truncate interval-in-seconds)
      (multiple-value-bind (hundreds)
          (round (* remainder 100))
        ;; handle roundup!
        (when (=& 100 hundreds)
          (incf seconds)
          (setf hundreds 0))
        (multiple-value-bind (minutes seconds)
            (truncate seconds 60)
          (multiple-value-bind (hours minutes)
              (truncate minutes 60)
            (multiple-value-bind (days hours)
                (truncate hours 24)
              ;; Determine likely field usage:
              (when (or (plusp& hundreds)
                        (plusp& seconds))
                (decf& maximum-fields 2))
              (when (plusp& hours) (decf& maximum-fields))
              (when (plusp& minutes) (decf& maximum-fields))
              (when (plusp days) (decf& maximum-fields))
              (incf& maximum-fields)    ; adjust for easy plusp& checks below
              ;; Determine the actual fields to include, rounding for excluded
              ;; fields as needed:
              (let* ((hundreds-p (and (not (zerop& hundreds))
                                      (plusp& maximum-fields)))
                     (seconds-p (progn
                                  (unless hundreds-p
                                    (incf& maximum-fields)
                                    (when (>=& hundreds 50)
                                      (incf& seconds)
                                      (when (=& seconds 60)
                                        (setf seconds 0)
                                        (incf& minutes)
                                        (when (=& minutes 60)
                                          (setf minutes 0)
                                          (incf& hours)
                                          (when (=& hours 24)
                                            (setf hours 0)
                                            (incf days))))))
                                  (and (not (zerop& seconds))
                                       (plusp& maximum-fields))))
                     (minutes-p (progn
                                  (unless seconds-p 
                                    (incf& maximum-fields)
                                    (when (>=& seconds 30)
                                      (incf& minutes)
                                      (when (=& minutes 60)
                                        (setf minutes 0)
                                        (incf& hours)
                                        (when (=& hours 24)
                                          (setf hours 0)
                                          (incf days)))))
                                  (and (not (zerop& minutes))
                                       (plusp& maximum-fields))))
                     (hours-p (progn 
                                (unless minutes-p
                                  (incf& maximum-fields)
                                  (when (>=& minutes 30)
                                    (incf& hours)
                                    (when (=& hours 24)
                                      (setf hours 0)
                                      (incf days))))
                                (and (not (zerop& hours))
                                     (plusp& maximum-fields))))
                     (days-p (progn
                               (unless hours-p
                                 (when (>=& hours 12)
                                   (incf days)))
                               (not (zerop days)))))
                (flet ((write-it (stream)
                         (when negative-p (format stream "minus "))
                         (when days-p
                           (format stream "~s day~:p~@[~*, ~]"
                                   days 
                                   (or hours-p minutes-p seconds-p hundreds-p)))
                         (when hours-p 
                           (format stream "~s hour~:p~@[~*, ~]"
                                   hours 
                                   (or minutes-p seconds-p hundreds-p)))
                         (when minutes-p
                           (format stream "~s minute~:p~@[~*, ~]"
                                   minutes 
                                   (or seconds-p hundreds-p)))
                         (cond
                          ((or seconds-p hundreds-p)
                           (format stream "~s~:[~*~;.~2,'0d~] second~p" 
                                   seconds
                                   hundreds-p
                                   hundreds 
                                   seconds))
                          ;; Should only occur when interval-in-seconds is zero:
                          ((not (or days-p hours-p minutes-p))
                           (format stream "0 seconds")))))
                  (if destination
                      (write-it destination)
                      (with-output-to-string (stream)
                        (write-it stream))))))))))))

;;; ---------------------------------------------------------------------------

(defun pretty-run-time-interval (internal-run-time 
                                 &optional (maximum-fields 5)
                                           (destination nil))
  ;;; Converts `internal-run-time' to a time interval string (rounded to
  ;;; nearest 100th of a second).
  (pretty-time-interval (/ internal-run-time 
                           #.(float internal-time-units-per-second))
                        maximum-fields
                        destination))

;;; ===========================================================================

(defparameter *time-interval-units-alist*
    ;; Plurals are handled automatically:
    `(("second"        . 1)
      ("sec"           . 1)
      ("s"             . 1)
      ("minute"        . 60)
      ("min"           . 60)
      ("m"             . 60)
      ("hour"          . #.(* 60 60))
      ("hr"            . #.(* 60 60))
      ("h"             . #.(* 60 60))
      ("day"           . #.(* 24 60 60))
      ("d"             . #.(* 24 60 60))
      ("week"          . #.(* 7 24 60 60))
      ("wk"            . #.(* 7 24 60 60))
      ("month"         . #.(* 30 24 60 60))
      ("mon"           . #.(* 30 24 60 60))
      ("year"          . #.(* 365 24 60 60))
      ("yr"            . #.(* 365 24 60 60))))

;;; ---------------------------------------------------------------------------

(defun parse-time-interval (string &key (start 0) 
                                        (end (length string))
                                        (separators " ,")
                                        ;; units keyword is not documented:
                                        (units *time-interval-units-alist*))
  ;; Returns the number of seconds representing the time interval specified in
  ;; string:
  (declare (simple-string string))
  (flet ((skip-separators ()
           (loop 
               while (and (< (& start) (& end))
                          (find (schar string start) separators))
               do (incf& start)))
         (skip-to-a-separator-minus-or-digit ()
           (loop 
               while (and (< (& start) (& end))
                          (let ((char (schar string start)))
                            (not (or (digit-char-p char)
                                     (char= char #\-)
                                     (find char separators)))))
               do (incf& start)))
         (negative-sign-p ()
           ;; When string[start] is #\-, increment start and return true:
           (when (and (<& start end)
                      (char= #\- (schar string start)))
             (incf& start)))
         (no-numeric-value-error (start end)
           (error "No interval numeric value found prior to ~s"
                  (subseq string start end))))
    (let ((result 0))
      (while (<& start end)
        (skip-separators)
        (let (value
              (negative? (negative-sign-p)))
          (multiple-value-setq (value start)
            (parse-integer string :start start :end end :junk-allowed 't))
          ;; Check for decimal-pointed fraction or ratio (we don't support all
          ;; CL numbers here):
          (when (<& start end)
            (let ((indicator-char (schar string start)))
              (cond 
               ;; decimal-pointed fraction:
               ((char= #\. indicator-char)
                (unless (=& *read-base* 10)
                  (error "Only a decimal number can contain a decimal point"))
                (incf& start)
                (let (fraction
                      (fraction-start start)
                      (negative? (negative-sign-p)))
                  (multiple-value-setq (fraction start)
                    (parse-integer string :start fraction-start :end end :junk-allowed 't))
                  (unless (and (or value fraction) (not negative?))
                    (no-numeric-value-error start end))
                  (setf value (+$ (if value (float value) 0.0f0)
                                  (if fraction 
                                      (/$ (float fraction) 
                                          (expt 10.0f0 (-& start fraction-start)))
                                      0.0f0)))))
               ;; ratio:
               ((char= #\/ indicator-char)
                (incf& start)
                (let (denominator
                      (negative? (negative-sign-p)))
                  (multiple-value-setq (denominator start)
                    (parse-integer string :start start :end end :junk-allowed 't))
                  (unless (and value denominator (not negative?))
                    (no-numeric-value-error start end))
                  (setf value (/ value denominator)))))))
          (unless value 
            (when (=& start end)
              (return-from parse-time-interval result))
            (no-numeric-value-error start end))
          (skip-separators)
          (let ((token-start start))
            (skip-to-a-separator-minus-or-digit)
            (let ((token-end start))
              ;; Ignore plural "s" character at end:
              (let ((1-token-end (1-& token-end)))
                (when (and (char= (schar string 1-token-end) #\s)
                           (/=& token-start 1-token-end))
                  (setf token-end 1-token-end)))
              (let ((unit-acons (assoc-if #'(lambda (key) 
                                              (string-equal string key
                                                            :start1 token-start
                                                            :end1 token-end))
                                          units)))
                (unless unit-acons 
                  (error "Unknown time-interval unit ~s following value ~s"
                         (subseq string token-start start)
                         value))
                (setf value (* value (cdr unit-acons)))
                (if negative? (decf result value) (incf result value)))))))
      result)))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


