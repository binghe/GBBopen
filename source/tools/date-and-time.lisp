;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/date-and-time.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri May 16 14:37:40 2008 *-*
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
;;; Copyright (C) 2002-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-16-08 File split from tools.lisp.  (Corkill)
;;;  03-20-04 Added pretty time-interval functions.  (Corkill)
;;;  05-15-08 Added parse-date.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

;;; ---------------------------------------------------------------------------
;;;  Exported tools entities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(brief-date-and-time         ; in mini-module.lisp, but part of
                                        ; :gbbopen-tools
	    internet-text-date-and-time ; not yet documented
	    iso8661-date-and-time	; not yet documented
	    message-log-date-and-time   ; not yet documented
            parse-date                  ; not yet documented
	    pretty-time-interval        ; not yet documented
	    pretty-run-time-interval    ; not yet documented
	    )))

;;; ===========================================================================
;;;  Time parsing and formatting

(defparameter *month-name-vector*
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defparameter *month-full-name-vector*
    #("January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November" "December"))

(defparameter *weekday-name-vector*
    #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

;;; ---------------------------------------------------------------------------

(defun parse-date (string &key (start 0) 
                               (end (length string))
                               (junk-allowed nil)
                               (separators "-/ ,")
                               (month-precedes-date *month-precedes-date*))
  ;;; Parses many intuitive date formats (sensitive to month-precedes-date,
  ;;; if needed):
  (declare (simple-string string))
  (let (date month year month-preceded-date)
    (flet ((process-date ()
             (multiple-value-setq (date start)
               (parse-integer string :start start :end end :junk-allowed t)))
           (process-month ()
             (cond
              ;; Numeric month:
              ((digit-char-p (schar string start))
               (multiple-value-setq (month start)
                 (parse-integer string :start start :end end :junk-allowed t)))
              ;; Month name:
              (t (let* (month-string
                        (month-equal-fn
                         #'(lambda (month-name)
                             (when (string-equal 
                                    string month-name
                                    :start1 start
                                    :end1 (min& end
                                                (+& start (length month-name))))
                               (setf month-string month-name)))))
                   (setf month (or (position-if month-equal-fn 
                                                *month-full-name-vector*)
                                   (position-if month-equal-fn
                                                *month-name-vector*)))
                   (unless month
                     (error "Unable to determine the month in ~s" string))
                   (incf& month)
                   (incf& start (length month-string))))))
           (skip-separators ()
             (while (and (<& start end)
                         (find (schar string start) separators))
               (incf& start))))
      (skip-separators)
      ;; If we have a month name, then we know the month-date order; otherwise
      ;; we'll assume month-first for until we process the second field:
      (when (alpha-char-p (schar string start))
        (setf month-preceded-date 't))
      (process-month)
      (skip-separators)
      (cond
       ((and (not month-preceded-date)
             (or
              ;; We have a month name in the second field:
              (alpha-char-p (schar string start))
              ;; Use the month-precedes-date value to decide the order:
              (not month-precedes-date)))
        ;; We actually have the date value from the first field (rather than
        ;; the month), so set the date from the assumed month value and then
        ;; proceed with month processing:
        (setf date month)
        (process-month))
       ;; Simply continue, as we have month then date order:
       (t (process-date)))
      (skip-separators))
    (check-type month (integer 1 12))
    (check-type date (integer 1 31))
    ;; Process year:
    (cond 
     ;; Assumed year, if omitted:
     ((=& start end)
      (multiple-value-bind (seconds minutes hours 
                            current-date current-month current-year)
          (get-decoded-time)
        (declare (ignore seconds minutes hours))
        (setf year current-year)
        ;; Assume next year, if the date is past in the current year:
        (when (or (<& month current-month)
                  (and (=& month current-month)
                       (<& date current-date)))
          (incf& year))))
     ;; Otherwise, process the specified year:
     (t (multiple-value-setq (year start)
          (parse-integer string :start start :end end 
                         :junk-allowed junk-allowed))))
    (check-type year (integer 0 #.most-positive-fixnum))
    ;; Upgrade YY to YYYY -- YY assumed within +/- 50 years from current time
    ;; (if year < 100):
    (setf year (cond 
                ;; No year upgrade needed:
                ((>=& year 100) year)
                ;; Do the upgrade:
                (t (let ((current-century
                          (*& 100 (truncate& (nth-value 5 (get-decoded-time))
                                             100))))
                     (if (>=& year 50) 
                         (+& year current-century -100)
                         (+& year current-century))))))
    (values date month year)))

;;; ---------------------------------------------------------------------------
;;;  message-log-date-and-time

(defun message-log-date-and-time (&optional (time (get-universal-time)))
  ;; Returns a string representing local time in "message log" format:
  ;; MMM DD HH:MM:SS
  (multiple-value-bind (second minute hour date month)
      (decode-universal-time time)
    (format nil 
            "~a ~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            (svref *month-name-vector* (1-& month))
            date
            hour
            minute
            second)))

;;; ---------------------------------------------------------------------------
;;;  ISO8661-date-and-time

(defun iso8661-date-and-time (&optional (time (get-universal-time)))
  ;; Returns a string representing time in ISO8661 (XML dateTime) format
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (format nil 
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
		    (9 . "AKDT")	; Alaska
		    (-1 . "BST")	; British Summer (IST is Irish Summer) 
		    (-2 . "CEST")	; Central Europe
		    (-3 . "EEST"))	; Eastern Europe
		  '((0 . "GMT")
		    (4 . "AST")
		    (5 . "EST")
		    (6 . "CST")
		    (7 . "MST")
		    (8 . "PST")
		    (9 . "AKST")	; Alaska
		    (10 . "HST")	; Hawaii
		    (-1 . "CET")	; Central Europe
		    (-2 . "EET")	; Eastern Europe
		    (-10 . "AEST"))))))	; Australian Eastern

;;; ---------------------------------------------------------------------------

(defun internet-text-date-and-time (&optional (time (get-universal-time))
                                              time-zone)
  ;;; Returns a string representing time in Internet Text Message format
  (multiple-value-bind (second minute hour date month year 
                        day daylight-savings-p zone)
      (if time-zone
          (decode-universal-time time time-zone)
          (decode-universal-time time))
    (let ((zone-value (*& -100 (if daylight-savings-p
                                   (1-& zone)
                                   zone))))
      (format nil 
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

(defun pretty-time-interval (interval-in-seconds)
  ;;; Converts `seconds' to a time interval string (rounded to nearest 
  ;;; 100th of a second).
  (let ((negative-p nil))
    (when (minusp interval-in-seconds)
      (setq negative-p 't
            interval-in-seconds (abs interval-in-seconds)))
    (multiple-value-bind (seconds remainder)
        (floor interval-in-seconds)
      (multiple-value-bind (hundreds)
          (round (* remainder 100))
        ;; handle roundup!
        (when (= 100 hundreds)
          (incf seconds)
          (setq hundreds 0))
        (multiple-value-bind (minutes seconds)
            (floor seconds 60)
          (multiple-value-bind (hours minutes)
              (floor minutes 60)
            (multiple-value-bind (days hours)
                (floor hours 24)
              (let ((days-p (not (zerop days)))
                    (hours-p (not (zerop hours)))
                    (minutes-p (not (zerop minutes)))
                    (seconds-p (not (zerop seconds)))
                    (hundreds-p (not (zerop hundreds))))
                (with-output-to-string (stream)
                  (when negative-p (format stream "-"))
                  (when days-p
                    (format stream "~s day~:p~@[~*, ~]"
                            days (or hours-p minutes-p seconds-p hundreds-p)))
                  (when hours-p
                    (format stream "~s hour~:p~@[~*, ~]"
                            hours (or minutes-p seconds-p hundreds-p)))
                  (when minutes-p
                    (format stream "~s minute~:p~@[~*, ~]"
                            minutes (or seconds-p hundreds-p))) 
                  (when (or seconds-p
                            hundreds-p
                            (and (not minutes-p)
                                 (not hours-p)
                                 (not days-p)))
                    (format stream "~s~:[~*~;.~2,'0d~] second~p" 
                            seconds
                            hundreds-p hundreds 
                            seconds)))))))))))

;;; ---------------------------------------------------------------------------

(defun pretty-run-time-interval (internal-run-time)
  ;;; Converts `internal-run-time' to a time interval string (rounded to
  ;;; nearest 100th of a second).
  (pretty-time-interval (/ internal-run-time 
                           #.(float internal-time-units-per-second))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


