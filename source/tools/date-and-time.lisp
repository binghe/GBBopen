;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/date-and-time.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Apr 16 16:17:00 2009 *-*
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
;;;  03-20-04 Added pretty time-duration functions.  (Corkill)
;;;  02-08-09 Added PARSE-DURATION function.  (Corkill)
;;;  03-05-09 Added BRIEF-DURATION, BRIEF-RUN-TIME-DURATION, and
;;;           PARSE-TIME functions.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(module-manager::*month-full-name-vector*
            module-manager::*month-name-vector*
            module-manager:*month-precedes-date*
            module-manager::*weekday-full-name-vector*
            module-manager::*weekday-name-vector*
            module-manager:brief-date
            module-manager:brief-date-and-time
            module-manager::decode-supplied-universal-time
            module-manager:parse-date)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*month-precedes-date*       ; these three entities are defined in 
            brief-date                  ; ../module-manager/module-manager.lisp, 
            brief-date-and-time         ; but are part of :gbbopen-tools
            brief-duration
            brief-run-time-duration
            full-date-and-time
            internet-text-date-and-time
            iso8661-date-and-time
            message-log-date-and-time
            encode-date-and-time        ; not documented yet
            parse-date                  ; also from module-manager.lisp
            parse-date-and-time         ; not documented yet
            parse-duration
            parse-time                  ; not documented yet
            parse-time-interval         ; older, deprecated name
            pretty-duration
            pretty-run-time-duration
            pretty-run-time-interval    ; older, deprecated name
            pretty-time-interval        ; older, deprecated name
            time-zone-offset            ; not documented yet
            )))

;;; ===========================================================================
;;;  Time parsing and formatting

;;; The following globals are defined in ../module-manager/module-manager.lisp:
;;;    (defvar *month-precedes-date* 't) 
;;;    (defparameter *month-name-vector* ...)
;;;    (defparameter *month-full-name-vector* ...)
;;;    (defparameter *weekday-name-vector* ...)
;;;    (defparameter *weekday-full-name-vector* ...)

(defparameter *standard-time-zone-abbreviations*
    ;; Time-zone abbreviations are not unique or universal, and the same hour
    ;; offset can map onto several different zone abbreviations; the following
    ;; choices of supported standard-time abbreviations were made arbitrarily:
    '((1 . "WAT")     ;; West Africa (also Cape Verdes Islands, Atlantic Ocean)
      (2 . "AT")      ;; Azores
      (7/2 . "NST")   ;; Newfoundland
      (4 . "AST")     ;; Atlantic
      (5 . "EST")     ;; Eastern (US)
      (6 . "CST")     ;; Central (US)
      (7 . "MST")     ;; Mountain (US)
      (8 . "PST")     ;; Pacific (US)
      (9 . "AKST")    ;; Alaska
      (10 . "HAST")   ;; Hawaii-Aleutian
      (11 . "NT")     ;; Nome
      (12 . "IDLW")   ;; International Dateline West
      (0 . "GMT")     ;; Greenwich (also Portugal, Reykjavik (Iceland), 
                      ;; Western Africa)
      (-1 . "CET")    ;; Central European (also Algeria, Nigeria, Angola)
      (-2 . "EET")    ;; Eastern European (also Finland, Balkans, Libya, Egypt, 
                      ;; South Africa)
      (-3 . "MSK")    ;; Moscow (also Baghdad, Eastern Africa, Ethiopia, Kenya, 
                      ;; Tanzania)
      (-4 . "ZP4")    ;; Samara (Russia Zone 3)
      (-5 . "ZP5")    ;; Yekaterinburg (Russia Zone 4)
      (-11/2 . "IST") ;; Indian
      (-6 . "ZP6")    ;; Omsk (Russia Zone 5), Bangladesh)
      (-7 . "WAST")   ;; West Austrailian Standard (also Christmas Island, 
                      ;; Krasnoyarsk (Russia Zone 6), Western Indonesia)
      (-8 . "AWST")   ;; Australian Western (also Irkutsk (Russia Zone 7), 
                      ;; China, Hong Kong, Philippines, Central Indonesia)
      (-9 . "JST")    ;; Japan (also Yakutsk (Russia Zone 8), Korea, 
                      ;; Eastern Indonesia)
      (-19/2 . "ACST");; Australian Central
      (-10 . "AEST")  ;; Australian Eastern (also Vladivostok (Russia Zone 9),
                      ;; Papua New Guinea)
      (-21/2 . "NFT") ;; Norfolk (Island)
      (-12 . "NZST")  ;; New Zealand (also Kamchatka (Russia), Fiji, 
                      ;; Marshall Islands)
      ;;; --------------------------------------------------------------------
      ;;;  Additional abbreviations (hourly offset duplicates--used for
      ;;;  decoding only):
      ))

(defparameter *daylight-time-zone-abbreviations*
    ;; Time-zone abbreviations are not unique or universal, and the same hour
    ;; offset can map onto several different zone abbreviations; the following
    ;; choices of supported daylight-savings abbreviations were made
    ;; arbitrarily:
    '((7/2 . "NDT")   ;; Newfoundland Daylight
      (4 . "ADT")     ;; Atlantic Daylight
      (5 . "EDT")     ;; Eastern Daylight (US)
      (6 . "CDT")     ;; Central Daylight (US)
      (7 . "MDT")     ;; Mountain Daylight (US)
      (8 . "PDT")     ;; Pacific Daylight (US)
      (9 . "AKDT")    ;; Alaska Daylight
      (10 . "HADT")   ;; Hawaii-Aleutian Daylight
      (0 . "BST")     ;; British Summer
      (-1 . "CEDT")   ;; Central European Daylight
      (-2 . "EEDT")   ;; Eastern European Daylight
      (-3 . "MSD")    ;; Moscow Daylight
      (-8 . "AWDT")   ;; Australian Western Daylight
      (-19/2 . "ACSD");; Australian Central Daylight
      (-10 . "AEDT") ;; Australian Eastern Daylight
      ;;; --------------------------------------------------------------------
      ;;;  Additional abbreviations (hourly offset duplicates--used for
      ;;;  decoding only):
      ))

;;; ---------------------------------------------------------------------------

(defun junk-in-string-error (string)
  (error "There's junk in this string: ~s" string))

;;; ---------------------------------------------------------------------------

(defun time-zone-abbreviation (zone daylight-savings-p utc-only-p)
  ;;; Return a time-zone abbreviation string for `zone'; `zone' is a rational
  ;;; with DECODE-UNIVERSAL-TIME semantics.
  (or (unless utc-only-p
        (cdr (assoc zone                      
                    (if daylight-savings-p
                        *daylight-time-zone-abbreviations*
                        *standard-time-zone-abbreviations*))))
      ;; No abbreviation from above:
      (let ((utc-zone (if (integerp zone)
                          (-& zone)
                          (-$ (float zone)))))
        (format nil "UTC~:[~@f~;~@d~]"
                (integerp utc-zone)
                (if daylight-savings-p
                    (1+ utc-zone)
                    utc-zone)))))

;;; ---------------------------------------------------------------------------

(defun time-zone-offset (string &key (start 0)
                                     (junk-allowed nil)
                                     (separators " :"))
  ;;; Return a time-zone offset given an abbreviation-string in `string'
  ;;; starting at `start', daylight-savings-p if the abbreviation-string
  ;;; represents a daylight-savings time, and the ending position of the
  ;;; time-zone-offset parse.  The returned time-zone offset is a rational
  ;;; with DECODE-UNIVERSAL-TIME semantics.
  (let ((string-length (length string))
        (daylight-savings-p nil)            
        pos)
    (labels ((skip-separators ()
               (loop 
                   while (and (< (& pos) (& string-length))
                              (find (schar string pos) separators))
                   do (incf& pos)))
             (find-zone (zone-abbr &optional signed-digit-separators)
               (let* ((zone-abbr-length (length zone-abbr))
                      (end (+& start zone-abbr-length)))
                 (when (and (>=& (-& string-length start) zone-abbr-length)
                            (string-equal zone-abbr string :start2 start :end2 end)
                            ;; Must be at end of string or have a following
                            ;; separator (or signed-digit-separator) char:
                            (or (=& end string-length)
                                (find (schar string end) separators)
                                (find (schar string end) 
                                      signed-digit-separators)))
                   (setf pos end)
                   (skip-separators)
                   ;; success:
                   't))))
      (let ((result
             (or (car (rassoc-if 
                       #'find-zone 
                       *standard-time-zone-abbreviations*))
                 (let ((dst-offset 
                        (car (rassoc-if 
                              #'find-zone 
                              *daylight-time-zone-abbreviations*))))
                   (when dst-offset 
                     (setf daylight-savings-p 't)
                     (1-& dst-offset)))
                 (and (find-zone "UTC" "+-")
                      (let (utc-offset
                            (initial-pos pos))
                        (multiple-value-setq (utc-offset pos)
                          (parse-integer string 
                                         :start pos
                                         :junk-allowed 't))
                        (cond 
                         ;; A lone + or - sign is junk:
                         ((and (not utc-offset)
                               (=& (1+& initial-pos) pos))
                          (if junk-allowed
                              (setf pos initial-pos)
                              (junk-in-string-error string)))
                         (t (skip-separators)))
                        (or (and utc-offset (-& utc-offset))
                            0))))))
        ;; skip separators when no zone abbreviation was found:
        (unless result
          (setf pos start)
          (skip-separators))
        ;; check for junk:
        (unless (or junk-allowed 
                    (=& pos string-length))
          (junk-in-string-error string))
        (values result daylight-savings-p pos)))))

;;; ---------------------------------------------------------------------------

(locally
  ;; SBCL (rightly) complains about combining &optional and &key, but we
  ;; ignore that here:
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (defun full-date-and-time (&optional universal-time
                             &key time-zone
                                  daylight-savings-p
                                  all-numeric
                                  full-names
                                  include-day
                                  include-seconds
                                  include-time-zone
                                  (month-precedes-date *month-precedes-date*)
                                  utc-offset-only
                                  12-hour
                                  destination)
    ;;;  Returns formatted date/time string (always includes date and time-of-day)
    (multiple-value-bind (second minute hour date month year
                          day local-daylight-savings-p zone)      
        (decode-supplied-universal-time universal-time time-zone)
      (let ((month-name (if all-numeric
                            month
                            (svref (the (simple-array t (*))
                                     (if full-names
                                         *month-full-name-vector*
                                         *month-name-vector*))
                                   (& (1- (& month))))))
            (am/pm (when 12-hour
                     (cond ((>=& hour 12)
                            (when (>=& hour 13)
                              (decf& hour 12))
                            "PM")
                           (t (when (zerop& hour) (setf hour 12))
                              "AM"))))
            (day-name (when include-day
                        (if full-names
                            (concatenate 'string
                              (svref (the (simple-array t (*))
                                       *weekday-full-name-vector*)
                                     day)
                              ",")
                            (svref (the (simple-array t (*))
                                     *weekday-name-vector*)
                                   day))))
            (time-zone-abbreviation
             (when (or include-time-zone utc-offset-only)
               (time-zone-abbreviation
                zone
                ;; If a `time-zone' was specified, the user must specify
                ;; whether daylight savings is applicable to the decoded
                ;; universal-time; otherwise, we use the local
                ;; daylight-savings-p determined by DECODE-UNIVERSAL-TIME:
                (if time-zone                   
                    daylight-savings-p
                    local-daylight-savings-p)
                utc-offset-only))))
        (if month-precedes-date
            (format destination
                    "~@[~a ~]~:[~a ~2d~@[,~*~] ~a~;~2,'0d/~2,'0d~*/~a~] ~
                     ~2,'0d:~2,'0d~:[~*~;:~2,'0d~]~@[~a~]~@[ ~a~]"
                    day-name
                    all-numeric
                    month-name
                    date
                    full-names
                    year
                    hour
                    minute
                    include-seconds
                    second
                    am/pm
                    time-zone-abbreviation)
            (format destination 
                    "~@[~a ~]~:[~2d ~a~@[,~*~] ~a~;~2,'0d/~2,'0d~*/~a~] ~
                     ~2,'0d:~2,'0d~:[~*~;:~2,'0d~]~@[~a~]~@[ ~a~]"
                    day-name
                    all-numeric
                    date
                    month-name
                    full-names
                    year
                    hour
                    minute
                    include-seconds
                    second
                    am/pm
                    time-zone-abbreviation))))))
  
;;; ---------------------------------------------------------------------------
;;;  message-log-date-and-time

(locally
  ;; SBCL (rightly) complains about combining &optional and &key, but we
  ;; ignore that here:
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (defun message-log-date-and-time (&optional universal-time
                                    &key destination)
    ;; Writes or returns a string representing local time in "message log"
    ;; format: MMM DD HH:MM:SS
    (multiple-value-bind (second minute hour date month)
        (decode-supplied-universal-time universal-time nil)
      (format destination
              "~a ~2,'0d ~2,'0d:~2,'0d:~2,'0d"
              (svref (the (simple-array t (*)) *month-name-vector*)
                     (1-& month))
              date
              hour
              minute
              second))))
  
;;; ---------------------------------------------------------------------------
;;;  ISO8661-date-and-time

(locally
  ;; SBCL (rightly) complains about combining &optional and &key, but we
  ;; ignore that here:
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (defun iso8661-date-and-time (&optional universal-time
                                &key destination)
    ;; Writes or returns a string representing time in ISO8661 (XML dateTime) 
    ;; format
    (multiple-value-bind (second minute hour date month year)
        (decode-supplied-universal-time universal-time 0)
      (format destination
              "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
              year
              month
              date
              hour
              minute
              second))))

;;; ---------------------------------------------------------------------------
;;;  Internet-text-date-and-time

(locally
  ;; SBCL (rightly) complains about combining &optional and &key, but we
  ;; ignore that here:
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (defun internet-text-date-and-time (&optional universal-time
                                      &key time-zone
                                           daylight-savings-p
                                           utc-offset-only
                                           destination)
    ;;; Returns a string representing time in Internet Text Message format
    (multiple-value-bind (second minute hour date month year 
                          day local-daylight-savings-p zone)
        (decode-supplied-universal-time universal-time time-zone)
      (let ((zone-value (*& -100 (if daylight-savings-p
                                     (1-& zone)
                                     zone))))
        (format destination
                "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d ~a~4,'0d~@[ (~a)~]"
                (svref (the (simple-array t (*)) *weekday-name-vector*) 
                       day)
                date
                (svref (the (simple-array t (*)) *month-name-vector*)
                       (1-& month))
                year
                hour
                minute
                second
                ;; we do want -0000!
                (if (plusp& zone-value) "+" "-")
                (abs& zone-value)
                (time-zone-abbreviation
                 zone 
                 ;; If a `time-zone' was specified, the user must specify
                 ;; whether daylight savings is applicable to the decoded
                 ;; universal-time; otherwise, we use the local
                 ;; daylight-savings-p determined by DECODE-UNIVERSAL-TIME:
                 (if time-zone                   
                     daylight-savings-p
                     local-daylight-savings-p)
                 utc-offset-only))))))
  
;;; ---------------------------------------------------------------------------

(defun parse-time (string &key (start 0) 
                               (end (length string))
                               (junk-allowed nil)
                               (separators " :"))
  (declare (simple-string string))
  (flet ((skip-separators ()
           (loop 
               while (and (<& start end)
                          (find (schar string start) separators))
               do (incf& start))))
    (let ((hour 0)
          (minute 0)
          (second 0)
          maybe-pos)
      (skip-separators)
      (when (<& start end)
        (multiple-value-setq (hour start)
          (parse-integer string :start start :end end :junk-allowed 't))
        (setf maybe-pos start))
      (when (and hour (<& start end))
        (skip-separators)
        (multiple-value-setq (minute start)
          (parse-integer string :start start :end end :junk-allowed 't))
        (when minute
          (setf maybe-pos start)
          (when (<& start end)
            (skip-separators)
            (multiple-value-setq (second start)
              (parse-integer string :start start :end end :junk-allowed 't))
            (when second (setf maybe-pos start)))))
      (skip-separators)
      ;; Check for am/pm:
      (when (>=& (-& end start) 2)
        (let ((end2 (+& start 2))) 
          (cond 
           ((and (string-equal "AM" string :start2 start :end2 end2)
                 (or (=& end2 end)
                     (find (schar string end2) separators)))
            (when (=& hour 12) (setf hour 0))
            (incf& start 2)
            (setf maybe-pos start)
            (skip-separators))
           ((and (string-equal "PM" string :start2 start :end2 (+& start 2))
                 (or (=& end2 end)
                     (find (schar string end2) separators)))
            (when (<& hour 12)
              (incf& hour 12))
            (incf& start 2)
            (setf maybe-pos start)
            (skip-separators)))))
      ;; Check for a time-zone offset:
      (multiple-value-bind (time-zone-offset daylight-savings-p tzo-pos)
          (time-zone-offset string 
                            :start start
                            :junk-allowed junk-allowed
                            :separators separators)
        (values (or second 0)
                (or minute 0)
                hour 
                time-zone-offset
                daylight-savings-p
                (if time-zone-offset
                    tzo-pos
                    (if (=& start end) 
                        start
                        (or maybe-pos start))))))))

;;; ---------------------------------------------------------------------------

(defun parse-date-and-time (string
                            &key (start 0) 
                                 (end (length string))
                                 (junk-allowed nil)
                                 (date-separators "-/ ,")
                                 (time-separators " :")
                                 (month-precedes-date *month-precedes-date*))
  (multiple-value-bind (date month year start)
      (parse-date string 
                  :start start :end end 
                  :junk-allowed 't
                  :separators date-separators
                  :month-precedes-date month-precedes-date)
    (multiple-value-bind (second minute hour 
                          time-zone daylight-savings-p start)
        (parse-time string 
                    :start start :end end 
                    :junk-allowed junk-allowed
                    :separators time-separators)
      (values second minute hour date month year 
              time-zone daylight-savings-p start))))

;;; ---------------------------------------------------------------------------

(defun encode-date-and-time (string &rest args &key time-zone)
  (declare (dynamic-extent args))
  (multiple-value-bind (second minute hour date month year 
                        specified-time-zone daylight-savings-p start)
      (apply #'parse-date-and-time string (remove-property args ':time-zone))
    (declare (ignore daylight-savings-p))
    (values
     (cond 
      ;; A time zone was specified in the string:
      (specified-time-zone
       (encode-universal-time second minute hour date month year 
                              specified-time-zone))
      ;; A :time-zone argument was specified in the call:
      (time-zone
       (encode-universal-time second minute hour date month year time-zone))
      ;; Use the local time zone:
      (t (encode-universal-time second minute hour date month year)))
     start)))

;;; ===========================================================================
;;;   Duration entities

(defun time-duration-fields (duration-in-seconds maximum-fields)
  (check-type maximum-fields (integer 1 5))
  (let ((negative-p nil))
    (when (minusp duration-in-seconds)
      (setf negative-p 't)
      (setf duration-in-seconds (abs duration-in-seconds)))
    (multiple-value-bind (seconds remainder)
        (truncate duration-in-seconds)
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
                (values negative-p days-p days hours-p hours 
                        minutes-p minutes seconds-p seconds
                        hundreds-p hundreds)))))))))

;;; ---------------------------------------------------------------------------

(defun brief-duration (duration-in-seconds 
                       &optional (maximum-fields 5)
                                 (destination nil))
  ;;; Converts `seconds' to a brief time-duration string (rounded to the
  ;;; nearest 100th of a second):
  (multiple-value-bind (negative-p days-p days hours-p hours 
                        minutes-p minutes seconds-p seconds
                        hundreds-p hundreds)
      (time-duration-fields duration-in-seconds maximum-fields)
    (flet ((write-it (stream)
             (when negative-p (format stream "-"))
             (when days-p
               (format stream "~sd~@[~* ~]"
                       days 
                       (or hours-p minutes-p seconds-p hundreds-p)))
             (when hours-p 
               (format stream "~sh~@[~* ~]"
                       hours 
                       (or minutes-p seconds-p hundreds-p)))
             (when minutes-p
               (format stream "~sm~@[~* ~]"
                       minutes 
                       (or seconds-p hundreds-p)))
             (cond
              ((or seconds-p hundreds-p)
               (format stream "~s~:[~*~;.~2,'0d~]s" 
                       seconds
                       hundreds-p
                       hundreds 
                       seconds))
              ;; Should only occur when duration-in-seconds is zero:
              ((not (or days-p hours-p minutes-p))
               (format stream "0s")))))
      (if destination
          (write-it destination)
          (with-output-to-string (stream)
            (write-it stream))))))
  
;;; ---------------------------------------------------------------------------

(defun pretty-duration (duration-in-seconds 
                        &optional (maximum-fields 5)
                                  (destination nil))
  ;;; Converts `seconds' to a time duration string (rounded to the nearest
  ;;; 100th of a second):
  (multiple-value-bind (negative-p days-p days hours-p hours 
                        minutes-p minutes seconds-p seconds
                        hundreds-p hundreds)
      (time-duration-fields duration-in-seconds maximum-fields)
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
              ;; Should only occur when duration-in-seconds is zero:
              ((not (or days-p hours-p minutes-p))
               (format stream "0 seconds")))))
      (if destination
          (write-it destination)
          (with-output-to-string (stream)
            (write-it stream))))))
  

;; Old, deprecated, function name:
(defun pretty-time-interval (duration-in-seconds)
  (pretty-duration duration-in-seconds))

(defcm pretty-time-interval (duration-in-seconds)
  `(pretty-duration ,duration-in-seconds))

;;; ---------------------------------------------------------------------------

(defun brief-run-time-duration (internal-run-time 
                                &optional (maximum-fields 5)
                                          (destination nil))
  ;;; Converts `internal-run-time' to a brief time-duration string (rounded to
  ;;; nearest 100th of a second).
  (brief-duration (/ internal-run-time 
                     #.(float internal-time-units-per-second))
                  maximum-fields
                  destination))

;;; ---------------------------------------------------------------------------

(defun pretty-run-time-duration (internal-run-time 
                                 &optional (maximum-fields 5)
                                           (destination nil))
  ;;; Converts `internal-run-time' to a time interval string (rounded to
  ;;; nearest 100th of a second).
  (pretty-duration (/ internal-run-time 
                      #.(float internal-time-units-per-second))
                   maximum-fields
                   destination))

;; Old, deprecated, function name:
(defun pretty-run-time-interval (internal-run-time 
                                 &optional (maximum-fields 5)
                                           (destination nil))
  (pretty-run-time-duration internal-run-time maximum-fields destination))

(defcm pretty-run-time-interval (&rest args)
  `(pretty-run-time-duration ,@args))

;;; ---------------------------------------------------------------------------

(defparameter *time-duration-units-alist*
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

(defun parse-duration (string &key (start 0) 
                                   (end (length string))
                                   (separators " ,")
                                   ;; units keyword is not documented:
                                   (units *time-duration-units-alist*))
  ;; Returns the number of seconds representing the time duration specified in
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
           (error "No duration numeric value found prior to ~s"
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
              (return-from parse-duration (values result start)))
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
                  (error "Unknown time-duration unit ~s following value ~s"
                         (subseq string token-start start)
                         value))
                (setf value (* value (cdr unit-acons)))
                (if negative? (decf result value) (incf result value)))))))
      (values result start))))

;; Old, deprecated, function name:
(defun parse-time-interval (&rest args)
  (declare (dynamic-extent args))
  (apply 'parse-duration args))

(defcm parse-time-interval (&rest args)
  `(parse-duration ,@args))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


