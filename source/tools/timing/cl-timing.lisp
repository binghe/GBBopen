;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/timing/cl-timing.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Apr 15 10:28:51 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        CL Implementation Timing 
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
;;;  07-02-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools-user)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Import symbols (defined in tools/atable.lisp):
  (import '(gbbopen-tools::*atable-transition-sizes*
            gbbopen-tools::%atable-data-list
            gbbopen-tools::atable-data
            gbbopen-tools::auto-transition-margin
            gbbopen-tools::determine-key/value-atable-index
            gbbopen-tools::determine-keys-only-atable-index
            gbbopen-tools::eset-transition-size
            gbbopen-tools::et-transition-size)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(cl-timing)))

;;; ---------------------------------------------------------------------------
;;;   Timing parameters

(defparameter *timing-iterations*
    #+clisp 2000000                     ; CLISP is slow
    #+ecl   6000000                     ; ECL is a bit faster
    #-(or clisp ecl) 
           10000000)

(defparameter *timing-instances* 500000)

(defparameter *max-list-test-size* 100)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *%timing-result%*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro brief-timer (n &body body)
    `(with-full-optimization ()
       ;; Do one untimed trial to prepare everything:
       (setf *%timing-result%* ,@body)
       (let ((%start-time% (get-internal-run-time)))
         (dotimes (i (& ,n))
           (declare (fixnum i))
           (setf *%timing-result%* ,@body))
         (locally (declare (notinline -))
           (- (get-internal-run-time) %start-time%))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro table-timer (n max-index &body body)
    `(with-full-optimization ()
       (let ((test-index 1))
         ;; Do one untimed trial to prepare everything:
         (setf *%timing-result%* ,@body)
         (setf test-index 1)
         (let ((start-time (get-internal-run-time)))
           (dotimes (i (& ,n))
             (declare (fixnum i))
             (setf *%timing-result%* ,@body)
             (incf& test-index)
             (when (>& test-index ,max-index)
               (setf test-index 1)))
           (locally (declare (notinline -))
             (- (get-internal-run-time) start-time)))))))
             
;;; ---------------------------------------------------------------------------

(defun fformat (stream &rest args)
  ;; Format followed by a force-output on the stream:
  (declare (dynamic-extent args))
  (apply #'format stream args)
  (force-output (if (eq stream 't) *standard-output* stream)))

;;; ---------------------------------------------------------------------------

(defun %make-key-string (i)
  (make-string 5 :initial-element (code-char (+& i #.(char-code #\A)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun %hidden-identity 
      ;; Identity function for testing (one that compilers don't know about to
      ;; optimize):
      (x) x))

;;; ---------------------------------------------------------------------------

(defun format-ticks (ticks)
  (format t "~6,2f seconds"
          (locally (declare (optimize (speed 0) (safety 3)))
            (/ ticks
               #.(float internal-time-units-per-second)))))
  
;;; ---------------------------------------------------------------------------

(defun do-list-timing (&optional (iterations *timing-iterations*))
  (let* ((max-size *max-list-test-size*)
         (list (loop for i fixnum from 1 to max-size collect i))
         (alist (loop for i fixnum from 1 to max-size collect (cons i i)))
         (warning-time 0)
         (time 0))
    (declare (type list list alist))
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (brief-timer iterations ,form))
              (format-ticks time)
              (when (and (not (zerop& warning-time))
                         (>& time warning-time))
                (format t " *****"))
              (terpri))))
      (fformat t "~&;;   Do nothing timing (~:d iterations)..."
               iterations)
      (time-it nil)
      (fformat t "~&;;   Fastest memq timing (~:d iterations)..."
               iterations)
      (time-it (car (memq 1 (the list list))))
      (setf warning-time (*& 2 time))
      (fformat t "~&;;   Fastest member eq timing (~:d iterations)..."
               iterations)
      (time-it (car (member 1 (the list list) :test #'eq)))
      (fformat t "~&;;   Fastest member eq key timing (~:d iterations)..."
               iterations)
      (time-it (car (member 1 (the list list)
                            :test #'eq :key #'%hidden-identity)))
      (fformat t "~&;;   Fastest member-if eq timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 item)))
        (declare (dynamic-extent #'fn))
        (time-it (car (member-if #'fn (the list list)))))
      (fformat t "~&;;   Fastest member-if eq key timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 (%hidden-identity item))))
        (declare (dynamic-extent #'fn))
        (time-it (car (member-if #'fn (the list list)))))
      (fformat t "~&;;   Fastest find eq timing (~:d iterations)..."
               iterations)
      (time-it (find 1 (the list list) :test #'eq))
      (fformat t "~&;;   Fastest find eq key timing (~:d iterations)..."
               iterations)
      (time-it (find 1 (the list list) 
                     :test #'eq :key #'%hidden-identity))
      (fformat t "~&;;   Fastest find-if eq timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 item)))
        (declare (dynamic-extent #'fn))
        (time-it (find-if #'fn (the list list))))
      (fformat t "~&;;   Fastest find-if eq key timing (~:d iterations)..."
               iterations)
      (flet ((fn (item) (eq 1 (%hidden-identity item))))
        (declare (dynamic-extent #'fn))
        (time-it (find-if #'fn (the list list))))
      ;; Assoc
      (fformat t "~&;;   Fastest assq timing (~:d iterations)..."
               iterations)
      (time-it (cdr (assq 1 (the list alist))))
      (fformat t "~&;;   Fastest assoc eq timing (~:d iterations)..."
               iterations)
      (time-it (cdr (assoc 1 (the list alist) :test #'eq)))
      (fformat t "~&;;   Fastest assoc eq key timing (~:d iterations)..."
               iterations)
      (time-it (cdr (assoc 1 (the list alist) 
                           :test #'eq
                           :key #'%hidden-identity)))
      (fformat t "~&;;   Fastest assoc-if eq timing (~:d iterations)..."
               iterations)
      (flet ((fn (key) (eq 1 key)))
        (declare (dynamic-extent #'fn))
        (time-it (cdr (assoc-if #'fn (the list alist)))))
      (fformat t "~&;;   Fastest assoc-if eq key timing (~:d iterations)..."
               iterations)
      (flet ((fn (key) (eq 1 (%hidden-identity key))))
        (declare (dynamic-extent #'fn))
        (time-it (cdr (assoc-if #'fn (the list alist)))))
      ;; Deletes
      (setf warning-time 0)
      (fformat t "~&;;   Fastest delq-one timing (~:d iterations)..."
               iterations)
      (let ((cons (list 1)))
        (declare (dynamic-extent cons)
                 (type cons cons))
        (setf (cdr cons) (rest list))
        (time-it (setf (cdr cons) (delq-one 1 (the cons cons)))))
      (setf warning-time (*& 2 time))
      (fformat t "~&;;   Fastest delete eq :count 1 timing (~:d iterations)..."
               iterations)
      (let ((cons (list 1)))
        (declare (dynamic-extent cons)
                 (type cons cons))
        (setf (cdr cons) (rest list))
        (time-it (setf (cdr cons) (delete 1 (the list list)
                                          :test #'eq
                                          :count 1)))))))

;;; ---------------------------------------------------------------------------

(defun do-ht-timing (&optional (iterations *timing-iterations*))
  (let ((keys-only-ht (make-keys-only-hash-table-if-supported :test #'eq))
        (ht (make-hash-table :test #'eq))
        (max-size *max-list-test-size*)
        time)
    (dotimes (i max-size)
      (setf (gethash i keys-only-ht) 't)
      (setf (gethash i ht) 'i))
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (table-timer iterations max-size ,form))
              (format-ticks time)
              (terpri))))
      ;; Hash table timings:
      #-has-keys-only-hash-tables
      (format t "~&;; * Keys-only hash tables are plain hash tables:")
      (fformat t "~&;;   Keys-only eq hash table timing (~:d iterations)..."
               iterations)
      (time-it (gethash 1 ht))
      (fformat t "~&;;   Normal eq hash table timing (~:d iterations)..."
               iterations)
      (time-it (gethash 1 ht)))))

;;; ---------------------------------------------------------------------------

(defun do-eset-timing (&optional (iterations *timing-iterations*))
  (let ((transition #.(max 1 (+ eset-transition-size auto-transition-margin)))
        time)
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (table-timer iterations transition ,form))
              (format-ticks time)
              (terpri))))
      ;; ESET timings:
      #+slower-eset
      (format t "~&;; * ESETs are plain hash tables:")
      (let ((eset-list (make-eset))
            (eset-ht (make-eset))
            (ht (make-keys-only-hash-table-if-supported :test #'eq)))
        ;; Fill them:
        (loop for i fixnum from transition downto 1 do
              (add-to-eset i eset-list))
        (dotimes (i transition)
          (add-to-eset i eset-ht)
          (setf (gethash i ht) 't))
        ;; Time them:
        (fformat t "~&;;   Do nothing timing (~:d iterations)..."
                 iterations)
        (time-it nil)
        (fformat t "~&;;   Fastest ESET (list) timing (~:d iterations)..."
                 iterations)
        (time-it (in-eset 1 eset-list))
        (fformat t "~&;;   Slowest ESET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (in-eset transition eset-list))
        (fformat t "~&;;   Average ESET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (in-eset test-index eset-list))
        (fformat t "~&;;   ESET (transitioned) timing (~:d iterations)..."
                 iterations)
        (time-it (in-eset test-index eset-ht))
        (fformat t "~&;;   eq ~:[key/value~;keys-only~] hash table timing ~
                           (~:d iterations)..."
                 (memq ':has-keys-only-hash-tables *features*)
                 iterations)
        (time-it (gethash test-index ht))))))
  
;;; ---------------------------------------------------------------------------

(defun do-et-timing (&optional (iterations *timing-iterations*))
  (let ((transition #.(max 1 (+ et-transition-size auto-transition-margin)))
        time)
    (macrolet
        ((time-it (form)
           `(progn
              (setf time (table-timer iterations transition ,form))
              (format-ticks time)
              (terpri))))
      ;; ET timings:
      #+slower-et
      (format t "~&;; * ETs are plain hash tables:")
      (let ((et-list (make-et))
            (et-ht (make-et))
            (ht (make-hash-table :test #'eq)))
        ;; Fill them:
        (loop for i fixnum from transition downto 1 do
              (setf (get-et i et-list) i))
        (dotimes (i transition)
          (setf (get-et i et-ht) i)
          (setf (gethash i ht) 't))
        ;; Time them:
        (fformat t "~&;;   Do nothing timing (~:d iterations)..."
                 iterations)
        (time-it nil)
        (fformat t "~&;;   Fastest ET (list) timing (~:d iterations)..."
                 iterations)
        (time-it (get-et 1 et-list))
        (fformat t "~&;;   Slowest ET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (get-et transition et-list))
        (fformat t "~&;;   Average ET (list @~d) timing (~:d iterations)..."
                 transition
                 iterations)
        (time-it (get-et test-index et-list))
        (fformat t "~&;;   ET (transitioned) timing (~:d iterations)..."
                 iterations)
        (time-it (get-et test-index et-ht))
        (fformat t "~&;;   eq key/value hash table timing (~:d iterations)..."
                 iterations)
        (time-it (gethash test-index ht))))))

;;; ---------------------------------------------------------------------------

(defun do-division-timing (&optional
                           (numerator 6)
                           (denominator 3))
  (declare (fixnum numerator denominator))
  (let ((iterations (truncate& *timing-iterations* 2)))
    (with-full-optimization ()
      (fformat t "~&;;   Division timing (~:d iterations)..."
               iterations)
      (fformat t "~&;;     /&:       ")
      (format-ticks (brief-timer iterations
                                 (/& numerator denominator)))
      (fformat t "~%;;     floor&:   ")
      (format-ticks (brief-timer iterations
                                 (floor& numerator denominator)))
      (fformat t "~%;;     truncate&:")
      (format-ticks (brief-timer iterations
                                 (truncate& numerator denominator))))))

;;; ---------------------------------------------------------------------------

;;  CMUCL & SCL require these definitions at compile time:
(eval-when (#+(or cmu scl) :compile-toplevel :load-toplevel :execute)

  (define-class test-instance ()
    (slot
     (non-cloned-slot :initform 1)))
  
  (define-class test-instance-clone (test-instance)
    (slot 
     (new-slot :initform -1))))

;;; ---------------------------------------------------------------------------

(defun do-instance-timing (&optional (size *timing-instances*))
  (declare (fixnum size))
  (let ((list nil)
        (warning-time 0))
    (fformat t "~&;;   Make-instance timing (~:d instances)..." size)
    (let ((start-time (get-internal-run-time)))
      (with-full-optimization ()
        (dotimes (i size)
          (declare (fixnum i))
          (push (make-instance 'test-instance :slot i) list)))
      (let ((time (- (get-internal-run-time) start-time)))
        (setf warning-time (*& time 2))
        (format-ticks time)))
    (fformat t "~&;;   Change-class timing (~:d instances)..." size)
    (let ((start-time (get-internal-run-time)))
      (with-full-optimization ()
        (dolist (instance list)
          (change-class instance
                        (load-time-value (find-class 'test-instance-clone)))))
      (let ((time (- (get-internal-run-time) start-time)))
        (format-ticks time)
        (when (>& time warning-time)
          (format t " *****"))))))
    
;;; ===========================================================================
;;;  Compute the size transition value for ESETs

#-slower-eset
(defun compute-eset-transition (&optional (verbose? 't))
  (let ((iterations 
         #+clisp
         100000
         #+ecl
         1000000
         #-(or clisp ecl)
         10000000)
        (max-size *max-list-test-size*)
        (transition 0)
        (contiguous-misses 0)
        (eset)
        (ht (make-keys-only-hash-table-if-supported :test #'eq)))
    (with-full-optimization ()
      ;; Initialize values
      (setf eset (cons max-size
                     (loop for i fixnum from 1 to max-size
                         collect i
                         do (setf (gethash i ht) 't))))
      ;; Time to time:
      (flet 
          ((time-eset (max-index)
             (let ((start-time (get-internal-run-time))
                   (test-index 1))
               (dotimes (i iterations)
                 (declare (fixnum i))
                 (setf *%timing-result%* (in-eset test-index eset))
                 (incf& test-index)
                 (when (>& test-index max-index)
                   (setf test-index 1)))
               (locally (declare (notinline -))
                 (- (get-internal-run-time) start-time))))
           (time-ht (max-index)
             (let ((start-time (get-internal-run-time))
                   (test-index 1))
               (dotimes (i iterations)
                 (declare (fixnum i))
                 (setf *%timing-result%* (gethash test-index ht))
                 (incf& test-index)
                 (when (>& test-index max-index)
                   (setf test-index 1)))
               (locally (declare (notinline -))
                 (- (get-internal-run-time) start-time)))))
        (loop for i fixnum from 1 to max-size do
              (let ((time-eset (time-eset i))
                    (time-ht (time-ht i)))
                (declare (fixnum time-eset time-ht))
                (when verbose?
                  (format t "~&;; i: ~d, eset: ~d, ht: ~d~%"
                          i time-eset time-ht))
                (cond ((<=& time-eset time-ht)
                       (setf contiguous-misses 0)
                       (setf transition i))
                      ((>=& (incf& contiguous-misses) 5)
                       (return)))))
        (when verbose?
          (format t "~&;; ESET transition: ~d~%" transition))
        transition))))
        
;;; ---------------------------------------------------------------------------
;;;  Compute the size transition value for ETs

#-slower-et
(defun compute-et-transition (&optional (verbose? 't))
  (let ((iterations 
         #+clisp
         100000
         #+ecl
         1000000
         #-(or clisp ecl)
         10000000)
        (max-size *max-list-test-size*)
        (transition 0)
        (contiguous-misses 0)
        (et)
        (ht (make-hash-table :test #'eq)))
    (with-full-optimization ()
      ;; Initialize values
      (setf et (cons max-size
                     (loop for i fixnum from 1 to max-size
                         collect (cons i i)
                         do (setf (gethash i ht) i))))
      ;; Time to time:
      (flet 
          ((time-et (max-index)
             (let ((start-time (get-internal-run-time))
                   (test-index 1))
               (dotimes (i iterations)
                 (declare (fixnum i))
                 (setf *%timing-result%* (get-et test-index et))
                 (incf& test-index)
                 (when (>& test-index max-index)
                   (setf test-index 1)))
               (locally (declare (notinline -))
                 (- (get-internal-run-time) start-time))))
           (time-ht (max-index)
             (let ((start-time (get-internal-run-time))
                   (test-index 1))
               (dotimes (i iterations)
                 (declare (fixnum i))
                 (setf *%timing-result%* (gethash test-index ht))
                 (incf& test-index)
                 (when (>& test-index max-index)
                   (setf test-index 1)))
               (locally (declare (notinline -))
                 (- (get-internal-run-time) start-time)))))
        (loop for i fixnum from 1 to max-size do
              (let ((time-ht (time-ht i))
                    (time-et (time-et i)))
                (declare (fixnum time-et time-ht))
                (when verbose?
                  (format t "~&;; i: ~d, et: ~d, ht: ~d~%"
                          i time-et time-ht))
                (cond ((<=& time-et time-ht)
                       (setf contiguous-misses 0)
                       (setf transition i))
                      ((>=& (incf& contiguous-misses) 5)
                       (return)))))
        (when verbose?
          (format t "~&;; ET transition: ~d~%" transition))
        transition))))
        
;;; ---------------------------------------------------------------------------
;;;  Compute the size transition values where hash-tables outperform lists

#-slow-atable
(defun compute-atable-transitions (&optional (verbose? 't))
  ;; Optional `verbose?' value can be :very for detailed output...
  (let ((iterations 
         #+clisp
         100000
         #-clisp
         1000000)
        (max-size *max-list-test-size*)
        (computed-sizes-vector
         (make-array '(10) :initial-element 0))
        (contiguous-misses 0)
        (very-verbose? (eq verbose? ':very)))
    (flet ((string-it (i)
             (make-string 2 :initial-element (code-char (+& i 32)))))
      (macrolet
          ((do-it (test keys-only keygen-fn)
             (let ((atable-index 
                    (if keys-only
                        (determine-keys-only-atable-index test)
                        (determine-key/value-atable-index test))))
               (flet ((build-at/ht-timer (name reader)
                        `(,name (max-index at/ht)
                                ;; Do one untimed trial to prepare everything:
                                (setf *%timing-result%* 
                                      (,reader (svref keys 1) at/ht))
                                (let ((test-index 1)
                                      (start-time (get-internal-run-time)))
                                  (setf test-index 1)
                                  (dotimes (i iterations)
                                    (declare (fixnum i))
                                    (setf *%timing-result%*
                                          (,reader (svref keys test-index) at/ht))
                                    (incf& test-index)
                                    (when (>& test-index max-index)
                                      (setf test-index 1)))
                                  (locally (declare (notinline -))
                                    (- (get-internal-run-time) start-time))))))
                 `(let ((atl (make-atable :test ',test
                                          :keys-only ,keys-only))
                        (ath (make-atable :test ',test
                                          :keys-only ,keys-only
                                          :size (1+& max-size)))
                        (ht (make-hash-table :test ',test))
                        (keys (make-array (list (1+& max-size)))))
                    (with-full-optimization ()
                      (setf (%atable-data-list (atable-data atl))
                            ;; Fill tables (list in increasing key order):
                            (loop for i fixnum from 1 to max-size 
                                for key = (funcall ,keygen-fn i)
                                collect ,(if keys-only 'key '(cons key i))
                                do (setf (get-entry key ath) i)
                                   (setf (gethash key ht) i)))
                      (when verbose?
                        (format t "~&;; Timing ~s~@[ (keys only)~*~] "
                                ',test 
                                ,keys-only))
                      ;; Time to time:
                      (flet (,(build-at/ht-timer 'time-at 'get-entry)
                             ,(build-at/ht-timer 'time-ht 'gethash))
                        (setf contiguous-misses 0)
                        (loop for i fixnum from 1 to max-size do
                              (let ((time-atl (time-at i atl))
                                    (time-ath (when very-verbose?
                                                (time-at i ath)))
                                    (time-ht (time-ht i ht)))
                                (declare (fixnum time-atl time-ht))
                                (when (and verbose? (not very-verbose?))
                                  (write-char #\.)
                                  (force-output))
                                (when very-verbose?
                                  (format t "~&;; i: ~d, atl: ~d, ath: ~d, ht: ~d~%"
                                          i time-atl time-ath time-ht))
                                (cond 
                                 ((<=& time-atl time-ht)
                                  (setf contiguous-misses 0)
                                  (setf (svref computed-sizes-vector ,atable-index)
                                        i))
                                 ((>=& (incf& contiguous-misses) 5)
                                  (when verbose?
                                    (format t " transition: ~d~%"
                                            (svref computed-sizes-vector ,atable-index)))
                                  (return))))))))))))
        (do-it eq t #'identity)
        (do-it eq nil #'identity)
        (do-it eql t #'identity)
        (do-it eql nil #'identity)
        (do-it equal t #'string-it)
        (do-it equal nil #'string-it)
        (do-it equalp t #'string-it)
        (do-it equalp nil #'string-it)
        computed-sizes-vector))))
  
;;; ---------------------------------------------------------------------------
;;;  Check Clozure CL's combined-method-hash-table threshold value

#+clozure
(defun check-combined-method-hash-table-threshold ()
  (format t "~&;; Computing Clozure CL's combined-method-hash-threshold...~%")
  (let ((threshold
         (truncate$
          (*$ 0.75 
              (float 
               (let ((*standard-output* 
                      ;; Run silently (to the null stream):
                      (make-broadcast-stream)))
                 (ccl::compute-eql-combined-method-hash-table-threshold)))))))
    (format t "~&;; Computed ~s value: ~d (current value is ~d)~%"
            'ccl::*eql-combined-method-hash-table-threshold*
            threshold
            ccl::*eql-combined-method-hash-table-threshold*)
    (unless (<& (abs& (-& ccl::*eql-combined-method-hash-table-threshold*
                          threshold))
                2)
      (format t "~&;; ***** For top performance, set ~s to ~s"
              'ccl::*eql-combined-method-hash-table-threshold*
              threshold))))

;;; ---------------------------------------------------------------------------

(defun cl-timing ()
  (format t "~&;; ~50,,,'-<-~>~%")
  (format t "~&;; Characters are~@[ not~] eq.~%"
          (not (eval '(eq (code-char 70) (code-char 70)))))
  (format t "~&;; Fixnums are~@[ not~] eq.~%"
          (not (eval '(eq most-positive-fixnum most-positive-fixnum))))
  ;;; Check the threshold on Clozure CL:
  #+(and clozure ignore)
  (check-combined-method-hash-table-threshold)
  ;; Do the timing:
  (format t "~&;; ~50,,,'-<-~>~%")
  (format t "~&;; Starting timings...~%")
  (do-list-timing)
  (format t "~&;;~%")
  (do-ht-timing)
  (format t "~&;;~%")
  (do-division-timing)
  (format t "~&;;~%")
  (do-eset-timing)
  (format t "~&;;~%")
  (do-et-timing)
  (format t "~&;;~%")
  (do-instance-timing)
  (format t "~&;; Timings completed.~%"))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (cl-timing))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
  