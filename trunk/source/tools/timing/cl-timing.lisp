;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/timing/cl-timing.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Mar  3 19:26:21 2010 *-*
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
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-02-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-user)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(cl-timing)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *test-value*)
  
  (defmacro timer (n &body body)
    `(let ((%start-time% (get-internal-real-time)))
       (with-full-optimization ()
           (dotimes (i (the fixnum ,n))
             (declare (fixnum i)) 
             (setf *test-value* ,@body)))
       (/ (- (get-internal-real-time) %start-time%)
          (load-time-value (float internal-time-units-per-second))))))

;;; ---------------------------------------------------------------------------

(defun do-division-timing (&optional
                           (numerator 6)
                           (denominator 3))
  (let ((iterations 
         ;; keep interations a fixnum (on CLISP):
         (min most-positive-fixnum 200000000)))
    (format t "~&;;   Division timing (~:d iterations)..."
            iterations)
    (let ((times
           `(,(timer iterations (/& numerator denominator))
             ,(timer iterations (floor& numerator denominator))
             ,(timer iterations (truncate& numerator denominator)))))
      (format t 
             "~{~&;;     /&:       ~6,2f seconds~
                ~%;;     floor&:   ~6,2f seconds~
                ~%;;     truncate&:~6,2f seconds~%~}"
             times))))

;;; ---------------------------------------------------------------------------

(defun bsearch-for-transition (max-value timer-iterations
                               sprint-fn marathon-fn)
  (declare (function sprint-fn marathon-fn))
  (let ((min-value 1)
        (test-value (floor& max-value 2)))
    (dotimes (i 50)
      (declare (fixnum i))
      (when (=& min-value test-value)
        (return-from bsearch-for-transition test-value))
      (let ((sprint-time 
             (timer timer-iterations (funcall sprint-fn test-value)))
            (marathon-time
             (timer timer-iterations (funcall marathon-fn test-value))))
        (if (<$ sprint-time marathon-time)
            (setf min-value test-value)
            (setf max-value test-value))
        (setf test-value (+& min-value (floor& (-& max-value min-value) 2)))))))

;;; ---------------------------------------------------------------------------

(defun determine-memq-transition ()
  (let* ((max-size 25)
         (list (loop for i fixnum from 1 to max-size 
                   collect (code-char (+& i #.(char-code #\A)))))
         (ht (make-keys-only-hash-table-if-supported
              :size max-size :test 'eq)))
    (dotimes (i max-size)
      (declare (fixnum i))
      (setf (gethash (code-char (+& i #.(char-code #\A))) ht) i))
    (flet ((memq-lookup (i)
             (let ((key (code-char (+& i #.(char-code #\A)))))
               (cdr (memq key list))))
           (ht-lookup (i)
             (let ((key (code-char (+& i #.(char-code #\A)))))
               (gethash key ht))))
      (declare (dynamic-extent #'memq-lookup #'ht-lookup))
      (format t "~&;;   Memq transition: ~4d~%"
              (bsearch-for-transition
               max-size 1000000 #'memq-lookup #'ht-lookup)))))

;;; ---------------------------------------------------------------------------

(defun determine-assoc-transition ()
  (let* ((max-size 25)
         (alist (loop for i fixnum from 1 to max-size collect (cons i i)))
         (ht (make-hash-table :size max-size)))
    (dotimes (i max-size)
      (declare (fixnum i))
      (setf (gethash i ht) i))
    (flet ((assoc-lookup (item)
             (cdr (assoc item alist)))
           (ht-lookup (item)
             (gethash item ht)))
      (declare (dynamic-extent #'assoc-lookup #'ht-lookup))
      (format t "~&;;   Assoc transition: ~4d~%"
              (bsearch-for-transition
               max-size 1000000 #'assoc-lookup #'ht-lookup)))))

;;; ---------------------------------------------------------------------------

(defun cl-timing ()
  (format t "~&;;; Starting timings...")
  (do-division-timing)
  (format t "~&;;; Determining transition points...")
  (determine-memq-transition)
  (determine-assoc-transition)
  (format t "~&;;; Timings completed.~%"))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (cl-timing))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
  