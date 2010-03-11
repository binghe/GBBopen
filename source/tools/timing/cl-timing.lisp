;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/timing/cl-timing.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Mar 11 05:27:34 2010 *-*
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

(in-package :gbbopen-tools-user)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Import symbols (defined in tools/atable.lisp):
  (import '(gbbopen-tools::*timer-result*
            gbbopen-tools::bsearch-for-transition
            gbbopen-tools::timer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(cl-timing)))

(defparameter *timing-iterations*
    ;; keep interations a fixnum:
    (min most-positive-fixnum 
         ;; Lessen iterations on slower CLs:
         #+clisp
         4000000
         #+ecl
         50000000
         #-(or clisp ecl)
         50000000))

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

(defun determine-memq-transition ()
  (format t "~&;; Memq/hash-table/atable transitions...~%")
  (let* ((max-size 30)
         (list (loop for i fixnum from 1 to max-size collect i))
         (ht (make-keys-only-hash-table-if-supported
              :test 'eq :size max-size))
         (equalp-ht (make-keys-only-hash-table-if-supported
                     :test 'equalp :size max-size))
         (keys-only-atable (make-atable :test 'eq :keys-only 't))
         (equalp-keys-only-atable (make-atable :test 'equalp :keys-only 't))
         (full-keys-only-atable
          (make-atable :test 'eq :keys-only 't :size max-size))
         (full-equalp-keys-only-atable
          (make-atable :test 'equalp :keys-only 't :size max-size)))
    (declare (list list))
    (dotimes (i max-size)
      (declare (fixnum i))
      (let ((key-string (%make-key-string i)))
        (setf (gethash i ht) i)
        (setf (gethash key-string equalp-ht) i)
        (setf (get-entry i full-keys-only-atable) i)
        (setf (get-entry key-string full-equalp-keys-only-atable) i)))
    (setf (get-entry 1 keys-only-atable) 1)
    (setf (get-entry (%make-key-string 1) equalp-keys-only-atable) 1)
    (let ((iterations *timing-iterations*)
          memq-time 
          ht-time
          at-time
          full-at-time)
      (fformat t "~&;;   Fastest memq timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (setf memq-time (timer iterations (car (memq 1 list)))))
      (fformat t "~&;;   Slowest tested memq timing (item ~s -- ~:d iterations)..."
               max-size
               iterations)
      (format t "~6,2f seconds~%"
              (timer iterations (car (memq max-size list))))
      (fformat t "~&;;   Fastest member eq timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (timer iterations (car (member 1 list :test 'eq))))
      #+(or clisp ecl lispworks)
      (format t  "~&;;     *** ~a does not optimize ~
                               (find key list :test 'eq) ***~%"
              (lisp-implementation-type))
      (fformat t "~&;;   Fastest find eq timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (timer iterations (find 1 (the list list) :test 'eq)))              
      (fformat t "~&;;   Fastest member-if eq timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (flet ((fn (item) (eq 1 item)))
                (declare (dynamic-extent #'fn))
                (timer iterations 
                       (car (member-if #'fn list)))))
      (fformat t "~&;;   Eq hash-table (keys only) timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (setf ht-time (timer iterations (gethash 1 ht))))
      (fformat t "~&;;   Fastest eq atable (keys only) timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (setf at-time (timer iterations (get-entry 1 keys-only-atable))))
      (format t "~&;;   Atable overhead (~:d iterations)...~6,2f seconds~%"
              iterations (- at-time memq-time))
      (fformat t "~&;;   Transitioned eq atable (keys only) timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (setf full-at-time
                    (timer iterations (get-entry 1 full-keys-only-atable))))
      (format t "~&;;   Rechecking atable overhead (~:d iterations)...~6,2f seconds~%"
              iterations (- full-at-time ht-time))
      ;; Equalp times:
      (let ((key-string (%make-key-string 1)))
        (fformat t "~&;;   Equalp hash-table (keys only) timing (~:d iterations)..."
                 iterations)
        (format t "~6,2f seconds~%"
                (setf ht-time (timer iterations (gethash key-string equalp-ht))))
        (fformat t "~&;;   Fastest equalp atable (keys only) timing (~:d iterations)..."
                 iterations)
        (format t "~6,2f seconds~%"
                (setf at-time 
                      (timer iterations 
                             (get-entry key-string equalp-keys-only-atable))))
        (format t "~&;;   Atable overhead (~:d iterations)...~6,2f seconds~%"
                iterations (- at-time memq-time))
        (fformat t "~&;;   Transitioned equalp atable (keys only) timing (~:d iterations)..."
                 iterations)
        (format t "~6,2f seconds~%"
                (setf full-at-time
                      (timer iterations
                             (get-entry key-string full-equalp-keys-only-atable))))
        (format t "~&;;   Rechecking atable overhead (~:d iterations)...~6,2f seconds~%"
                iterations (- full-at-time ht-time))))
    ;; Transitions:
    (flet ((memq-lookup (key)
             (declare (fixnum key))
             (cdr (the list (memq key list))))
           (ht-lookup (key)
             (declare (fixnum key))
             (gethash key ht))
           (at-lookup (key)
             (declare (fixnum key))
             (get-entry key keys-only-atable))
           (full-at-lookup (key)
             (declare (fixnum key))
             (get-entry key full-keys-only-atable)))
      (declare (dynamic-extent #'memq-lookup #'ht-lookup 
                               #'at-lookup #'full-at-lookup))
      (format t "~&;;   Memq transition: ~4d~%"
              (bsearch-for-transition
               max-size 1000000 #'memq-lookup #'ht-lookup #'identity))
      (format t "~&;;   Eq atable (keys only) transition: ~4d~%"
              (bsearch-for-transition
               max-size 1000000 #'at-lookup #'ht-lookup #'identity))
      (format t "~&;;   Full eq atable (keys only) transition: ~4d~%"
              (bsearch-for-transition
               max-size 1000000 #'at-lookup #'full-at-lookup #'identity)))))
  
;;; ---------------------------------------------------------------------------

(defun determine-assoc-transition ()
  (format t "~&;; Assoc/hash-table/atable transitions...~%")
  (let* ((max-size 40)
         (alist (loop for i fixnum from 1 to max-size collect (cons i i)))
         (ht (make-hash-table :test 'eq :size max-size))
         (equalp-ht (make-hash-table :test 'equalp :size max-size))
         (atable (make-atable :test 'eq))
         (equalp-atable (make-atable :test 'equalp))
         (full-atable
          (make-atable :test 'equalp :size max-size))
         (full-equalp-atable
          (make-atable :test 'equalp :size max-size)))
    (declare (list alist))
    (dotimes (i max-size)
      (declare (fixnum i))
      (let ((key-string (%make-key-string i)))
        (setf (gethash i ht) i)
        (setf (gethash key-string equalp-ht) i)
        (setf (get-entry i full-atable) i)
        (setf (get-entry key-string full-equalp-atable) i)))
    (setf (get-entry 1 atable) 1)
    (setf (get-entry (%make-key-string 1) atable) 1)
    (let ((iterations *timing-iterations*)
          assoc-time
          ht-time
          at-time
          full-at-time)
      (fformat t "~&;;   Fastest assoc eq timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (setf assoc-time 
                    (timer iterations (cdr (assoc 1 alist :test #'eq)))))
      (fformat t "~&;;   Slowest tested assoc eq timing (item ~s -- ~:d iterations)..."
               max-size
               iterations)
      (format t "~6,2f seconds~%"
              (timer iterations (car (assoc max-size alist :test #'eq))))
      (fformat t "~&;;   Eq hash-table timing (~:d iterations)..."
              iterations)
      (format t "~6,2f seconds~%"
              (setf ht-time (timer iterations (gethash 1 ht))))
      (fformat t "~&;;   Fastest eq atable timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (setf at-time (timer iterations (get-entry 1 atable))))
      (format t "~&;;   Atable overhead (~:d iterations)...~6,2f seconds~%"
               iterations (- at-time assoc-time))
      (fformat t "~&;;   Transitioned eq atable timing (~:d iterations)..."
               iterations)
      (format t "~6,2f seconds~%"
              (setf full-at-time
                    (timer iterations (get-entry 1 full-atable))))
      (format t "~&;;   Rechecking atable overhead (~:d iterations)...~6,2f seconds~%"
              iterations (- full-at-time ht-time))
      ;; Equalp:
      (let ((key-string (%make-key-string 1)))
        (fformat t "~&;;   Equalp hash-table timing (~:d iterations)..."
                 iterations)
        (format t "~6,2f seconds~%"
                (setf ht-time (timer iterations (gethash key-string equalp-ht))))
        (fformat t "~&;;   Fastest equalp atable timing (~:d iterations)..."
                 iterations)
        (format t "~6,2f seconds~%"
                (setf at-time 
                      (timer iterations (get-entry key-string equalp-atable))))
        (format t "~&;;   Atable overhead (~:d iterations)...~6,2f seconds~%"
                iterations (- at-time assoc-time))
        (fformat t "~&;;   Transitioned equalp atable timing (~:d iterations)..."
                 iterations)
        (format t "~6,2f seconds~%"
                (setf full-at-time
                      (timer iterations (get-entry key-string full-equalp-atable))))
        (format t "~&;;   Rechecking atable overhead (~:d iterations)...~6,2f seconds~%"
                iterations (- full-at-time ht-time))))
    ;; Transitions:
    (with-full-optimization ()
      (flet ((assoc-lookup (key)
               (declare (fixnum key))
               (cdr (the list (assoc key alist :test #'eq))))
             (ht-lookup (key)
               (declare (fixnum key))
               (gethash key ht))
             (at-lookup (key)
               (declare (fixnum key))
               (get-entry key atable))
             (full-at-lookup (key)
               (declare (fixnum key))
               (get-entry key full-atable)))
        (declare (dynamic-extent #'assoc-lookup #'ht-lookup 
                                 #'at-lookup #'full-at-lookup))
        (format t "~&;;   Assoc transition: ~4d~%"
                (bsearch-for-transition
                 max-size 1000000 #'assoc-lookup #'ht-lookup #'identity))
        (format t "~&;;   Eq atable transition: ~4d~%"
                (bsearch-for-transition
                 max-size 1000000 #'at-lookup #'ht-lookup #'identity))
        (format t "~&;;   Full eq atable transition: ~4d~%"
                (bsearch-for-transition
                 max-size 1000000 #'at-lookup #'full-at-lookup #'identity))))))

;;; ---------------------------------------------------------------------------

(defun do-division-timing (&optional
                           (numerator 6)
                           (denominator 3))
  (let ((iterations (truncate *timing-iterations* 4)))
    (fformat t "~&;;   Division timing (~:d iterations)..."
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

(defun cl-timing ()
  (format t "~&;; Characters are~@[ not~] eq.~%"
          (not (eval '(eq (code-char 70) (code-char 70)))))
  (format t "~&;; Fixnums are~@[ not~] eq.~%"
          (not (eval '(eq most-positive-fixnum most-positive-fixnum))))
  (format t "~&;; ~50,,,'-<-~>~%")
  (format t "~&;; Starting timings...~%")
  (determine-memq-transition)
  (determine-assoc-transition)
  (do-division-timing)
  (format t "~&;; Timings completed.~%"))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (cl-timing))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
  