;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/timing/cl-timing.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Mar 13 16:25:12 2010 *-*
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
  (import '(gbbopen-tools::*brief-timer-result*
            gbbopen-tools::bsearch-for-transition
            gbbopen-tools::brief-timer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(cl-timing)))

;;; ---------------------------------------------------------------------------
;;;   Timing parameters

(defparameter *timing-iterations*
    ;; Timing interation must be a fixnum divisible by 100 -- use fewer
    ;; iterations on slower CLs:
    #+clisp 4000000                     ; CLISP is slow
    #+ecl 50000000                      ; ECL is a bit faster
    #-(or clisp ecl) 50000000)

(defparameter *max-list-test-size* 30)

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

(defun do-list-timing ()
  (let* ((max-size *max-list-test-size*)
         (iterations *timing-iterations*)
         (list (loop for i fixnum from 1 to max-size collect i))
         (alist (loop for i fixnum from 1 to max-size collect (cons i i))))
    (declare (type list list alist))
    (fformat t "~&;;   Do nothing timing (~:d iterations)..."
             iterations)
    (format t "~6,2f seconds~%"
            (brief-timer iterations nil))
    (fformat t "~&;;   Fastest memq timing (~:d iterations)..."
             iterations)
    (format t "~6,2f seconds~%"
            (brief-timer iterations (car (memq 1 (the list list)))))
    (fformat t "~&;;   Fastest member eq timing (~:d iterations)..."
             iterations)
    (format t "~6,2f seconds~%"
            (brief-timer iterations 
                         (car (member 1 (the list list) :test #'eq))))
    #+(or clisp ecl lispworks)
    (format t  "~&;;     *** ~a does not optimize ~
                             (find key list :test #'eq) ***~%"
            (lisp-implementation-type))
    (fformat t "~&;;   Fastest find eq timing (~:d iterations)..."
             iterations)
    (format t "~6,2f seconds~%"
            (brief-timer iterations 
                         (find 1 (the list list) :test #'eq)))
    (fformat t "~&;;   Fastest member-if eq timing ~
                                         (~:d iterations)..."
                       iterations)
    (format t "~6,2f seconds~%"
            (flet ((fn (item) (eq 1 item)))
              (declare (dynamic-extent #'fn))
              (brief-timer iterations 
                           (car (member-if #'fn (the list list))))))
    ;; Assoc
    (fformat t "~&;;   Fastest assoc eq timing (~:d iterations)..."
             iterations)
    (format t "~6,2f seconds~%"
            (brief-timer iterations 
                         (cdr (assoc 1 (the list alist) :test #'eq))))
    ;; Deletes
    (fformat t "~&;;   Fastest delq-one timing (~:d iterations)..."
             iterations)
    (format t "~6,2f seconds~%"
            (let ((cons (list 1)))
              (declare (dynamic-extent cons)
                       (type cons cons))
              (setf (cdr cons) (rest list))
              (brief-timer iterations 
                           (setf (cdr cons) (delq-one 1 (the cons cons))))))
    #+(or clisp clozure cmu ecl lispworks sbcl scl)
    (format t  "~&;;     *** ~a does not optimize ~
                             (delete key list :test #'eq :count 1) ***~%"
            (lisp-implementation-type))
    (fformat t "~&;;   Fastest delete eq :count 1 timing (~:d iterations)..."
             iterations)
    (format t "~6,2f seconds~%"
            (let ((cons (list 1)))
              (declare (dynamic-extent cons)
                       (type cons cons))
              (setf (cdr cons) (rest list))
              (brief-timer iterations 
                           (setf (cdr cons) (delete 1 (the list list)
                                                    :test #'eq
                                                    :count 1)))))))

;;; ---------------------------------------------------------------------------

(macrolet
    ((build-atable-transition-fn (keys-only?)
       (let ((ht-maker (if keys-only?
                           'make-keys-only-hash-table-if-supported
                           'make-hash-table))
             (memq/assoc (if keys-only? "memq" "assoc")))
         `(defun ,(if keys-only? 
                      'determine-memq-transition 
                      'determine-assoc-transition) 
              ()
            (format t "~&;; ~a/hash-table/atable transitions...~%"
                    ,(string-capitalize memq/assoc))
            (let* ((max-size *max-list-test-size*)
                   (list (loop for i fixnum from 1 to max-size collect 
                               ,(if keys-only? 'i '(cons i i))))
                   (ht (,ht-maker :test 'eq :size max-size))
                   (equalp-ht (,ht-maker :test 'equalp :size max-size))
                   (atable (make-atable :test 'eq :keys-only ,keys-only?))
                   (equalp-atable
                    (make-atable :test 'equalp :keys-only ,keys-only?))
                   (full-atable
                    (make-atable :test 'eq :keys-only ,keys-only?
                                 :size max-size))
                   (full-equalp-atable
                    (make-atable :test 'equalp :keys-only ,keys-only?
                                 :size max-size)))
              (declare (list list))
              (dotimes (i max-size)
                (declare (fixnum i))
                (let ((key-string (%make-key-string i)))
                  (setf (gethash i ht) i)
                  (setf (gethash key-string equalp-ht) i)
                  (setf (get-entry i full-atable) i)
                  (setf (get-entry key-string full-equalp-atable) i)))
              (setf (get-entry 1 atable) 1)
              (setf (get-entry (%make-key-string 1) equalp-atable) 1)
              (let ((iterations *timing-iterations*)
                    memq/assoc-time 
                    ht-time
                    at-time
                    full-at-time)
                (fformat t "~&;;   Repeat fastest ~a timing (~:d iterations)..."
                         ,memq/assoc
                         iterations)
                (format t "~6,2f seconds~%"
                        (setf memq/assoc-time 
                              (brief-timer
                               iterations 
                               ,(if keys-only?
                                    '(car (memq 1 list))
                                    '(cdr (assoc 1 list :test #'eq))))))
                (fformat t "~&;;   Slowest tested ~a timing ~
                                   (item ~s -- ~:d iterations)..."
                         ,memq/assoc
                         max-size
                         iterations)
                (format t "~6,2f seconds~%"
                        (brief-timer
                         iterations
                         ,(if keys-only?
                              '(car (memq 1 list))
                              '(cdr (assoc 1 list :test #'eq)))))
                (fformat t "~&;;   Eq hash-table~@[ (keys only)~*~] timing ~
                                   (~:d iterations)..."
                         ,keys-only?
                         iterations)
                (format t "~6,2f seconds~%"
                        (setf ht-time (brief-timer iterations (gethash 1 ht))))
                (fformat t "~&;;   Fastest eq atable~@[ (keys only)~*~] timing ~
                                   (~:d iterations)..."
                         ,keys-only?
                         iterations)
                (format t "~6,2f seconds~%"
                        (setf at-time 
                              (brief-timer iterations (get-entry 1 atable))))
                (format t "~&;;   Atable overhead (~:d iterations)...~
                                  ~6,2f seconds~%"
                        iterations
                        (- at-time memq/assoc-time))
                (fformat t "~&;;   Transitioned eq atable~@[ (keys only)~*~] ~
                                   timing (~:d iterations)..."
                         ,keys-only?
                         iterations)
                (format t "~6,2f seconds~%"
                        (setf full-at-time
                              (brief-timer iterations (get-entry 1 full-atable))))
                (format t "~&;;   Rechecking atable overhead (~:d iterations)...~
                                  ~6,2f seconds~%"
                        iterations (- full-at-time ht-time))
                ;; Equalp times:
                (let ((key-string (%make-key-string 1)))
                  (fformat t "~&;;   Equalp hash-table~@[ (keys only)~*~] ~
                                     timing (~:d iterations)..."
                           ,keys-only?
                           iterations)
                  (format t "~6,2f seconds~%"
                          (setf ht-time 
                                (brief-timer iterations 
                                             (gethash key-string equalp-ht))))
                  (fformat t "~&;;   Fastest equalp atable~@[ (keys only)~*~] ~
                                     timing (~:d iterations)..."
                           ,keys-only?
                           iterations)
                  (format t "~6,2f seconds~%"
                          (setf at-time 
                                (brief-timer 
                                 iterations 
                                 (get-entry key-string equalp-atable))))
                  (format t "~&;;   Atable overhead (~:d iterations)...~
                                    ~6,2f seconds~%"
                          iterations
                          (- at-time memq/assoc-time))
                  (fformat t "~&;;   Transitioned equalp atable~
                                     ~@[ (keys only)~*~] timing ~
                                     (~:d iterations)..."
                           ,keys-only?
                           iterations)
                  (format t "~6,2f seconds~%"
                          (setf full-at-time
                                (brief-timer 
                                 iterations
                                 (get-entry key-string full-atable))))
                  (format t "~&;;   Rechecking atable overhead ~
                                    (~:d iterations)...~6,2f seconds~%"
                          iterations 
                          (- full-at-time ht-time))))
              ;; Transitions:
              (flet ((list-lookup (key)
                       (declare (fixnum key))
                       ,(if keys-only? 
                            '(car (memq key list))
                            '(cdr (assoc key list :test #'eq))))
                     (ht-lookup (key)
                       (declare (fixnum key))
                       (gethash key ht))
                     (at-lookup (key)
                       (declare (fixnum key))
                       (get-entry key atable))
                     (full-at-lookup (key)
                       (declare (fixnum key))
                       (get-entry key full-atable)))
                (declare (dynamic-extent #'list-lookup #'ht-lookup 
                                         #'at-lookup #'full-at-lookup))
                (format t "~&;;   ~a transition: ~4d~%"
                        ,(string-capitalize memq/assoc)
                        (bsearch-for-transition
                         max-size 1000000 #'list-lookup #'ht-lookup 
                         #'identity))
                (format t "~&;;   Eq atable~@[ (keys only)~*~] ~
                                  transition: ~4d~%"
                        ,keys-only?
                        (bsearch-for-transition
                         max-size 1000000 #'at-lookup #'ht-lookup 
                         #'identity))
                (format t "~&;;   Full eq atable~@[ (keys only)~*~] ~
                                  transition: ~4d~%"
                        ,keys-only?
                        (bsearch-for-transition
                         max-size 1000000 #'at-lookup #'full-at-lookup 
                         #'identity))))))))
  ;; Build keys-only transition function:
  (build-atable-transition-fn 't)
  ;; Build keys/value transition function:
  (build-atable-transition-fn nil))

;;; ---------------------------------------------------------------------------

(defun do-division-timing (&optional
                           (numerator 6)
                           (denominator 3))
  (let ((iterations (truncate *timing-iterations* 4)))
    (fformat t "~&;;   Division timing (~:d iterations)..."
             iterations)
    (let ((times
           `(,(brief-timer iterations (/& numerator denominator))
             ,(brief-timer iterations (floor& numerator denominator))
             ,(brief-timer iterations (truncate& numerator denominator)))))
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
  (do-list-timing)
  (determine-memq-transition)
  (determine-assoc-transition)
  (do-division-timing)
  (format t "~&;; Timings completed.~%"))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (cl-timing))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
  