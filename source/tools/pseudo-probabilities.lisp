;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/pseudo-probabilities.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul  3 12:50:35 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Pseudo-Probability Entities
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-29-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Generated fixnum-equivalent operators are exported by DEFDN% (below):
  (export '(*%                          ; normalizing *&
            /%                          ; normalizing /&
            exp%
            pprob2prob
            prob2pprob
            pseudo-probability
            ln%)))

;;; ===========================================================================
;;;   Pseudo-Probabilities
;;;
;;;  A "pseudo probability" represents a [0.0..1.0] value as a fixnum between
;;;  0 and MAX-PPROB (normally [0..1000]).  This provides fast, no allocation,
;;;  computations with discretized probability values.
;;;
;;;  CLISP provides the fewest fixnum bits (24).  In order to keep the value
;;;  of (ln% 1) a fixnum we use 1000 as MAX-PPROB.  (1513 is the largest
;;;  MAX-PPROB value where (ln% 1) can be represented as a 24-bit fixnum, but
;;;  we want to keep MAX-PPROB a power of ten to allow easy visual
;;;  transposition of the decimal point for human translation between real and
;;;  pseudo-probability values.  7743 is the largest MAX-PPROB value where (ln% 1)
;;;  is a 29-bit fixnum.)  

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant max-pprob 1000)
  (defconstant max-pprob$ (float max-pprob))
  (deftype pseudo-probability () `(integer 0 ,max-pprob)))
  
;;; ---------------------------------------------------------------------------

(defun prob2pprob (prob)
  (round$ (*$ (float prob) max-pprob$)))

(defcm prob2pprob (prob)
  `(round$ (*$ (float ,prob) ,max-pprob$)))

;;; ---------------------------------------------------------------------------

(defun pprob2prob (pprob)
  (/$ (float pprob) max-pprob$))

(defcm pprob2prob (pprob)
  `(/$ (float ,pprob) ,max-pprob$))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defdn% (op &optional result values-types)
    (declare (ignore result values-types))
    (let ((pprob-op (intern (concatenate 'simple-string
                              (symbol-name op) "%")
                            ':gbbopen-tools))
          (fixnum-op (intern (concatenate 'simple-string
                               (symbol-name op) "&")
                             ':gbbopen-tools)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (export '(,pprob-op)))
         
         (defun ,pprob-op (&rest args)
           (declare (dynamic-extent args0))
           (apply #',fixnum-op args))
         
         (defcm ,pprob-op (&rest args)
           `(,',fixnum-op ,@args))))))

(defdn% + t)
(defdn% 1+  t)
(defdn% - t)
(defdn% 1- t)
(defdn% =)
(defdn% /=)
(defdn% <)
(defdn% <=)
(defdn% >)
(defdn% >=)
(defdn% min t)
(defdn% max t)
(defdn% zerop)
(defdn% plusp)
(defdn% minusp)
(defdn% evenp)
(defdn% oddp)
(defdn% abs t)
;; Not yet available:
;; (defdn% mod t)
;; (defdn% floor t (fixnum fixnum))
;; (defdn% ceiling t (fixnum fixnum))
;; (defdn% truncate t (fixnum fixnum))
;; (defdn% round t (fixnum fixnum))
;; (defdn% ffloor t (float fixnum))
;; (defdn% fceiling t (float fixnum))
;; (defdn% ftruncate t (float fixnum))
;; (defdn% fround t (float fixnum))

;;; ---------------------------------------------------------------------------

(defun *%-2op (pprob1 pprob2)
  (truncate& (*& pprob1 pprob2) max-pprob))

(defcm *%-2op (pprob1 pprob2)
  `(truncate& (*& ,pprob1 ,pprob2) ,max-pprob))

(defun *% (&rest args)
  (declare (dynamic-extent args))
  (let ((result max-pprob))
    (while args
      (setf result (*%-2op result (pop args))))
    result))

(defcm *% (&rest args)
  (if args
      (let ((first-arg (pop args)))
        (if args
            ;; two or more args:
            `(*%-2op ,first-arg 
                     ,(labels 
                          ((another (args)
                             (declare (dynamic-extent args))
                             (if (rest args)
                                 `(*%-2op ,(first args)
                                          ,(another (rest args)))
                                 (first args))))
                        (another args)))
            ;; one arg:
            first-arg))
      ;; no args:
      max-pprob))

;;; ---------------------------------------------------------------------------

(defun /%-2op (pprob1 pprob2)
  (truncate& (*& pprob1 max-pprob) pprob2))

(defcm /%-2op (pprob1 pprob2)
  `(truncate& (*& ,pprob1 ,max-pprob) ,pprob2))

(defun /% (pprob1 &rest args)
  (declare (dynamic-extent args))
  (if args
      ;; not unary:
      (let ((result pprob1))
        (while args
          (setf result (/%-2op result (pop args))))
        result)
      ;; Unary /% doesn't really make sense, as only (/ MAX-PPROB) returns a
      ;; legal pseudo-probabilty value, but we'll give the user some rope to
      ;; hang themselves with:
      (/%-2op max-pprob pprob1)))

(defcm /% (first-arg &rest args)
  (if args
      ;; two or more args:
      `(/%-2op ,first-arg 
               ,(labels 
                    ((another (args)
                       (declare (dynamic-extent args))
                       (if (rest args)
                           `(/%-2op ,(first args)
                                    ,(another (rest args)))
                           (first args))))
                  (another args)))
      ;; one arg:
      `(/%-2op max-pprob ,first-arg)))

;;; ---------------------------------------------------------------------------

(defvar *ln%-vector* 
    ;; The ln% lookup table
    (let ((vector (make-array (list (1+& max-pprob)))))
      (loop for i fixnum from 1 to max-pprob
          do (setf (svref vector i) 
                   (prob2pprob
                    (*$ max-pprob$ (log (pprob2prob i))))))
      vector))

;;; ---------------------------------------------------------------------------

(defun ln%-error (pprob)
  (error "Unable to take the ~s of ~s"
         'ln%         
         (/$ (float pprob) #.(float max-pprob))))

(defun ln% (pprob)
  ;;; Return the pprob-ln fixnum of pprob:
  (unless (<& 0 pprob #.(1+ max-pprob))
    (ln%-error pprob))
  (svref (the (simple-array t (*)) *ln%-vector*) pprob))

(defcm ln% (pprob)  
  (with-once-only-bindings (pprob)
    `(with-full-optimization ()
         (unless (<& 0 ,pprob #.(1+ max-pprob))
           (ln%-error ,pprob))
       (svref (the (simple-array t (*)) *ln%-vector*) ,pprob))))

;;; ---------------------------------------------------------------------------

(defun exp% (pprob-ln)
  ;;; Return e**pprob-ln where pprob-ln is a fixnum between (ln% 1) and 0
  (with-full-optimization ()
  (unless (<=& (load-time-value (ln% 1)) pprob-ln 0)
    (error "Unable to represent the ~s of ~s as a pseudo-probability"
           'exp%
           pprob-ln))
  ;; Perform reverse lookup in *ln%-vector* using binary search:
  (let ((min 1)
        (max #.(1+ max-pprob))
        i
        result)
    (loop 
      (setf i (+& min (truncate& (-& max min) 2)))
      (setf result (svref (the (simple-array t (*)) *ln%-vector*) i))
      (cond 
       ;; We found the index, return it as its the e**pprob-ln value:
       ((or (>=& i max) 
            (<=& i min))
        (return i))
       ;; Search in lower half:
       ((<& pprob-ln result)
        (setf max i))
       ;; Search in upper half:
       ((>=& pprob-ln result)
        (setf min i)))))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


