;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/pseudo-probabilities.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul  3 02:44:23 2008 *-*
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
  (export '(+%
            -%
            *%
            /%
            exp%
            pprob2prob
            prob2pprob
            pseudo-probability
            ln%)))

;;; ===========================================================================
;;;   Pseudo-Probabilities
;;;

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

(defun +% (&rest args)
  (declare (dynamic-extent args0))
  (apply #'+ args))

(defcm +% (&rest args)
  `(+& ,@args))

;;; ---------------------------------------------------------------------------

(defun -% (&rest args)
  (declare (dynamic-extent args0))
  (apply #'- args))

(defcm -% (&rest args)
  `(-& ,@args))

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

(defun unary-/%-error (pprob1)
  (error "Unary /% can only be called with ~s, ~s was provided."
         max-pprob
         pprob1))

(defun /% (pprob1 &rest args)
  (declare (dynamic-extent args))
  (cond 
   ;; not unary:
   (args
    (let ((result pprob1))
      (while args
        (setf result (/%-2op result (pop args))))
      result))
   ;; unary doesn't really make sense, as only max-pprob is legal:
   ((=& max-pprob pprob1) max-pprob)
   (t (unary-/%-error pprob1))))

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
      (with-once-only-bindings (first-arg)
        `(if (=& ,max-pprob ,first-arg)
             ,max-pprob
             (unary-/%-error ,first-arg)))))

;;; ---------------------------------------------------------------------------

(defvar *ln%-vector* 
    ;; The ln% lookup table
    (let ((vector (make-array (list (1+& max-pprob))))
          (multiplier (float max-pprob)))
      (loop for i fixnum from 1 to max-pprob
          do (setf (svref vector i) 
                   (prob2pprob
                    (*$ multiplier (log (pprob2prob i))))))
      vector))

;;; ---------------------------------------------------------------------------

(defun ln%-error (pprob)
  (error "Unable to take the ln% of ~s"
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
    (error "Unable to represent exp% of ~s as a pseudo-probability"
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


