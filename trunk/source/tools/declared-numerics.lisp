;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/declared-numerics.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Mar 24 10:21:22 2008 *-*
;;;; *-* Machine: cyclone.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *            Shorthand Declared Numeric Operations and Values
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;
;;;  The implementation-specific numeric type predicates fixnump, 
;;;  single-float-p, and double-float-p need to be imported or defined.
;;;
;;;  Infinity and -infinity values are implementation dependent and must be
;;;  defined for each new port.  These are generally IEEE 754 number-format
;;;  value for infinity (INF) and negative infinity (-INF).  Many
;;;  implementations use read-time evaluation (#.) notation to represent these
;;;  (a non-evaluating, standard reader syntax is sorely needed!).  Lispworks
;;;  uses an extended number syntax with a doubled exponent sign (with the
;;;  second sign positive) to encode infinite values:
;;;     1e++0  positive infinity
;;;    -1e++0  negative infinity
;;;  Lispworks also uses a similar extension (but with a negative second 
;;;  exponent sign) to encode not-a-numbers (NaNs):
;;;     1e+-0  NaN
;;;  The sign of the first exponent sign (+ or -) doesn't matter in
;;;  determining the value (so 1e-+0 is also infinity and 1d--0 is also NaN).
;;;  The type of the value can be encoded just as with any other float
;;;  (1s++0, 1d++0, etc.).  
;;;
;;;  We proposed wider adoption of the Lispworks encoding scheme in other
;;;  Common Lisp implementations and provided patches for CMUCL, OpenMCL, and
;;;  SBCL to use the Lispworks representation.  Our proposal met with mixed
;;;  opinions, and Nikodemus Siivola suggested an alternative approach using a
;;;  #@ dispatch macro.  This is the approach that is currently being used in
;;;  GBBopen, but it has three issues:
;;;    1. Printing infinite values using #@ does not work in Lispworks, as we
;;;       are unable to use print-object (other CLs could eventually share this
;;;       issue)
;;;    2. Digitool MCL has an existing #@ dispatch that we have to work around
;;;    3. Other packages might also want to use the #@ dispatch for other
;;;    purposes
;;;  Until CL implementations "standardize" on a portable, non-read-eval, 
;;;  mechanism for infinite values, this is our best attempt.
;;;
;;;  Most Common Lisp implementations map double-float numbers to the the
;;;  64-bit IEEE 754 double format and single-float numbers to the 32-bit IEEE
;;;  754 single format.
;;;
;;;  Digitool's Macintosh Common Lisp (MCL) maps both the double-float and
;;;  single-float types to the 64-bit IEEE 754 double format (*only* the
;;;  short-float type maps to the IEEE 754 single format).
;;;
;;;  Prior to their 5.0 release, Lispworks supported only the IEEE 754 double
;;;  format, so double-float and single-float type declarations were
;;;  equivalent.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-04-02 File Created.  (Corkill)
;;;  02-20-04 Added full-safety escape feature.  (Corkill)
;;;  03-13-04 Added infinity values.  (Corkill)
;;;  04-13-04 Added SBCL & CMUCL infinity values.  (Corkill)
;;;  05-21-04 Export fixnump, single-float-p, and double-float-p.  (Corkill)
;;;  06-14-04 Changed single-float operation indicator from && to $.  (Corkill)
;;;  01-11-05 Added fixnum infinity values.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  08-23-05 Added coerce&, coerce$, and coerce$$.  (Corkill)
;;;  10-31-05 Add non-*read-eval*, *print-readably* printing for infinity and 
;;;           NaN values for SBCL, CMUCL, & OpenMCL.  (Corkill)
;;;  11-02-05 Added CormanLisp support (faking infinity for now).  (Corkill)
;;;  11-27-05 Changed infinity reading/printing to #@ dispatching macro (as
;;;           suggested by Nikodemus Siivola); no special NaN I/O.  (Corkill)
;;;  01-18-06 Added abs&, abs$, and abs$$.  (Corkill)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  03-24-06 Added infinity-not-available feature.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  11-15-06 Added short-float support.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 
   #+allegro
   '(excl:fixnump excl:single-float-p excl:double-float-p)
   #+clisp
   '(sys::fixnump sys::short-float-p sys::single-float-p sys::double-float-p
     sys::long-float-p)
   #+clozure
   '(ccl:fixnump ccl::short-float-p ccl::double-float-p)
   #+cmu 
   '(extensions:fixnump lisp::short-float-p kernel:single-float-p
     kernel:double-float-p kernel:long-float-p)
   #+cormanlisp
   '(lisp::fixnump lisp::short-float-p lisp::single-float-p
     lisp::double-float-p lisp::long-float-p)
   #+digitool-mcl
   '(ccl:fixnump ccl::double-float-p)
   #+ecl
   '(si:fixnump)
   #+gcl
   '(system:fixnump)
   #+lispworks
   '(lispworks:fixnump lispworks:short-float-p hcl:single-float-p
     hcl:double-float-p lispworks:long-float-p)
   #+openmcl-legacy
   '(ccl:fixnump ccl::short-float-p ccl::double-float-p)
   #+sbcl
   '(sb-int:fixnump sb-int:short-float-p sb-int:single-float-p
     sb-int:double-float-p sb-int:long-float-p)
   #+scl
   '(ext:fixnump kernel:short-float-p kernel:single-float-p
     kernel:double-float-p kernel:long-float-p)
   #-(or allegro clisp clozure cmu cormanlisp digitool-mcl ecl gcl 
         lispworks openmcl-legacy sbcl scl)
   (need-to-port (fixnump short-float-p single-float-p double-float-p
                          long-float-p))))

;;; CLs that don't have short-float-p predicates:
#+(or allegro ecl gcl)
(defun short-float-p (obj)
  (typep obj 'short-float))

#+(and (or allegro ecl gcl) (not full-safety))
(define-compiler-macro short-float-p (obj)
  `(typep ,obj 'short-float))

;;; CLs that don't have single-float-p predicates:
#+(or clozure digitool-mcl ecl gcl openmcl-legacy)
(defun single-float-p (obj)
  (typep obj 'single-float))

#+(and (or clozure digitool-mcl ecl gcl openmcl-legacy)
       (not full-safety))
(define-compiler-macro single-float-p (obj)
  `(typep ,obj 'single-float))

;;; CLs that don't have double-float-p predicates:
#+(or ecl gcl)
(defun double-float-p (obj)
  (typep obj 'double-float))

#+(and (or ecl gcl) (not full-safety))
(define-compiler-macro double-float-p (obj)
  `(typep ,obj 'double-float))

;;; CLs that don't have long-float-p predicates:
#+(or allegro clozure ecl gcl openmcl-legacy)
(defun long-float-p (obj)
  (typep obj 'long-float))

#+(and (or allegro clozure ecl gcl openmcl-legacy)
       (not full-safety))
(define-compiler-macro long-float-p (obj)
  `(typep ,obj 'long-float))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(fixnump short-float-p single-float-p double-float-p long-float-p
	    ;; Coercion:
	    coerce& coerce$& coerce$ coerce$$ coerce$$$
	    ;; Declared fixnum ops:
	    & /& *& +& -& 1+& 1-& 
	    /=& <& <=& =& >& >=& 
	    bounded-value& ceiling& decf& decf&-after evenp& 
	    floor& incf& incf&-after max& min& minusp& 
	    abs& mod& oddp& plusp& round& truncate& zerop&
	    ;; Declared short-float ops:
	    $& /$& *$& +$& -$& 1+$& 1-$&
	    /=$& <$& <=$& =$& >$& >=$& 
	    bounded-value$& ceiling$& decf$& decf$&-after evenp$& 
	    floor$& incf$& incf$&-after max$& min$& minusp$& 
	    abs$& mod$& oddp$& plusp$& round$& truncate$& zerop$&
	    ;; Declared single-float ops:
	    $ /$ *$ +$ -$ 1+$ 1-$
	    /=$ <$ <=$ =$ >$ >=$ 
	    bounded-value$ ceiling$ decf$ decf$-after evenp$ 
	    floor$ incf$ incf$-after max$ min$ minusp$ 
	    abs$ mod$ oddp$ plusp$ round$ truncate$ zerop$
	    ;; Declared double-float ops:
	    $$ /$$ *$$ +$$ -$$ 1+$$ 1-$$ 
	    /=$$ <$$ <=$$ =$$ >$$ >=$$ 
	    bounded-value$$ ceiling$$ decf$$ decf$$-after evenp$$ 
	    floor$$ incf$$ incf$$-after max$$ min$$ minusp$$ 
	    abs$$ mod$$ oddp$$ plusp$$ round$$ truncate$$ zerop$$
	    ;; Declared long-float ops:
	    $$$ /$$$ *$$$ +$$$ -$$$ 1+$$$ 1-$$$ 
	    /=$$$ <$$$ <=$$$ =$$$ >$$$ >=$$$ 
	    bounded-value$$$ ceiling$$$ decf$$$ decf$$$-after evenp$$$ 
	    floor$$$ incf$$$ incf$$$-after max$$$ min$$$ minusp$$$ 
	    abs$$$ mod$$$ oddp$$$ plusp$$$ round$$$ truncate$$$ zerop$$$
	    ;; Infinite values:
	    infinity -infinity infinity& -infinity&
	    infinity$ -infinity$ infinity$& -infinity$&
            infinity$$ -infinity$$ infinity$$$ -infinity$$$
	    ;; Infinity reader escape hook (undocumented):
	    *inf-reader-escape-hook*)))

;;; ---------------------------------------------------------------------------
;;;  Check if various floats are not implemented distinctly (run at compile
;;;  time in order to push features)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun check-for-numeric-type (declared-type feature)
    (let ((actual-type (type-of (coerce 1.0l0 declared-type))))
      (when (eq actual-type declared-type)
        (pushnew feature *features*))))
  (check-for-numeric-type 'short-float ':has-short-float)
  (check-for-numeric-type 'single-float ':has-single-float)
  (check-for-numeric-type 'double-float ':has-double-float)
  (check-for-numeric-type 'long-float ':has-long-float))

;;; ---------------------------------------------------------------------------
;;;  Warn if various floats are not implemented distinctly

(defun warn-numeric-type (declared-type feature)
  (unless (member feature *features*)
    (let ((actual-type (type-of (coerce 1.0l0 declared-type))))
      (warn "~s is equivalent to ~s on ~a~@[ running on ~a~]."
            declared-type
            actual-type
            (lisp-implementation-type) 
            (machine-type)))))
  
(warn-numeric-type 'short-float ':has-short-float)
(warn-numeric-type 'single-float ':has-single-float)
(warn-numeric-type 'double-float ':has-double-float)
(warn-numeric-type 'long-float ':has-long-float)

;;; ---------------------------------------------------------------------------
;;; Allows a function to be called from the #@ inf-reader function, if the
;;; object read is not one of the legal infinite-value names:

(declaim (special *inf-reader-escape-hook*))
(unless (boundp '*inf-reader-escape-hook*)
  (setf *inf-reader-escape-hook* nil))

;;; ===========================================================================
;;;   Fixnum Operations

(defun coerce& (arg) (coerce arg 'fixnum))

#-full-safety
(define-compiler-macro coerce& (arg)
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'fixnum)
	 ,arg
	 (coerce ,arg 'fixnum))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro & (arg)
    ;;; Wraps (the fixnum ...) around `arg'
    `(the #-full-safety fixnum #+full-safety t ,arg))
  
  (flet ((fixnum-op (op args &optional result values-types)
	   ;;; Builds a form declaring all the arguments to OP to be
           ;;; fixnums.  If `result' is true then the result of the
           ;;; operation is also a fixnum unless `values-types' is
	   ;;; specified.
           (let ((form `(,op ,@(mapcar #'(lambda (x) `(& ,x)) args))))
             (if result
                 `(the #-full-safety ,(if values-types
                                          (cons 'values values-types)
                                          'fixnum)
                       #+full-safety t
                    ,form)
                 form))))
    (defmacro +& (&rest args) (fixnum-op '+ args t))
    (defmacro 1+& (&rest args) (fixnum-op '1+ args t))
    (defmacro -& (&rest args) (fixnum-op '- args t))
    (defmacro 1-& (&rest args) (fixnum-op '1- args t))
    (defmacro *& (&rest args) (fixnum-op '* args t))
    (defmacro /& (&rest args) (fixnum-op '/ args t))
    (defmacro =& (&rest args) (fixnum-op '= args))
    (defmacro /=& (&rest args) (fixnum-op '/= args))
    (defmacro <& (&rest args) (fixnum-op '< args))
    (defmacro <=& (&rest args) (fixnum-op '<= args))
    (defmacro >& (&rest args) (fixnum-op '> args))
    (defmacro >=& (&rest args) (fixnum-op '>= args))
    (defmacro min& (&rest args) (fixnum-op 'min args t))
    (defmacro max& (&rest args) (fixnum-op 'max args t))
    (defmacro zerop& (&rest args) (fixnum-op 'zerop args))
    (defmacro plusp& (&rest args) (fixnum-op 'plusp args))
    (defmacro minusp& (&rest args) (fixnum-op 'minusp args))
    (defmacro evenp& (&rest args) (fixnum-op 'evenp args))
    (defmacro oddp& (&rest args) (fixnum-op 'oddp args))
    (defmacro abs& (&rest args) (fixnum-op 'abs args t))
    (defmacro mod& (&rest args) (fixnum-op 'mod args t))
    (defmacro floor& (&rest args) 
      (fixnum-op 'floor args t '(fixnum fixnum)))
    (defmacro ceiling& (&rest args) 
      (fixnum-op 'ceiling args t '(fixnum fixnum)))
    (defmacro truncate& (&rest args)
      (fixnum-op 'truncate args t '(fixnum fixnum)))
    (defmacro round& (&rest args) 
      (fixnum-op 'round args t '(fixnum fixnum))))
  
  (define-modify-macro incf& (&optional (increment 1)) +&)

  (define-modify-macro decf& (&optional (increment 1)) -&)

  (defmacro incf&-after (place &optional (increment 1))
    ;;; Returns the current value of `place' (before the incf is done)
    `(let ((%old-value% ,place))
       (setf ,place (+& %old-value% ,increment))
       %old-value%))
  
  (defmacro decf&-after (place &optional (increment 1))
    ;;; Returns the current value of `place' (before the decf is done)
    `(let ((%old-value% ,place))
       (setf ,place (-& %old-value% ,increment))
       %old-value%))
  
  (defmacro bounded-value& (min n max)
    `(max& ,min (min& ,n ,max))))
 
;; Until GCL fixes local function capture by defmacro:
#+gcl
(defun fixnum-op (op args &optional result values-types)
  (let ((form `(,op ,@(mapcar #'(lambda (x) `(& ,x)) args))))
    (if result
	`(the #-full-safety ,(if values-types
				 (cons 'values values-types)
				 'fixnum)
	      #+full-safety t
	      ,form)
	form)))

;;; ===========================================================================
;;;   Short-Float Operations

(defun coerce$& (arg) (coerce arg 'short-float))

#-full-safety
(define-compiler-macro coerce$& (arg) 
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'short-float)
	 ,arg
	 (coerce ,arg 'short-float))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro $& (arg)
    ;;; Wraps (the short-float ...) around `arg'
    `(the #-full-safety short-float #+full-safety t ,arg))
  
  (flet ((short-float-op (op args &optional result values-types)
	   ;;; Builds a form declaring all the arguments to `op' to be
           ;;; short-floats.  If `result' is true then the result of the
           ;;; operation is also a short-float unless `values-types'
           ;;; is specified.
           (let ((form `(,op ,@(mapcar #'(lambda (x) `($& ,x)) args))))
             (if result
                 `(the #-full-safety ,(if values-types
                                          (cons 'values values-types)
                                          'short-float)
                       #+full-safety t
                    ,form)
                 form))))
    (defmacro +$& (&rest args) (short-float-op '+ args t))
    (defmacro 1+$& (&rest args) (short-float-op '1+ args t))
    (defmacro -$& (&rest args) (short-float-op '- args t))
    (defmacro 1-$& (&rest args) (short-float-op '1- args t))
    (defmacro *$& (&rest args) (short-float-op '* args t))
    (defmacro /$& (&rest args) (short-float-op '/ args t))
    (defmacro =$& (&rest args) (short-float-op '= args))
    (defmacro /=$& (&rest args) (short-float-op '/= args))
    (defmacro <$& (&rest args) (short-float-op '< args))
    (defmacro <=$& (&rest args) (short-float-op '<= args))
    (defmacro >$& (&rest args) (short-float-op '> args))
    (defmacro >=$& (&rest args) (short-float-op '>= args))
    (defmacro min$& (&rest args) (short-float-op 'min args t))
    (defmacro max$& (&rest args) (short-float-op 'max args t))
    (defmacro zerop$& (&rest args) (short-float-op 'zerop args))
    (defmacro plusp$& (&rest args) (short-float-op 'plusp args))
    (defmacro minusp$& (&rest args) (short-float-op 'minusp args))
    (defmacro evenp$& (&rest args) (short-float-op 'evenp args))
    (defmacro oddp$& (&rest args) (short-float-op 'oddp args))
    (defmacro abs$& (&rest args) (short-float-op 'abs args t))
    (defmacro mod$& (&rest args) (short-float-op 'mod args t))
    (defmacro floor$& (&rest args) 
      (short-float-op 'floor args t '(fixnum short-float)))
    (defmacro ceiling$& (&rest args) 
      (short-float-op 'ceiling args t '(fixnum short-float)))
    (defmacro truncate$& (&rest args)
      (short-float-op 'truncate args t '(fixnum short-float)))
    (defmacro round$& (&rest args) 
      (short-float-op 'round args t '(fixnum short-float))))
   
  (define-modify-macro incf$& (&optional (increment 1.0s0)) +$&)

  (define-modify-macro decf$& (&optional (increment 1.0s0)) -$&)

  (defmacro incf$&-after (place &optional (increment 1.0s0))
    ;;; Returns the current value of `place' (before the incf is done)
    `(let ((%old-value% ,place))
       (setf ,place (+$& %old-value% ,increment))
       %old-value%))
  
  (defmacro decf$&-after (place &optional (increment 1.0s0))
    ;;; Returns the current value of `place' (before the decf is done)
    `(let ((%old-value% ,place))
       (setf ,place (-$& %old-value% ,increment))
       %old-value%))
  
  (defmacro bounded-value$& (min n max)
    `(max$& ,min (min$& ,n ,max))))
 
;; Until GCL fixes local function capture by defmacro:
#+gcl
(defun short-float-op (op args &optional result values-types)
  (let ((form `(,op ,@(mapcar #'(lambda (x) `($& ,x)) args))))
    (if result
	`(the #-full-safety ,(if values-types
				 (cons 'values values-types)
				 'short-float)
	      #+full-safety t
	      ,form)
	form)))

;;; ===========================================================================
;;;   Single-Float Operations

(defun coerce$ (arg) (coerce arg 'single-float))

#-full-safety
(define-compiler-macro coerce$ (arg) 
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'single-float)
	 ,arg
	 (coerce ,arg 'single-float))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro $ (arg)
    ;;; Wraps (the single-float ...) around `arg'
    `(the #-full-safety single-float #+full-safety t ,arg))
  
  (flet ((single-float-op (op args &optional result values-types)
	   ;;; Builds a form declaring all the arguments to `op' to be
           ;;; single-floats.  If `result' is true then the result of the
           ;;; operation is also a single-float unless `values-types'
           ;;; is specified.
           (let ((form `(,op ,@(mapcar #'(lambda (x) `($ ,x)) args))))
             (if result
                 `(the #-full-safety ,(if values-types
                                          (cons 'values values-types)
                                          'single-float)
                       #+full-safety t
                    ,form)
                 form))))
    (defmacro +$ (&rest args) (single-float-op '+ args t))
    (defmacro 1+$ (&rest args) (single-float-op '1+ args t))
    (defmacro -$ (&rest args) (single-float-op '- args t))
    (defmacro 1-$ (&rest args) (single-float-op '1- args t))
    (defmacro *$ (&rest args) (single-float-op '* args t))
    (defmacro /$ (&rest args) (single-float-op '/ args t))
    (defmacro =$ (&rest args) (single-float-op '= args))
    (defmacro /=$ (&rest args) (single-float-op '/= args))
    (defmacro <$ (&rest args) (single-float-op '< args))
    (defmacro <=$ (&rest args) (single-float-op '<= args))
    (defmacro >$ (&rest args) (single-float-op '> args))
    (defmacro >=$ (&rest args) (single-float-op '>= args))
    (defmacro min$ (&rest args) (single-float-op 'min args t))
    (defmacro max$ (&rest args) (single-float-op 'max args t))
    (defmacro zerop$ (&rest args) (single-float-op 'zerop args))
    (defmacro plusp$ (&rest args) (single-float-op 'plusp args))
    (defmacro minusp$ (&rest args) (single-float-op 'minusp args))
    (defmacro evenp$ (&rest args) (single-float-op 'evenp args))
    (defmacro oddp$ (&rest args) (single-float-op 'oddp args))
    (defmacro abs$ (&rest args) (single-float-op 'abs args t))
    (defmacro mod$ (&rest args) (single-float-op 'mod args t))
    (defmacro floor$ (&rest args) 
      (single-float-op 'floor args t '(fixnum single-float)))
    (defmacro ceiling$ (&rest args) 
      (single-float-op 'ceiling args t '(fixnum single-float)))
    (defmacro truncate$ (&rest args)
      (single-float-op 'truncate args t '(fixnum single-float)))
    (defmacro round$ (&rest args) 
      (single-float-op 'round args t '(fixnum single-float))))
   
  (define-modify-macro incf$ (&optional (increment 1.0f0)) +$)

  (define-modify-macro decf$ (&optional (increment 1.0f0)) -$)

  (defmacro incf$-after (place &optional (increment 1.0f0))
    ;;; Returns the current value of `place' (before the incf is done)
    `(let ((%old-value% ,place))
       (setf ,place (+$ %old-value% ,increment))
       %old-value%))
  
  (defmacro decf$-after (place &optional (increment 1.0f0))
    ;;; Returns the current value of `place' (before the decf is done)
    `(let ((%old-value% ,place))
       (setf ,place (-$ %old-value% ,increment))
       %old-value%))
  
  (defmacro bounded-value$ (min n max)
    `(max$ ,min (min$ ,n ,max))))
 
;; Until GCL fixes local function capture by defmacro:
#+gcl
(defun single-float-op (op args &optional result values-types)
  (let ((form `(,op ,@(mapcar #'(lambda (x) `($ ,x)) args))))
    (if result
	`(the #-full-safety ,(if values-types
				 (cons 'values values-types)
				 'single-float)
	      #+full-safety t
	      ,form)
	form)))

;;; ===========================================================================
;;;   Double-Float Operations

(defun coerce$$ (arg) (coerce arg 'double-float))

#-full-safety
(define-compiler-macro coerce$$ (arg)
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'double-float)
	 ,arg
	 (coerce ,arg 'double-float))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro $$ (arg)
    ;;; Wraps (the double-float ...) around `arg'
    `(the #-full-safety double-float #+full-safety t ,arg))
  
  (flet ((double-float-op (op args &optional result values-types)
           ;;; Builds a form declaring all the arguments to `op' to be
           ;;; double-floats.  If `result' is true then the result of the
           ;;; operation is also a double-float unless `values-types'
           ;;; is specified.
           (let ((form `(,op ,@(mapcar #'(lambda (x) `($$ ,x)) args))))
             (if result
                 `(the #-full-safety ,(if values-types
                                          (cons 'values values-types)
                                          'double-float)
                       #+full-safety t
                    ,form)
                 form))))
    (defmacro +$$ (&rest args) (double-float-op '+ args t))
    (defmacro 1+$$ (&rest args) (double-float-op '1+ args t))
    (defmacro -$$ (&rest args) (double-float-op '- args t))
    (defmacro 1-$$ (&rest args) (double-float-op '1- args t))
    (defmacro *$$ (&rest args) (double-float-op '* args t))
    (defmacro /$$ (&rest args) (double-float-op '/ args t))
    (defmacro =$$ (&rest args) (double-float-op '= args))
    (defmacro /=$$ (&rest args) (double-float-op '/= args))
    (defmacro <$$ (&rest args) (double-float-op '< args))
    (defmacro <=$$ (&rest args) (double-float-op '<= args))
    (defmacro >$$ (&rest args) (double-float-op '> args))
    (defmacro >=$$ (&rest args) (double-float-op '>= args))
    (defmacro min$$ (&rest args) (double-float-op 'min args t))
    (defmacro max$$ (&rest args) (double-float-op 'max args t))
    (defmacro zerop$$ (&rest args) (double-float-op 'zerop args))
    (defmacro plusp$$ (&rest args) (double-float-op 'plusp args))
    (defmacro minusp$$ (&rest args) (double-float-op 'minusp args))
    (defmacro evenp$$ (&rest args) (double-float-op 'evenp args))
    (defmacro oddp$$ (&rest args) (double-float-op 'oddp args))
    (defmacro abs$$ (&rest args) (double-float-op 'abs args t))
    (defmacro mod$$ (&rest args) (double-float-op 'mod args t))
    (defmacro floor$$ (&rest args) 
      (double-float-op 'floor args t '(fixnum double-float)))
    (defmacro ceiling$$ (&rest args) 
      (double-float-op 'ceiling args t '(fixnum double-float)))
    (defmacro truncate$$ (&rest args)
      (double-float-op 'truncate args t '(fixnum double-float)))
    (defmacro round$$ (&rest args) 
      (double-float-op 'round args t '(fixnum double-float))))
  
  (define-modify-macro incf$$ (&optional (increment 1.0d0)) +$$)

  (define-modify-macro decf$$ (&optional (increment 1.0d0)) -$$)
 
  (defmacro incf$$-after (place &optional (increment 1.0d0))
    ;;; Returns the current value of `place' (before the incf is done)
    `(let ((%old-value% ,place))
       (setf ,place (+$$ %old-value% ,increment))
       %old-value%))
  
  (defmacro decf$$-after (place &optional (increment 1.0d0))
    ;;; Returns the current value of `place' (before the decf is done)
    `(let ((%old-value% ,place))
       (setf ,place (-$$ %old-value% ,increment))
       %old-value%))
  
  (defmacro bounded-value$$ (min n max)
    `(max$$ ,min (min$$ ,n ,max))))
 
;; Until GCL fixes local function capture by defmacro:
#+gcl
(defun double-float-op (op args &optional result values-types)
  (let ((form `(,op ,@(mapcar #'(lambda (x) `($$ ,x)) args))))
    (if result
	`(the #-full-safety ,(if values-types
				 (cons 'values values-types)
				 'double-float)
	      #+full-safety t
	      ,form)
	form)))

;;; ===========================================================================
;;;   Long-Float Operations

(defun coerce$$$ (arg) (coerce arg 'long-float))

#-full-safety
(define-compiler-macro coerce$$$ (arg)
  (with-once-only-bindings (arg)
    ;; avoid coercion if not required (some CLs will coerce anyway):
    `(if (typep ,arg 'long-float)
	 ,arg
	 (coerce ,arg 'long-float))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro $$$ (arg)
    ;;; Wraps (the long-float ...) around `arg'
    `(the #-full-safety long-float #+full-safety t ,arg))
  
  (flet ((long-float-op (op args &optional result values-types)
           ;;; Builds a form declaring all the arguments to `op' to be
           ;;; long-floats.  If `result' is true then the result of the
           ;;; operation is also a long-float unless `values-types'
           ;;; is specified.
           (let ((form `(,op ,@(mapcar #'(lambda (x) `($$$ ,x)) args))))
             (if result
                 `(the #-full-safety ,(if values-types
                                          (cons 'values values-types)
                                          'long-float)
                       #+full-safety t
                    ,form)
                 form))))
    (defmacro +$$$ (&rest args) (long-float-op '+ args t))
    (defmacro 1+$$$ (&rest args) (long-float-op '1+ args t))
    (defmacro -$$$ (&rest args) (long-float-op '- args t))
    (defmacro 1-$$$ (&rest args) (long-float-op '1- args t))
    (defmacro *$$$ (&rest args) (long-float-op '* args t))
    (defmacro /$$$ (&rest args) (long-float-op '/ args t))
    (defmacro =$$$ (&rest args) (long-float-op '= args))
    (defmacro /=$$$ (&rest args) (long-float-op '/= args))
    (defmacro <$$$ (&rest args) (long-float-op '< args))
    (defmacro <=$$$ (&rest args) (long-float-op '<= args))
    (defmacro >$$$ (&rest args) (long-float-op '> args))
    (defmacro >=$$$ (&rest args) (long-float-op '>= args))
    (defmacro min$$$ (&rest args) (long-float-op 'min args t))
    (defmacro max$$$ (&rest args) (long-float-op 'max args t))
    (defmacro zerop$$$ (&rest args) (long-float-op 'zerop args))
    (defmacro plusp$$$ (&rest args) (long-float-op 'plusp args))
    (defmacro minusp$$$ (&rest args) (long-float-op 'minusp args))
    (defmacro evenp$$$ (&rest args) (long-float-op 'evenp args))
    (defmacro oddp$$$ (&rest args) (long-float-op 'oddp args))
    (defmacro abs$$$ (&rest args) (long-float-op 'abs args t))
    (defmacro mod$$$ (&rest args) (long-float-op 'mod args t))
    (defmacro floor$$$ (&rest args) 
      (long-float-op 'floor args t '(fixnum long-float)))
    (defmacro ceiling$$$ (&rest args) 
      (long-float-op 'ceiling args t '(fixnum long-float)))
    (defmacro truncate$$$ (&rest args)
      (long-float-op 'truncate args t '(fixnum long-float)))
    (defmacro round$$$ (&rest args) 
      (long-float-op 'round args t '(fixnum long-float))))
  
  (define-modify-macro incf$$$ (&optional (increment 1.0l0)) +$$$)

  (define-modify-macro decf$$$ (&optional (increment 1.0l0)) -$$$)
 
  (defmacro incf$$$-after (place &optional (increment 1.0l0))
    ;;; Returns the current value of `place' (before the incf is done)
    `(let ((%old-value% ,place))
       (setf ,place (+$$$ %old-value% ,increment))
       %old-value%))
  
  (defmacro decf$$$-after (place &optional (increment 1.0l0))
    ;;; Returns the current value of `place' (before the decf is done)
    `(let ((%old-value% ,place))
       (setf ,place (-$$$ %old-value% ,increment))
       %old-value%))
  
  (defmacro bounded-value$$$ (min n max)
    `(max$$$ ,min (min$$$ ,n ,max))))
 
;;; ===========================================================================
;;;   Infinity Values
;;;
;;;  Infinity values are not required by the CL standard, but most they are
;;;  provided in most CL implementations.  We also want to be able to save and
;;;  communicate infinity values, and using implementation-specific #.
;;;  representations won't work between implementations or if *read-eval*
;;;  is turned off.  We like the text-representation approach used by 
;;;  Lispworks:
;;;     1e++0 => positive infinity
;;;    -1e++0 => negative infinity
;;;     1e+-0 => not-a-number
;;;  but we have been unable to influence a broader adoption in other CL
;;;  implementations.  So, in GBBopen, we resort to a #@ dispatch macro
;;;  mechanism (see below), which has its own problems.  This is an area
;;;  where some defacto standardization is sorely needed.
;;;
;;;  Note: 
;;;    * CLISP, ECL, and GCL do not support IEEE 754 infinity representations

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  #+(or clisp cormanlisp ecl gcl)
  (pushnew :infinity-not-available *features*))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
    
  ;; --------------------------------------------------------------------------
  ;; Fixnum infinity approximations:
  
  (defconstant infinity& most-positive-fixnum)
  (defconstant -infinity& most-negative-fixnum)
  
  ;; --------------------------------------------------------------------------
  ;; Double-float infinities (defined first, so that we can use them in
  ;;                          defining other types, when needed)
  
  (defconstant infinity$$
      #+allegro excl::*infinity-double*
      #+clozure #.(unwind-protect
                      (progn
                        (ccl:set-fpu-mode :division-by-zero nil)
                        (/ 0d0))
                    (ccl:set-fpu-mode :division-by-zero t))
      #+cmu ext:double-float-positive-infinity
      #+digitool-mcl #.(unwind-protect
			   (progn
			     (ccl:set-fpu-mode :division-by-zero nil)
			     (/ 0d0))
			 (ccl:set-fpu-mode :division-by-zero t))
      #+lispworks #.(read-from-string "10E999")
      #+openmcl-legacy #.(unwind-protect
                             (progn
                               (ccl:set-fpu-mode :division-by-zero nil)
                               (/ 0d0))
                           (ccl:set-fpu-mode :division-by-zero t))
      #+sbcl sb-ext:double-float-positive-infinity
      #+scl ext:double-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-double-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy
            sbcl scl infinity-not-available)
      (need-to-port infinity$$))

  (defconstant -infinity$$
      #+allegro excl::*negative-infinity-double*
      #+clozure #.(unwind-protect
                      (progn
                        (ccl:set-fpu-mode :division-by-zero nil)
                        (/ -0d0))
                    (ccl:set-fpu-mode :division-by-zero t))
      #+cmu ext:double-float-negative-infinity
      #+digitool-mcl #.(unwind-protect
			   (progn
			     (ccl:set-fpu-mode :division-by-zero nil)
			     (/ -0d0))
			 (ccl:set-fpu-mode :division-by-zero t))
      #+lispworks #.(read-from-string "-10E999")
      #+openmcl-legacy #.(unwind-protect
                             (progn
                               (ccl:set-fpu-mode :division-by-zero nil)
                               (/ -0d0))
                           (ccl:set-fpu-mode :division-by-zero t))
      #+sbcl sb-ext:double-float-negative-infinity
      #+scl ext:double-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-negative-double-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy 
            sbcl scl infinity-not-available)
      (need-to-port -infinity$$))

  ;; --------------------------------------------------------------------------
  ;; Single-float infinities:
  
  (defconstant infinity$
      #+allegro excl::*infinity-single*
      #+clozure (coerce infinity$$ 'single-float)
      #+cmu ext:single-float-positive-infinity
      #+digitool-mcl (coerce infinity$$ 'single-float)
      #+lispworks (coerce infinity$$ 'single-float)
      #+openmcl-legacy (coerce infinity$$ 'single-float)
      #+sbcl sb-ext:single-float-positive-infinity
      #+scl ext:single-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-single-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy
            sbcl scl infinity-not-available)
      (need-to-port infinity$))
  
  (defconstant -infinity$
      #+allegro excl::*negative-infinity-single*
      #+clozure (coerce -infinity$$ 'single-float)
      #+cmu ext:single-float-negative-infinity
      #+digitool-mcl (coerce -infinity$$ 'single-float)
      #+lispworks (coerce -infinity$$ 'single-float)
      #+openmcl-legacy (coerce -infinity$$ 'single-float)
      #+sbcl sb-ext:single-float-negative-infinity
      #+scl ext:single-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-negative-single-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy
            sbcl scl infinity-not-available)
      (need-to-port -infinity$))
  
  ;; --------------------------------------------------------------------------
  ;; Short-float infinities:
  
  (defconstant infinity$&
      #+allegro infinity$
      #+clozure (coerce infinity$$ 'short-float)
      #+cmu ext:short-float-positive-infinity
      #+digitool-mcl (coerce infinity$$ 'short-float)
      #+lispworks (coerce infinity$$ 'short-float)
      #+openmcl-legacy (coerce infinity$$ 'short-float)
      #+sbcl sb-ext:short-float-positive-infinity
      #+scl ext:short-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-short-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy
            sbcl scl infinity-not-available)
      (need-to-port infinity$&))
  
  (defconstant -infinity$&
      #+allegro -infinity$
      #+clozure (coerce -infinity$$ 'short-float)
      #+cmu ext:short-float-negative-infinity
      #+digitool-mcl (coerce -infinity$$ 'short-float)
      #+lispworks (coerce -infinity$$ 'short-float)
      #+openmcl-legacy (coerce -infinity$$ 'short-float)
      #+sbcl sb-ext:short-float-negative-infinity
      #+scl ext:short-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-negative-short-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy
            sbcl scl infinity-not-available)
      (need-to-port -infinity$&))

  ;; --------------------------------------------------------------------------
  ;; Long-float infinities:
  
  (defconstant infinity$$$
      #+allegro infinity$$
      #+clozure (coerce infinity$$ 'long-float)
      #+cmu ext:long-float-positive-infinity
      #+digitool-mcl (coerce infinity$$ 'long-float)
      #+lispworks (coerce infinity$$ 'long-float)
      #+openmcl-legacy (coerce infinity$$ 'long-float)
      #+sbcl sb-ext:long-float-positive-infinity
      #+scl ext:long-float-positive-infinity
      ;; We have to fake infinity
      #+infinity-not-available most-positive-long-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy
            sbcl scl infinity-not-available)
      (need-to-port infinity$$$))
  
  (defconstant -infinity$$$
      #+allegro -infinity$$
      #+clozure (coerce -infinity$$ 'long-float)
      #+cmu ext:long-float-negative-infinity
      #+digitool-mcl (coerce -infinity$$ 'long-float)
      #+lispworks (coerce -infinity$$ 'long-float)
      #+openmcl-legacy (coerce -infinity$$ 'long-float)
      #+sbcl sb-ext:long-float-negative-infinity
      #+scl ext:long-float-negative-infinity
      ;; We have to fake negative infinity
      #+infinity-not-available most-positive-long-float
      #-(or allegro clozure cmu digitool-mcl lispworks openmcl-legacy
            sbcl sl infinity-not-available)
      (need-to-port -infinity$$$))
  
  ;; --------------------------------------------------------------------------
  ;; Generic infinities:
  
  (defconstant infinity infinity$)
  (defconstant -infinity -infinity$))
  
;;; ---------------------------------------------------------------------------
;;;   INF input & output 
;;;
;;;   Until CL implementations "standardize" on a portable, non-read-eval, 
;;;   mechanism for infinite values, we roll our own using a #@ dispatch macro.
;;;   (Thanks to Nikodemus Siivola for suggesting this approach.)
;;;
;;;   The Lispworks printer does not call the print-object methods, because
;;;   the methods violate the conforming program rules that state that the
;;;   consequences are undefined for a method on a standardized generic
;;;   function which is applicable when all of the arguments are direct
;;;   instances of standardized classes.  We've yet to figure out a work
;;;   around.

(defun inf-reader (stream sub-char infix-parameter)
  (declare (ignore sub-char infix-parameter))
  (let ((what (with-standard-io-syntax	; protect against foolery
		(read stream t nil 't))))
    (flet ((illegal-value-error (what)
	     (error "Illegal infinite-value specifier: #@~s" what)))	 
      (typecase what
	;; Maintain Digitool MCL's #@(<big point>) extension:      
	#+digitool-mcl
	(cons (apply #'ccl::make-big-point what))     
	(symbol 
	 (flet ((symbol-equal (a b)
		  (string-equal (symbol-name a) (symbol-name b))))
	   (cond
	    ((symbol-equal what 'short-float-infinity) infinity$&)
	    ((symbol-equal what 'single-float-infinity) infinity$)
	    ((symbol-equal what 'double-float-infinity) infinity$$)
	    ((symbol-equal what 'long-float-infinity) infinity$$$)
	    ((symbol-equal what 'short-float-negative-infinity) -infinity$&)
	    ((symbol-equal what 'single-float-negative-infinity) -infinity$)
	    ((symbol-equal what 'double-float-negative-infinity) -infinity$$)
	    ((symbol-equal what 'long-float-negative-infinity) -infinity$$$)
	    (*inf-reader-escape-hook*
	     (funcall *inf-reader-escape-hook* what))
	    (t (illegal-value-error what)))))
	(otherwise
	 (if *inf-reader-escape-hook*
	     (funcall *inf-reader-escape-hook* what)
	     (illegal-value-error what)))))))

;;; ---------------------------------------------------------------------------

(safely-set-dispatch-macro-character #\# #\@ 
				     #-cormanlisp 'inf-reader
				     #+cormanlisp #'inf-reader)

;;; ---------------------------------------------------------------------------

;; We don't define infinite-value print-object methods on CLs with faked
;; infinities:
#-infinity-not-available
(let (#+lispworks          ;; Lispworks doesn't like such print-object methods
      (lispworks:*handle-warn-on-redefinition* nil))
  #+has-short-float
  (defmethod print-object ((obj (eql infinity$&)) stream)
    (format stream "#@short-float-infinity")
    obj) 
  #+has-single-float
  (defmethod print-object ((obj (eql infinity$)) stream)
    (format stream "#@single-float-infinity")
    obj) 
  #+has-double-float
  (defmethod print-object ((obj (eql infinity$$)) stream)
    (format stream "#@double-float-infinity")
    obj)
  #+has-long-float
  (defmethod print-object ((obj (eql infinity$$$)) stream)
    (format stream "#@long-float-infinity")
    obj)
  #+has-short-float
  (defmethod print-object ((obj (eql -infinity$&)) stream)
    (format stream "#@short-float-negative-infinity")
    obj)  
  #+has-single-float
  (defmethod print-object ((obj (eql -infinity$)) stream)
    (format stream "#@single-float-negative-infinity")
    obj)  
  #+has-double-float
  (defmethod print-object ((obj (eql -infinity$$)) stream)
    (format stream "#@double-float-negative-infinity")
    obj)
  #+has-long-float
  (defmethod print-object ((obj (eql -infinity$$$)) stream)
    (format stream "#@long-float-negative-infinity")
    obj))

;;; ---------------------------------------------------------------------------

;; From CMUCL's print.lisp:
#+cmu
(lisp::without-package-locks
 (defun lisp::output-float-infinity (x stream)
   (print-object x stream)))

#+scl
(defun lisp::output-float-infinity (x stream)
  (print-object x stream))

;; From SBCL's print.lisp:
#+sbcl
(sb-ext::without-package-locks
 (defun sb-impl::output-float-infinity (x stream)
   (print-object x stream)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

