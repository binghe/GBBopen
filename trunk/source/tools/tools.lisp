;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/tools.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jun 25 13:58:59 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                          Useful Lisp Extensions
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
;;;   MOP class-finalization functions used in ensure-finalized-class must be
;;;   imported. The MOP specializer-name extraction function and eql handling
;;;   used in undefmethod must also be addressed.
;;;
;;;   Print-pretty-function-object can be customized for a CL implementation.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-04-02 File created.  (Corkill)
;;;  03-10-04 Added PUSHNEW/INCF-ACONS.  (Corkill)
;;;  03-21-04 Added REMOVE-PROPERTIES.  (Corkill)
;;;  04-30-04 Added SET-EQUAL.  (Corkill)
;;;  05-10-04 Added DO-UNTIL.  (Corkill)
;;;  05-24-04 Added MACROLET-DEBUG.  (Corkill)
;;;  05-31-04 Improve COUNTED-DELETE and SET-EQUAL.  (Corkill)
;;;  06-06-04 Added ENSURE-LIST-OF-LISTS.  (Corkill)
;;;  07-08-04 Added XOR.  (Corkill)
;;;  07-15-04 Added READ-CHAR-IMMEDIATELY.  (Corkill)
;;;  05-27-04 Added SETS-OVERLAP-P.  (Corkill)
;;;  06-01-05 Added PRINT-PRETTY-FUNCTION-OBJECT.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  11-02-05 Added CormanLisp support.  (Corkill)
;;;  11-30-05 Rewrote LIST-LENGTH=1 as LIST-LENGTH-1-P.  (Corkill)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  03-11-06 Depreciated ASSURE-LIST.  (Corkill)
;;;  03-12-06 Added LIST-LENGTH-2-P.  (Corkill)
;;;  03-18-06 Added DOSEQUENCE.  (Corkill)
;;;  04-07-06 Added SHUFFLE-LIST.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  08-20-06 Added EXTRACT-DECLARATIONS.  (Corkill)
;;;  09-22-06 Added CormanLisp 3.0 support.  (Corkill)
;;;  12-05-07 Added SHRINK-VECTOR.  (Corkill)
;;;  01-06-08 Added LIST-LENGTH>1.  (Corkill)
;;;  01-09-08 Added LIST-LENGTH> and TRIMMED-SUBSTRING.  (Corkill)
;;;  02-29-08 Added handler-forms and error-condition lexical function to 
;;;           WITH-ERROR-HANDLING.  (Corkill)
;;;  02-09-08 Added NICER-Y-OR-N-P and NICER-YES-OR-NO-P.  (Corkill)
;;;  05-01-08 Added DECF/DELETE-ACONS.  (Corkill)
;;;  05-25-08 Added MULITPLE-VALUE-SETF.  (Corkill)
;;;  06-01-08 Added COMPILER-MACROEXPAND-1 and COMPILER-MACROEXPAND.  (Corkill)
;;;  06-25-08 Added :conditions option to WITH-ERROR-HANDLING and exclude
;;;           handling EXCL::INTERRUPT-SIGNAL on Allegro by default.  (Corkill) 
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

;;; ---------------------------------------------------------------------------
;;;  Import routinely available entities, whenever possible

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 
   #+allegro 
   '(excl::extract-declarations
     excl:interrupt-signal
     excl::memq
     excl:until
     excl:while
     excl::xor
     sys:copy-file)
   #+clisp
   '(posix::copy-file
     system::memq)
   #+clozure
   '(ccl:compiler-macroexpand
     ccl:compiler-macroexpand-1
     ccl:copy-file
     ccl:memq
     ccl:delq)
   #+cmu
   '(ext:compiler-macroexpand
     ext:compiler-macroexpand-1
     ext:memq
     ext:delq)
   #+cormanlisp
   '()
   #+digitool-mcl
   '(ccl:compiler-macroexpand
     ccl:compiler-macroexpand-1
     ccl:copy-file
     ccl:memq
     ccl:delq)
   ;; Note: ECL's while doesn't include a NIL block, so we can't use it
   #+ecl
   '(si:memq)
   #+gcl
   '()
   #+lispworks
   '(harlequin-common-lisp:compiler-macroexpand
     harlequin-common-lisp:compiler-macroexpand-1
     system::copy-file
     system:memq
     system:delq)
   #+sbcl
   '(sb-int:memq
     sb-int:delq)
   #+scl
   '(ext:compiler-macroexpand
     ext:compiler-macroexpand-1
     ext:memq
     ext:delq)
   #-(or allegro clisp clozure cmu cormanlisp digitool-mcl ecl gcl 
         lispworks sbcl scl)
   '()))

;;; ---------------------------------------------------------------------------
;;;  Exported tools entities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(bounded-value
            copy-file                   ; not yet documented
            compiler-macroexpand
            compiler-macroexpand-1
	    counted-delete
	    decf-after			; not yet finished or documented
	    decf/delete-acons
	    decf/delete&-acons
	    decf/delete$-acons
	    decf/delete$$-acons
	    decf/delete$$$-acons
	    define-directory		; in mini-module, but part of tools
	    delq
	    do-until
	    dosequence
	    dotted-conc-name		; in mini-module, but part of tools
	    dotted-length
	    error-condition             ; lexical fn in with-error-handling
            error-message               ; lexical fn in with-error-handling
	    ensure-finalized-class
	    ensure-list
	    ensure-list-of-lists	; not yet documented
	    extract-declarations	; not documented
	    incf-after                  ; not yet finished or documented
            interrupt-signal
	    list-length-1-p
	    list-length-2-p
	    list-length>
	    list-length>1
	    macrolet-debug              ; not documented
	    make-keyword
	    memq
            multiple-value-setf
            nicer-y-or-n-p              ; not yet documented
            nicer-yes-or-no-p           ; not yet documented
	    nsorted-insert
	    print-pretty-function-object ; not yet documented
	    push-acons
	    pushnew-acons
	    pushnew/incf-acons
	    pushnew/incf&-acons
	    pushnew/incf$-acons
	    pushnew/incf$$-acons
	    pushnew/incf$$$-acons
	    pushnew-elements
	    read-char-immediately	; not yet documented
	    remove-property
	    remove-properties
	    set-equal
	    sets-overlap-p
	    shuffle-list
            shrink-vector
	    sole-element
	    splitting-butlast
	    trimmed-substring
	    undefmethod
	    until
	    while
	    with-error-handling
	    xor)))

;;; ===========================================================================
;;;  Basic while and until macros

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro while (test &body body)
    `(loop (unless ,test (return))
       ,@body)))

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro until (test &body body)
    `(loop (when ,test (return))
       ,@body)))

(defmacro do-until (form test)
  `(loop ,form (when ,test (return))))

;;; ===========================================================================
;;;  Compiler-macroexpand (for those CL's that don't provide it)

#-(or clozure cmu digitool-mcl lispworks scl)
(defun compiler-macroexpand-1 (form &optional env)
  (let ((compiler-macro-function 
         (and (consp form)
              (symbolp (car form))
              (compiler-macro-function (car form)))))
    (if compiler-macro-function
        (let ((expansion (funcall compiler-macro-function form env)))
          (values expansion (not (eq form expansion))))
        (values form nil))))

;;; ---------------------------------------------------------------------------

#-(or clozure cmu digitool-mcl lispworks scl)
(defun compiler-macroexpand (form &optional env)
  (multiple-value-bind (expansion expanded-p)
      (compiler-macroexpand-1 form env)
    (let ((expanded-at-least-once expanded-p))
      (while expanded-p
        (multiple-value-setq (expansion expanded-p)
          (compiler-macroexpand-1 expansion env)))
      (values expansion expanded-at-least-once))))

;;; ===========================================================================
;;;  Multiple-value-setf

(defmacro multiple-value-setf (places form)
  ;;; Like multiple-value-setq, but works with places.  A "place" of nil means
  ;;; to ignore the corresponding value from `form'.  Returns the primarly
  ;;; value of evaluating `form'.
  (loop 
      for place in places
      for name = (gensym)
      collect name into bindings
      if (eql 'nil place)
        unless (eq place (first places))
          collect `(declare (ignore ,name)) into ignores
        end                                         
      else
        collect `(setf ,place ,name) into body
      finally (return `(multiple-value-bind ,bindings ,form
                         ,@ignores
                         ,@body
                         ;; Return the primary value (like multiple-value-setq)
                         ,(first bindings)))))

;;; ===========================================================================
;;;  Memq (lists only)

#-(or allegro
      clisp
      clozure
      cmu
      digitool-mcl
      ecl
      lispworks
      sbcl 
      scl)
(progn
  (defun memq (item list)
    (declare (list list))
    (member item list :test #'eq))
  
  (defcm memq (item list)
    `(member ,item (the ,list ,list) :test #'eq)))

;;; ===========================================================================
;;;  Delq (lists only)

#+allegro
(progn
  (defun delq (item list)
    (excl::list-delete-eq item list))
  
  (defcm delq (item list)
    `(excl::list-delete-eq ,item ,list)))

#-(or allegro clozure cmu digitool-mcl lispworks sbcl scl)
(progn
  (defun delq (item list)
    (declare (list list))
    (delete item list :test #'eq))
  
  (defcm delq (item list)
    `(delete ,item (the list ,list) :test #'eq)))

;;; ===========================================================================
;;;  Copy-file (for CLs that don't provide their own version)

#-(or allegro clisp clozure digitool-mcl lispworks)
(defun copy-file (from to)
  (with-open-file (output to
                   :element-type 'unsigned-byte
                   :direction ':output
                   :if-exists ':supersede)
    (with-open-file (input from
                     :element-type 'unsigned-byte
                     :direction ':input)
      (with-full-optimization ()
        (let (byte)
          (loop (setf byte (read-byte input nil nil))
            (unless byte (return))
            (write-byte byte output)))))))

;;; ===========================================================================
;;;  Extract-declarations (for CLs that don't provide their own version)

#-(or allegro)
(defun extract-declarations (body)
  ;; Return three values: 
  ;;  1. doc string, if present
  ;;  2. list of declarations
  ;;  3. remaining body
  (let ((doc-string nil)
        (declarations nil))
    (loop
      (let ((elt (first body)))
	(cond ((and (consp elt) (eq (first elt) 'declare))
	       (push (pop body) declarations))
	      ((and (null doc-string)
		    (stringp elt)
		    ;; to be a doc string, there must be another form in body:
		    (cdr body))
	       (setf doc-string (pop body)))
	      (t (return)))))
    (values doc-string (nreverse declarations) body)))

;;; ===========================================================================
;;;  Dosequence (based on James Anderson's mod of Thomas Burdick's version):

(defmacro dosequence ((var sequence &optional result) &body forms)
  (with-gensyms (end-p fun)
    `(block nil
       (flet ((,fun (,var ,end-p)
                (tagbody
                  (when ,end-p (go ,end-p))
                  ,@forms
                  (return-from ,fun nil)
                  ,end-p
                  (return-from ,fun ,result))))
         (map nil #'(lambda (element) (,fun element nil))
              ,sequence)
         ,@(when result `((,fun nil t))))))) 

;;; ===========================================================================
;;;  Nicer y-or-n-p and yes-or-no-p (add initial help to Allegro & CMUCL
;;;  versions:

(defun nicer-y-or-n-p (&optional control-string &rest args)
  (declare (dynamic-extent args))
  (apply #'y-or-n-p 
         #+(or allegro cmu)
         (when control-string
           (format nil "~a[y or n] " control-string))
         #-(or allegro cmu)
         control-string
         args))

;;; ---------------------------------------------------------------------------

(defun nicer-yes-or-no-p (&optional control-string &rest args)
  (declare (dynamic-extent args))
  (apply #'yes-or-no-p 
         #+(or allegro cmu)
         (when control-string
           (format nil "~a[yes or no] " control-string))
         #-(or allegro cmu)
         control-string
         args))

;;; ===========================================================================
;;;  With-error-handling
;;;
;;;  Evaluates body if an error occurs while evaluating form

(defun error-message-string (condition)                            
  (let ((*print-readably* nil))
    (handler-case (format nil "~a" condition)
      (error () "<error: unprintable condition>"))))

;;; ---------------------------------------------------------------------------

(defmacro with-error-handling (form-and-handler &body error-body)
  ;;; Full signature:
  ;;;  (with-error-handling {form | 
  ;;;                        (form [(:conditions condition*)] handler-form*)}
  ;;;      error-form*)
  (let ((conditions 
         #+allegro '(and error (not interrupt-signal))
         #-allegro 'error))
    ;; Determine if form-and-handler is form or (form &body handler-body):
    (unless (and (consp form-and-handler)
                 (consp (car form-and-handler))
                 ;; Support CLHS 3.1.2.1.2.4 lambda form:
                 (not (eq (caar form-and-handler) 'lambda)))
      ;; Convert a simple form to a form with (null) handler-body:
      (setf form-and-handler (list form-and-handler)))
    (destructuring-bind (form &body handler-body)
        form-and-handler
      ;; Check handler-body for :conditions option:
      (when (and handler-body
                 (consp (first handler-body))
                 (eq ':conditions (first (first handler-body))))
        (setf conditions (sole-element (rest (first handler-body))))
        (setf handler-body (rest handler-body)))
      ;; Now generate the handler:
      (let ((block (gensym))
            (condition/tag (when error-body (gensym))))
        `(block ,block
           (let (,@(when error-body (list condition/tag)))
             (tagbody
               (handler-bind
                   ((,conditions
                     #'(lambda (condition)
                         ,@(if handler-body
                               `((flet ((error-message ()
                                          (error-message-string condition))
                                        (error-condition ()
                                          condition))
                                   (declare (dynamic-extent error-message
                                                            error-condition))
                                   #-sbcl
                                   (declare (ignorable error-message
                                                       error-condition))
                                   ,@(if error-body
                                         `(,@handler-body
                                           ;; Save the condition for use
                                           ;; by (error-message) in error-body:
                                           (setf ,condition/tag condition)
                                           (go ,condition/tag))
                                         `((return-from ,block 
                                             (progn ,@handler-body))))))
                               `(,@(if error-body
                                       `(,@handler-body
                                         ;; Save the condition for use by
                                         ;; (error-message) in error-body:
                                         (setf ,condition/tag condition)
                                         (go ,condition/tag))
                                       `((declare (ignore condition))
                                         (return-from ,block (values)))))))))
                 (return-from ,block ,form))
               ,@(when error-body (list condition/tag))
               ,@(when error-body
                   `((flet ((error-message ()
                              (error-message-string ,condition/tag))
                            (error-condition ()
                              ,condition/tag))
                       (declare (dynamic-extent error-message error-condition))
                       #-sbcl
                       (declare (ignorable error-message error-condition))
                       (return-from ,block (progn ,@error-body))))))))))))
  
;;; ===========================================================================
;;;  Ensure-finalized-class

(defun ensure-finalized-class (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  class)

(defcm ensure-finalized-class (class)
  (with-once-only-bindings (class)
    `(progn
       (unless (class-finalized-p ,class)
	 (finalize-inheritance ,class))
       ,class)))

;;; ===========================================================================
;;;  Make-keyword

(defun make-keyword (string-symbol-or-character)
  (intern (string string-symbol-or-character)  
	  (load-time-value (find-package 'keyword))))

(defcm make-keyword (string-symbol-or-character)
  `(intern (string ,string-symbol-or-character) 
	   (load-time-value (find-package 'keyword))))

;;; =========================================================================== 
;;;  Ensure-list 

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defcm ensure-list (x)
  (with-once-only-bindings (x)
    `(if (listp ,x) ,x (list ,x))))

;;; ===========================================================================
;;;  Ensure-list-of-lists

(defun ensure-list-of-lists (x)
  (let ((x (ensure-list x)))
    (if (listp (car x)) x (list x))))

(defcm ensure-list-of-lists (x)
  (with-once-only-bindings (x)
    `(if (listp (car ,x)) ,x (list ,x))))

;;; ===========================================================================
;;;   Sole-element
;;;
;;;   Like first, but signals an error of more than 1 element is present
;;;   in the list.

(defun sole-element (list)
  (prog1 (first list)
    (when (rest list)
      (sole-element-violation list))))

(defcm sole-element (list)
  (with-once-only-bindings (list)
    `(prog1 (first ,list)
       (when (rest ,list)
	 (sole-element-violation ,list)))))
  
(defun sole-element-violation (list)
  (cerror "Ignore the remaining elements."
          "The list ~s contains more than 1 element."
          list))

;;; ===========================================================================
;;;   Shrink-vector
;;;
;;;   Destructively truncates a simple vector (when the CL implementation
;;;   supports it--some implementations allocate a new vector anyway)

(defun shrink-vector (vector length)
  #+allegro
  (excl::.primcall 'sys::shrink-svector vector length)
  ;; Can we do better on CLISP?
  #+clisp
  (if (=& length (length vector))
      vector
      (subseq vector 0 length))
  #+clozure
  (ccl::%shrink-vector vector length)
  #+cmu
  (lisp::shrink-vector vector length)
  #+digitool
  (ccl::%shrink-vector vector length)
  #+ecl
  (si::shrink-vector vector length)
  #+lispworks
  (system::shrink-vector$vector vector length)
  #+sbcl
  (sb-kernel:shrink-vector vector length)
  #+scl
  (common-lisp::shrink-vector vector length))

(defcm shrink-vector (vector length)
  #+allegro
  `(excl::.primcall 'sys::shrink-svector ,vector ,length)
  #+clisp
  (with-once-only-bindings (vector length)
    `(if (=& ,length (length ,vector))
         ,vector
         (subseq ,vector 0 ,length)))
  #+clozure
  `(ccl::%shrink-vector ,vector ,length)
  #+cmu
  `(lisp::shrink-vector ,vector ,length)
  #+digitool
  `(ccl::%shrink-vector ,vector ,length)
  #+ecl
  `(si::shrink-vector ,vector ,length)
  #+lispworks
  `(system::shrink-vector$vector ,vector ,length)
  #+sbcl
  `(sb-kernel:shrink-vector ,vector ,length)
  #+scl
  `(common-lisp::shrink-vector ,vector ,length))

;;; ===========================================================================
;;;  Trimmed-substring

(defun trimmed-substring (character-bag string 
			  &optional (start 0) (end (length string)))
  (declare (fixnum start end))
  ;; Allow string-designator:
  (unless (stringp string)
    (setf string (string string)))
  ;; Return extracted substring with `char-bag' trimming:
  (while (and (<& start end)
	      (find (char (the simple-string string) start) character-bag))
    (incf& start))
  (decf& end)
  (while (and (<& start end)
	      (find (char (the simple-string string) end) character-bag))
    (decf& end))
  (subseq string start (1+& end)))

;;; ===========================================================================
;;;  Specialized length checkers

(defun list-length-1-p (list)
  (and (consp list) (null (cdr list))))

(defcm list-length-1-p (list)
  (with-once-only-bindings (list)
    `(and (consp ,list) (null (cdr ,list)))))

;;; ---------------------------------------------------------------------------

(defun list-length-2-p (list)
  (and (consp list)
       (let ((rest (cdr list)))
	 (and (consp rest)
	      (null (cdr rest))))))

(defcm list-length-2-p (list)
  (with-once-only-bindings (list)
    `(and (consp ,list)
	  (let ((rest (cdr, list)))
	    (and (consp rest)
		 (null (cdr rest)))))))

;;; ---------------------------------------------------------------------------

(defun list-length> (n list)
  (dotimes (i (1+& n) 't)
    (declare (fixnum i))
    (unless (consp list)
      (return nil))
    (setf list (cdr (the cons list)))))

;;; ---------------------------------------------------------------------------

(defun list-length>1 (list)
  (and (consp list) (consp (cdr list))))

(defcm list-length>1 (list)
  (with-once-only-bindings (list)
    `(and (consp ,list) (consp (cdr ,list)))))

;;; ===========================================================================
;;;  Shuffle-list

(defun shuffle-list (list)
  (when list
    (let ((random-bound 1)
	  (result (list (pop list))))
      (dolist (item list)
	(let ((position (random (incf& random-bound))))
	  (if (zerop& position)
	      (push item result)
	      (let ((tail (nthcdr (1-& position) result)))
		(setf (cdr tail) (cons item (cdr tail)))))))
      result)))

;;; ===========================================================================
;;;  Set-equal

(defun set-equal (list1 list2 &key key
				   (test #'eql test-supplied-p)
				   (test-not nil test-not-supplied-p))
  ;;; Return 't if all elements in `list1' appear in `list2' (and vice
  ;;; versa).  Does not worry about duplicates in either list.
  (when (and test-supplied-p test-not-supplied-p)
    (error "Both ~s and ~s were supplied." ':test ':test-not))
  (let ((key (when key (coerce key 'function)))
        (test (if test-not 
		  (complement (coerce test-not 'function))
		  (coerce test 'function))))
    (declare (type (or function null) key)
             (type function test))
    (dolist (element list1)
      (unless (member (if key (funcall key element) element)
		      list2 :key key :test test)
	(return-from set-equal nil)))
    (dolist (element list2)
      (unless (member (if key (funcall key element) element)
		      list1 :key key :test test)
	(return-from set-equal nil)))
    ;; return success:
    't))

;;; ===========================================================================
;;;  Sets-overlap-p

(defun sets-overlap-p (list1 list2 &key key
					(test #'eql test-supplied-p)
					(test-not nil test-not-supplied-p))
  ;;; Return 't if any element in `list1' appears in `list2'. 
  ;;; Does not worry about duplicates in either list.
  (when (and test-supplied-p test-not-supplied-p)
    (error "Both ~s and ~s were supplied." ':test ':test-not))
  (let ((key (when key (coerce key 'function)))
        (test (if test-not 
		  (complement (coerce test-not 'function))
		  (coerce test 'function))))
    (declare (type (or function null) key)
             (type function test))
    (dolist (element list1)
      (when (member (if key (funcall key element) element)
		    list2 :key key :test test)
	(return-from sets-overlap-p 't)))
    ;; return failure:
    nil))

;;; ===========================================================================
;;;   XOR (imported/exported from some CL implementations)

#-allegro
(defun xor (&rest args)
  (declare (dynamic-extent args))
  (let ((result nil))
    (dolist (arg args result)
      (when arg (setf result (not result))))))

;;; ===========================================================================
;;;   Association-list extensions

(defmacro push-acons (key datum place &environment env)
  ;;; Pushes an acons of key and datum onto the place alist (whether or not a
  ;;; matching key exists in the place alist.  Returns the updated alist.
  (if (symbolp place)
      `(setf ,place (acons ,key ,datum ,place))
      (with-once-only-bindings (key datum)
	(multiple-value-bind (vars vals store-vars writer-form reader-form)
	    (get-setf-expansion place env)
	  `(let* (,@(mapcar #'list vars vals)
		  (,(first store-vars)
                   (acons ,key ,datum ,reader-form)))
             ,writer-form)))))

;;; ---------------------------------------------------------------------------

(defmacro pushnew-acons (key datum place &rest keys &environment env)
  ;;; Performs a push-acons of place, key, and datum only if (assoc key place)
  ;;; returns nil.  Otherwise, datum replaces the old datum of key.  Returns
  ;;; the updated alist."
  (with-once-only-bindings (key datum)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      (with-gensyms (assoc-result)
	`(let* (,@(mapcar #'list vars vals)
		(,(first store-vars) ,reader-form)
		(,assoc-result (assoc ,key ,(first store-vars) ,@keys)))
	   (cond (,assoc-result
		  (rplacd ,assoc-result ,datum)
		  ,(first store-vars))
		 (t (setf ,(first store-vars)
		      (acons ,key ,datum ,(first store-vars)))))
	   ,writer-form)))))

;;; ---------------------------------------------------------------------------

(defun pushnew/incf-acons-expander (incf-fn-sym key incr place keys env)
  (with-once-only-bindings (key incr)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      (with-gensyms (assoc-result)
	`(let* (,@(mapcar #'list vars vals)
		(,(first store-vars) ,reader-form)
		(,assoc-result (assoc ,key ,(first store-vars) ,@keys)))
	   (cond (,assoc-result
		  (rplacd ,assoc-result 
			  (,incf-fn-sym (cdr ,assoc-result) ,incr))
		  ,(first store-vars))
		 (t (setf ,(first store-vars)
		      (acons ,key ,incr ,(first store-vars)))))
	   ,writer-form)))))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf-acons (key incr place &rest keys &environment env)
  ;;; Increments the value of key by incr, if it is present; otherwise
  ;;; performs a push-acons of place, key, and incr.  Returns the updated
  ;;; alist."
  (pushnew/incf-acons-expander '+ key incr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf&-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+& key incr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf$-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+$ key incr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro pushnew/incf$$-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+$$ key incr place keys env))

;;; ---------------------------------------------------------------------------

#+(and scl long-float)
(defmacro pushnew/incf$$$-acons (key incr place &rest keys &environment env)
  (pushnew/incf-acons-expander '+$$$ key incr place keys env))

;;; ---------------------------------------------------------------------------
;;;  decf/delete-acons (inverse of pushnew/incf-acons)

(defun acons-not-found-error (key place)
  (error "Item ~s was not found in the alist in ~w." key place))

;;; ---------------------------------------------------------------------------

(defun decf/delete-acons-expander (decf-fn-sym key decr place keys env)
  (let ((keyword-key-value (getf keys ':key)))
    (with-once-only-bindings (key decr)
      (multiple-value-bind (vars vals store-vars writer-form reader-form)
          (get-setf-expansion place env)
        (with-gensyms (assoc-result new-value)
          `(let* (,@(mapcar #'list vars vals)
                  (,(first store-vars) ,reader-form)
                  (,assoc-result (assoc ,key ,(first store-vars) ,@keys)))
             (cond (,assoc-result
                    (let ((,new-value 
                           (,decf-fn-sym (cdr ,assoc-result) ,decr)))
                      (if (zerop ,new-value)
                          ;; Remove the acons:
                          (setf ,(first store-vars)
                                (delete ,key ,(first store-vars)
                                        :key ,(if keyword-key-value
                                                  `#'(lambda (,new-value)
                                                       (funcall
                                                        ,keyword-key-value
                                                        (car ,new-value)))
                                                  '#'car)
                                        ,@(remove-property keys ':key)))
                          ;; Update the value:
                          (rplacd ,assoc-result ,new-value))))
                   (t (acons-not-found-error ,key ',place)))
             ,writer-form))))))

;;; ---------------------------------------------------------------------------

(defmacro decf/delete-acons (key decr place &rest keys &environment env)
  ;;; Decrements the value of key by decr, if it is present; otherwise
  ;;; performs a push-acons of place, key, and decr.  Returns the updated
  ;;; alist."
  (decf/delete-acons-expander '- key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf/delete&-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-& key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf/delete$-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-$ key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf/delete$$-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-$$ key decr place keys env))

;;; ---------------------------------------------------------------------------

(defmacro decf/delete$$$-acons (key decr place &rest keys &environment env)
  (decf/delete-acons-expander '-$$$ key decr place keys env))

;;; ===========================================================================
;;;   Pushnew-elements
;;;
;;;   Does a pushnew of each element in its list argument onto place

(defmacro pushnew-elements (list place &rest keys &environment env)
  (with-once-only-bindings (list)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      `(let* (,@(mapcar #'list vars vals)
              (,(first store-vars) ,reader-form))
         (dolist (element ,list)
           (pushnew element ,(first store-vars) ,@keys))
         ,writer-form))))

;;; ===========================================================================
;;;   Incf-after and decf-after
;;;
;;;   Like incf & decf, but returns the original value.

(defun incf-after (place &optional (increment 1))
  (declare (ignore place increment))
  (error "Write a better version of INCF-AFTER!"))

(defun decf-after (place &optional (decrement 1))
  (declare (ignore place decrement))
  (error "Write a better version of DECF-AFTER!"))

;;; ===========================================================================
;;;   Bounded-value
;;;
;;;   Returns n bounded by min and max (type-declared versions are defined in
;;;   declared-numerics.lisp, so this definition is rarely used).

(defun bounded-value (min n max)
  (cond ((< n min) min)
        ((> n max) max)
        (t n)))

;;; ===========================================================================
;;;   Counted-delete
;;;
;;;   This is what delete should have been (and was on the LispMs).  Returns
;;;   the number of items that were deleted as a second value.

(defun counted-delete (item seq &rest args 
		       &key (test #'eql) 
			    (test-not nil test-not-supplied-p)
		       &allow-other-keys)
  (declare (dynamic-extent args))
  ;; no need to check for both test and test-not, delete should do it for us
  ;; (but doesn't in most implementations...):
  (let ((items-deleted 0)
	(test (if test-not 
		  (coerce test-not 'function) 
		  (coerce test 'function))))
    (declare (type function test))
    (flet ((new-test (a b)
             (when (funcall test a b)
               (incf& items-deleted))))
      #-gcl
      (declare (dynamic-extent #'new-test))
      (values (apply #'delete item seq 
		     (if test-not-supplied-p ':test-not ':test)
		     #'new-test 
		     args)
              items-deleted))))

;;; ===========================================================================
;;;   Dotted-length
;;;
;;;   Length function primitive for dotted lists

(defun dotted-length (list)
  (declare (list list))
  (do ((list list (cdr list))
       (i 0 (1+& i)))
      ((atom list) i)
    (declare (fixnum i))))

;;; ===========================================================================
;;;   Splitting-butlast
;;;
;;;   Butlast that returns the unused tail of the list as a second value

(defun splitting-butlast (list &optional (n 1))
  (declare (list list) (fixnum n))
  (unless (null list)
    (let ((length (dotted-length list)))
      (unless (<& length n)
        (let ((result nil))
          (dotimes (i (-& length n))
	    (declare (fixnum i))
            (push (pop list) result))
          (values (nreverse result) list))))))             

;;; ===========================================================================
;;;   Remove-property
;;;
;;;   Non-destructive removal of property from a generalized plist

(defun remove-property (plist indicator)
  (do* ((ptr plist (cddr ptr))
        (ind (car ptr) (car ptr))
        (result nil))
      ;; Only when nothing was found:
      ((null ptr) plist)
    (cond ((atom (cdr ptr))
	   (error "~s is a malformed property list." plist))
	  ((eq ind indicator)
	   (return (nreconc result (cddr ptr)))))
    (setf result (list* (second ptr) ind result))))
        
;;; ===========================================================================
;;;   Remove-properties
;;;
;;;   Non-destructive removal of properties from a generalized plist

(defun remove-properties (plist indicators)
  (cond ((null plist) nil)
        ((memq (first plist) indicators)
         (remove-properties (cddr plist) indicators))
        (t (list* (first plist) (second plist)
                  (remove-properties (cddr plist) indicators)))))
        
;;; ===========================================================================
;;;   NSorted-insert
;;;
;;;   Inserts item in list based on predicate and sort-key functions

(defun nsorted-insert (item list &optional (predicate #'<)
                                           (key #'identity))
  (let ((predicate (coerce predicate 'function))
	(key (coerce key 'function)))
    (declare (type function predicate key))
    (cond
     ;; empty list
     ((null list) (list item))
     ;; destructive insert
     (t (let ((item-key (funcall key item)))
	  (cond 
	   ;; handle front insertion specially
	   ((funcall predicate item-key (funcall key (car list)))
	    (cons item list))
	   (t (do ((sublist list (cdr sublist)))
		  ((null (cdr sublist))
		   (setf (cdr sublist) (list item))
		   list)
		(when (funcall predicate 
			       item-key
			       (funcall key (cadr sublist)))
		  (setf (cdr sublist) (cons item (cdr sublist)))
		  (return list))))))))))

;;; ===========================================================================
;;;   Print-pretty-function-object

(defun print-pretty-function-object (fn &optional (stream *standard-output*))
  (let ((name (nth-value 2 (function-lambda-expression fn))))
    #+allegro
    (when (consp name) (setf name (second name)))
    #+lispworks
    (when (consp name) (setf name (third name)))
    (if name
	(print-unreadable-object (fn stream)
	  (format stream "~s ~s" 'function name))
	(prin1 name stream))))

;;; ===========================================================================
;;;   Read-char immediately

(defun read-char-immediately (&optional (stream *standard-input*))
  ;;; Returns a single character keystroke from the user, unbuffered if
  ;;; possible
  
  ;; <implementation-specific versions>

  ;; for CLs without unbuffered read-char capability, throw away all but
  ;; the first character of the line (requires a <Return> by the user):
  (let ((line (handler-case (read-line stream)
                (stream-error () nil))))
    (if (plusp& (length line))
	(elt line 0)
	#\SPACE)))

;;; ===========================================================================
;;;   Add missing extract-specializer-names

#+ecl
(defun extract-specializer-names (arglist)
  ;;; Extracted from si::c-local'ed parse-specialized-lambda-list -- better
  ;;; would be to simply include extract-specializer-names in method.lsp
  (let* (parameters lambda-list specializers)
    (do ((arg (first arglist) (first arglist)))
	((or (null arglist)
	     (memq arg '(&optional &rest &key &allow-other-keys &aux))))
      (pop arglist)
      (push (if (listp arg) (first arg) arg) parameters)
      (push (if (listp arg) (first arg) arg) lambda-list)
      (push (if (listp arg) 
		(if (consp (second arg))
		    `(eql ,(eval (cadadr arg)))
		    (second arg))
		())
	    specializers))
    (when (eq (first arglist) '&optional)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
	  ((or (null arglist)
	       (memq arg '(&optional &rest &key &allow-other-keys &aux))))
	(pop arglist)
	(push (if (listp arg) (first arg) arg) parameters)
	(push arg lambda-list)))
    (when (eq (first arglist) '&rest)
      (push (pop arglist) lambda-list)
      (when (not (symbolp (first arglist)))
	(error "~s in the lambda-list is not a symbol."
	       (first arglist)))
      (push (pop arglist) lambda-list))
    (when (eq (first arglist) '&key)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
	  ((or (null arglist)
	       (memq arg '(&optional &rest &key &aux))))
	(pop arglist)
	(when (eq arg '&allow-other-keys)
	  (push arg lambda-list)
	  (return))
	(push (if (listp arg) (first arg) arg) parameters)
	(push arg lambda-list)))
    (when (eq (first arglist) '&aux)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
	  ((or (null arglist)
	       (memq arg '(&optional &rest &key &allow-other-keys &aux))))
	(pop arglist)
	(push (if (listp arg) (first arg) arg) parameters)
	(push arg lambda-list)))
    (when arglist (error "The position of the lambda-list keyword ~s~%~
                          is not correct."
			 (first arglist)))
    (nreverse specializers)))

;;; ===========================================================================
;;;  Individual method removal (based on Zack Rubinstein's original version)
;;;
;;;  Note: this does not work well with some env-specific eql specializers

(defun find-and-remove-method (generic-function method-qualifiers
                               specialized-lambda-list)
  (flet ((make-specializer (name)
           (if (and (consp name) (eq (first name) 'eql))
               (intern-eql-specializer (eval (second name)))
               (find-class name))))           
    #-gcl
    (declare (dynamic-extent #'make-specializer))
    (let* ((specializer-names
            (extract-specializer-names specialized-lambda-list))
           (method-object
            (find-method generic-function 
                         (ensure-list method-qualifiers)
                         (mapcar #'make-specializer specializer-names)
                         ;; don't signal errors
                         nil)))
      (if method-object
          (remove-method generic-function method-object)
          (warn "Unable to locate method ~s ~s ~s"
                generic-function
                method-qualifiers
                specializer-names)))))

;;; ---------------------------------------------------------------------------

(flet ((method-qualifiers-p (spec)
         (or (null spec)
             (keywordp spec)
             (and (consp spec)
                  (every #'keywordp spec)))))
  (defmacro undefmethod (method-name maybe-qualifiers &rest args)
    (if (method-qualifiers-p maybe-qualifiers)
        `(find-and-remove-method 
          #',method-name ',maybe-qualifiers ',(first args))
        `(find-and-remove-method 
          #',method-name nil ',maybe-qualifiers))))

;;; ===========================================================================
;;;   Macrolet-debug

(defmacro macrolet-debug ((&rest macrobindings) &body body)
  ;;; This handy macro can help with macrolet debugging.  It defines the local
  ;;; macro definitions as global macros (allowing quick macroexpansion of
  ;;; the `body' forms)
  `(progn
     ,@(mapcar
	#'(lambda (macro)
	    `(defmacro ,@macro))
	macrobindings)
     ,@body))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


