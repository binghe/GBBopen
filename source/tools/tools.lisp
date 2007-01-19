;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/tools.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jan 19 17:07:33 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

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
;;; Copyright (C) 2002-2007, Dan Corkill <corkill@GBBopen.org>
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
;;;  07-04-02 File Created.  (Corkill)
;;;  03-10-04 Added pushnew/incf-acons.  (Corkill)
;;;  03-20-04 Added pretty time-interval functions.  (Corkill)
;;;  03-21-04 Added remove-properties.  (Corkill)
;;;  04-30-04 Added set-equal.  (Corkill)
;;;  05-10-04 Added do-until.  (Corkill)
;;;  05-24-04 Added macrolet-debug.  (Corkill)
;;;  05-31-04 Improve counted-delete and set-equal.  (Corkill)
;;;  06-06-04 Added ensure-list-of-lists.  (Corkill)
;;;  07-08-04 Added xor.  (Corkill)
;;;  07-15-04 Added read-char-immediately.  (Corkill)
;;;  05-27-04 Added sets-overlap-p.  (Corkill)
;;;  06-01-05 Added print-pretty-function-object.  (Corkill)
;;;  06-08-05 Added CLISP support.  (sds)
;;;  11-02-05 Added CormanLisp support.  (Corkill)
;;;  11-30-05 Rewrote list-length=1 as list-length-1-p.  (Corkill)
;;;  02-13-06 Added GCL support.  (Corkill)
;;;  03-11-06 Depreciated assure-list.  (Corkill)
;;;  03-12-06 Added list-length-2-p and removed deprecated list-length=1.
;;;           (Corkill)
;;;  03-18-06 Added dosequence.  (Corkill)
;;;  04-07-06 Added shuffle-list.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  08-20-06 Added extract-declarations.  (Corkill)
;;;  09-22-06 Added CormanLisp 3.0 support.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

;;; ---------------------------------------------------------------------------
;;;  Create CLOS package nickname where needed

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext::without-package-locks
   (add-package-nickname "CLOS" :pcl)))
  
#+cormanlisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-nickname "CLOS" :common-lisp))

#+digitool-mcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-nickname "CLOS" :ccl))

#+gcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-nickname "CLOS" :pcl))

#+openmcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-nickname "CLOS" :ccl))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext::without-package-locks
   (add-package-nickname "CLOS" :sb-pcl)))

;;; ---------------------------------------------------------------------------
;;;  Import MOP symbols, as needed

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(#-cormanlisp
	    clos::class-finalized-p 	; missing in CormanLisp 3.0
	    #-(or cormanlisp ecl)
	    clos:extract-specializer-names
	    clos::finalize-inheritance 	; not exported in ECL
	    #-(or cormanlisp ecl lispworks)
	    clos::intern-eql-specializer)))

;;; CormanLisp 3.0 is missing extract-specializer-names:
#+cormanlisp
(defun extract-specializer-names (specialized-lambda-list)
  (lisp::extract-specializers specialized-lambda-list))

;;; CormanLisp 3.0 is missing class-finalized-p, so we always assume a class
;;; is finalized:
#+cormanlisp
(defun class-finalized-p (class)
  (declare (ignore class))
  't)

;;; Lispworks and CormanLisp are missing intern-eql-specializer:
#+(or cormanlisp lispworks)
(defun intern-eql-specializer (x)
  `(eql ,x))

;;; ---------------------------------------------------------------------------
;;;  Import routinely available entities, whenever possible

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 
   #+allegro 
   '(excl::extract-declarations
     excl::memq
     excl:until
     excl:while
     excl::xor)
   #+clisp
   '(system::memq)
   #+cmu
   '(ext:memq
     ext:delq)
   #+cormanlisp
   '()
   #+digitool-mcl
   '(ccl:memq
     ccl:delq)
   ;; Note: ECL's while doesn't include a NIL block, so we can't use it
   #+ecl
   '(si:memq)
   #+gcl
   '()
   #+lispworks
   '(system:memq
     system:delq)
   #+openmcl
   '(ccl:memq
     ccl:delq)
   #+sbcl
   '(sb-int:memq
     sb-int:delq)
   #+scl
   '(ext:memq
     ext:delq)
   #-(or allegro clisp cmu cormanlisp digitool-mcl ecl gcl lispworks openmcl
	 sbcl scl)
   '()))

;;; ---------------------------------------------------------------------------
;;;  Exported tools entities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(bounded-value
	    brief-date-and-time		; in mini-module, but part of tools
	    counted-delete
	    decf-after			; not yet finished or documented
	    define-directory		; in mini-module, but part of tools
	    delq
	    do-until
	    dosequence
	    dotted-conc-name		; in mini-module, but part of tools
	    dotted-length
	    error-message		; lexical fn in with-error-handling
	    ensure-finalized-class
	    ensure-list
	    ensure-list-of-lists	; not yet documented
	    extract-declarations	; not documented
	    incf-after			; not yet finished or documented
	    internet-text-date-and-time ; not yet documented
	    iso8661-date-and-time	; not yet documented
	    list-length-1-p
	    list-length-2-p
	    macrolet-debug		; not documented
	    make-keyword
	    memq
	    message-log-date-and-time	; not yet documented
	    nsorted-insert
	    pretty-time-interval	; not yet documented
	    pretty-run-time-interval	; not yet documented
	    print-pretty-function-object ; not yet documented
	    push-acons
	    pushnew-acons
	    pushnew/incf-acons
	    pushnew/incf&-acons
	    pushnew/incf$-acons
	    pushnew/incf$$-acons
	    #+(and scl long-float)
	    pushnew/incf$$$-acons
	    pushnew-elements
	    read-char-immediately	; not yet documented
	    remove-property
	    remove-properties
	    set-equal
	    sets-overlap-p
	    shuffle-list
	    sole-element
	    splitting-butlast
	    undefmethod
	    until
	    while
	    with-error-handling
	    xor)))

;;; ===========================================================================
;;;  Basic while and until macros

#-allegro
(defmacro while (test &body body)
  `(loop (unless ,test (return))
     ,@body))

#-allegro
(defmacro until (test &body body)
  `(loop (when ,test (return))
     ,@body))

(defmacro do-until (form test)
  `(loop ,form (when ,test (return))))

;;; ===========================================================================
;;;  Memq (lists only)

#-(or allegro clisp cmu digitool-mcl ecl lispworks openmcl sbcl scl)
(progn
  (defun memq (item list)
    (declare (list list))
    (member item list :test #'eq))
  
  #-full-safety
  (define-compiler-macro memq (item list)
    `(member ,item (the ,list ,list) :test #'eq)))

;;; ===========================================================================
;;;  Delq (lists only)

#+allegro
(progn
  (defun delq (item list)
    (excl::list-delete-eq item list))
  
  #-full-safety
  (define-compiler-macro delq (item list)
    `(excl::list-delete-eq ,item ,list)))

#-(or allegro cmu digitool-mcl lispworks openmcl sbcl scl)
(progn
  (defun delq (item list)
    (declare (list list))
    (delete item list :test #'eq))
  
  #-full-safety
  (define-compiler-macro delq (item list)
    `(delete ,item (the list ,list) :test #'eq)))

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
;;;  With-error-handling
;;;
;;;  Evaluates body if an error occurs evaluating form

(defmacro with-error-handling (form &body body)
  `(handler-case ,form
    (error (condition)
           ,@(if body
                 `((flet ((error-message ()
                            (let ((*print-readably* nil))
                              (handler-case (format nil "~a" condition)
                               (error ()
                                 "<error: unprintable condition>")))))
                     (declare (dynamic-extent error-message))
		     #-sbcl
                     (declare (ignorable error-message))
                     ,@body))
                 '((declare (ignore condition)))))))

;;; ===========================================================================
;;;  Ensure-finalized-class

(defun ensure-finalized-class (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  class)

#-full-safety
(define-compiler-macro ensure-finalized-class (class)
  (with-once-only-bindings (class)
    `(progn
       (unless (class-finalized-p ,class)
	 (finalize-inheritance ,class))
       ,class)))

;;; ===========================================================================
;;;  Make-keyword

(defun make-keyword (symbol)
  (intern (symbol-name symbol)  
	  (load-time-value (find-package 'keyword))))

#-full-safety
(define-compiler-macro make-keyword (symbol)
  `(intern (symbol-name ,symbol) 
	   (load-time-value (find-package 'keyword))))

;;; =========================================================================== 
;;;  Ensure-list 

(defun ensure-list (x)
  (if (listp x) x (list x)))

#-full-safety
(define-compiler-macro ensure-list (x)
  (with-once-only-bindings (x)
    `(if (listp ,x) ,x (list ,x))))

;;; ===========================================================================
;;;  Ensure-list-of-lists

(defun ensure-list-of-lists (x)
  (let ((x (ensure-list x)))
    (if (listp (car x)) x (list x))))

#-full-safety
(define-compiler-macro ensure-list-of-lists (x)
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

#-full-safety
(define-compiler-macro sole-element (list)
  (with-once-only-bindings (list)
    `(prog1 (first ,list)
       (when (rest ,list)
	 (sole-element-violation ,list)))))
  
(defun sole-element-violation (list)
  (cerror "Ignore the remaining elements."
          "The list ~s contains more than 1 element."
          list))

;;; ===========================================================================
;;;  Specialized length checkers

(defun list-length-1-p (list)
  (and (consp list) (null (cdr list))))

#-full-safety
(define-compiler-macro list-length-1-p (list)
  (with-once-only-bindings (list)
    `(and (consp ,list) (null (cdr ,list)))))

(defun list-length-2-p (list)
  (and (consp list)
       (let ((rest (cdr list)))
	 (and (consp rest)
	      (null (cdr rest))))))

#-full-safety
(define-compiler-macro list-length-2-p (list)
  (with-once-only-bindings (list)
    `(and (consp ,list)
	  (let ((rest (cdr, list)))
	    (and (consp rest)
		 (null (cdr rest)))))))

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
      (when arg (setq result (not result))))))

;;; ===========================================================================
;;;   Association-list extensions

(defmacro push-acons (key datum place &environment env)
  ;;; Pushes an acons of key and datum onto the place alist (whether or not a
  ;;; matching key exists in the place alist.  Returns the updated alist.
  (if (symbolp place)
      `(setq ,place (acons ,key ,datum ,place))
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
		 (t (setq ,(first store-vars)
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
		 (t (setq ,(first store-vars)
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
    (setq result (list* (second ptr) ind result))))
        
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
;;;  Time formatting

(defparameter *month-name-vector*
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defparameter *weekday-name-vector*
    #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

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
  ;;; Return a string of typical time-zone abbreviations;
  ;;; zone is an integer with decode-universal-time semantics.
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
;;;   Print-pretty-function-object

(defun print-pretty-function-object (fn &optional (stream *standard-output*))
  (let ((name (nth-value 2 (function-lambda-expression fn))))
    #+allegro
    (when (consp name) (setq name (second name)))
    #+lispworks
    (when (consp name) (setq name (third name)))
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
  (flet ((make-qualifier (name)
           (if (and (consp name)
                    (eq (first name) 'eql))
               (intern-eql-specializer (eval (second name)))
               (find-class name))))           
    #-gcl
    (declare (dynamic-extent #'make-qualifier))
    (let* ((specializer-names
            (extract-specializer-names specialized-lambda-list))
           (method-object
            (find-method generic-function 
                         (ensure-list method-qualifiers)
                         (mapcar #'make-qualifier specializer-names)
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


