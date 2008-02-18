;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/gbbopen/units.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Feb 15 01:41:16 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       GBBopen Unit-Class Functions
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
;;;   [Description of MOP implementation-specific porting details needed...]
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  07-18-02 File Created.  (Corkill)
;;;  09-15-02 Moved extended class functions into ../tools/define-class.lisp.
;;;           (Corkill)
;;;  01-21-04 Added class-instances-count.  (Corkill)
;;;  03-11-04 Added inheritance of initial-space-instances and
;;;           dimensional-values (no inheritance prevention yet).  (Corkill)
;;;  03-16-04 Changed dimension-value-type :label to :element.  (Corkill)
;;;  04-20-04 MOP imports separated into mop-interface.lisp.  (Corkill)
;;;  05-27-04 Added unit-class-dimensions.  (Corkill)
;;;  06-08-04 Added parse-unit-classes-specifier.  (Corkill)
;;;  08-05-04 Removed hacks supporting CMUCL-18e non-compliances.  (Corkill)
;;;  08-16-04 Added check for accidentally quoted :singular values 
;;;           in link-slot options.  (Corkill)
;;;  09-01-04 Added check on finalization that a unit-class inherits from
;;;           standard-unit-instance.  (Corkill)
;;;  09-27-04 Removed all-unit-class-names.  (Corkill)
;;;  12-21-04 Remove instance-name-comparison-test GF mechanism.  (Corkill)
;;;  06-01-05 Support two-slot :interval dimensional-values.  (Corkill)
;;;  06-08-05 Added CLISP support. (sds)
;;;  06-13-05 Moved GBBopen unit metaclass definitions to 
;;;           unit-metaclasses.lisp. (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(class-instances-count
            class-instances-summary     ; not yet documented
            define-unit-class
            describe-unit-class
            effective-link-definition
            effective-nonlink-slot-definition 
	    ensure-unit-class		; not yet implemented/documented
            find-effective-slot-definition-by-name ; not documented (yet...)
            find-unit-class             ; not documented (at least yet...)
            gbbopen-effective-slot-definition
            reset-unit-class)))

;;; ---------------------------------------------------------------------------

(defparameter *dimension-value-types*
    '(:point :interval :mixed :element :boolean))

;;; ===========================================================================
;;;   Define-unit-class

(defmethod validate-class-option ((metaclass standard-class) option)
  #+ecl (declare (ignore metaclass))
  (or (memq (car option) *standard-define-class-options*)
      (memq (car option) '(:default-initargs 
			   :documentation
			   :metaclass
			   #+(or ecl lispworks)
			   :optimize-slot-access))))

 ;;; ---------------------------------------------------------------------------

(defun compute-inherited-unit-class-values (unit-class)
  ;;; Determines directly inherited unit-class values from super-unit-classes
  ;;; of `unit-class' and then propagates them down to all sub-unit-classes
  
  ;; Begin with direct values:
  (let ((initial-space-instances
         (standard-unit-class.initial-space-instances unit-class))
        (dimensional-values
         (standard-unit-class.dimensional-values unit-class))
        (retain
         (standard-unit-class.retain unit-class)))
    (declare (type list dimensional-values))
    (unless initial-space-instances
      ;; Find closest initial-space-instances from supers:
      (dolist (super (class-direct-superclasses unit-class))
        (when (typep super 'standard-unit-class)
          (let ((inherited-value 
                 (standard-unit-class.effective-initial-space-instances
                  super)))
            (when inherited-value
              (setq initial-space-instances inherited-value)
              (return))))))
    ;; Add dimensional-values from supers:
    (dolist (super (class-direct-superclasses unit-class))
      (when (typep super 'standard-unit-class)
        (dolist (dimensional-value 
                    (standard-unit-class.effective-dimensional-values super))
          (pushnew dimensional-value dimensional-values
                   :test #'eq
                   :key #'car))))
    ;; Stash the values:
    (setf (standard-unit-class.effective-initial-space-instances unit-class)
          initial-space-instances)
    (setf (standard-unit-class.effective-dimensional-values unit-class)
          dimensional-values)
    ;; Cache unit-class-dimensions:
    (setf (standard-unit-class.unit-class-dimensions unit-class)
          (mapcar #'(lambda (dimensional-value)
                      `(,(first dimensional-value)
                        ,(ecase (second dimensional-value)
                           (:boolean ':boolean)
                           (:element ':enumerated)
                           ((:point :interval :mixed) ':ordered))))
		  dimensional-values))
    ;; Propogate retain attribute from supers:
    (unless retain
      (dolist (super (class-direct-superclasses unit-class))
        (when (and (typep super 'standard-unit-class)
                   (eq (standard-unit-class.retain super) ':propagate))
          (setf (standard-unit-class.retain unit-class) ':propagate)
          (return))))
    ;; Propagate to subs (naive, for now...):
    (dolist (sub (class-direct-subclasses unit-class))
      (compute-inherited-unit-class-values sub))))

;;; ---------------------------------------------------------------------------

(defvar *%%fixup-function-objects%%*)

(defun fixup-function-value-part1 (value)
  ;;; Part 1 of the function-fixup scheme.  Adds functions and possible
  ;;; closures to *%%fixup-function-objects%%* and replaces `value' with a
  ;;; gensym key into *%%fixup-function-objects%%*
  (cond 
   ;; We'll allow symbols as if they were quoted symbols
   ((symbolp value) value)
   ;; Quoted symbols imply (funcall 'symbol ...)
   ((and (consp value) 
         (eq (first value) 'quote)
         (symbolp (second value)))
    (second value))
   ;; Functions & possible closures
   ((and (consp value) 
         (eq (first value) 'function))
    (let ((fixup-symbol (gensym "F")))
      (push `(,fixup-symbol ,value)
            *%%fixup-function-objects%%*)
      fixup-symbol))
   ;; Pass thru anything else
   (t value)))

;;; ---------------------------------------------------------------------------
;;;
;;; <dimensional-value> :== <incomposite-dimensional-value> | 
;;;                         <composite-dimensional-value>
;;; <incomposite-dimensional-value>
;;;             :== (<dim-name> <dimension-value-type> <dimension-value-place>)
;;; <composite-dimensional-value>
;;;             :== (<dim-name> <dimension-value-type> 
;;;                  <composite-type> <dimension-value-place>)
;;; <composite-type> :== :set | :series |
;;;                      :ascending-series <ordering-dimension-name> |
;;;                      :descending-series <ordering-dimension-name>
;;; <dimension-value-type> :==  :point | :interval | :mixed | 
;;;                             :element | :boolean
;;; <dimension-value-place> :== {<slot-name> [<slot-name>]} |
;;;                             {<function> [<slot-name>]}
;;;  note that 2 slot-names are only allowed for :interval dimension values

(defun parse-dimensional-value-spec (dv-spec)
  (case (third dv-spec)
    ;; non-series composite dimensional value:
    ((:set :sequence)
     (destructuring-bind (dimension-name dimension-value-type
                          composite-type slot-name-or-fn 
                          &optional slot-name)
         dv-spec
       (values dimension-name dimension-value-type composite-type 
               nil slot-name-or-fn slot-name)))
    ((:ascending-series :descending-series)
     (destructuring-bind (dimension-name dimension-value-type
                          composite-type ordering-dimension-name
                          slot-name-or-fn &optional slot-name)
         dv-spec
       (values dimension-name dimension-value-type composite-type
               ordering-dimension-name slot-name-or-fn slot-name)))    
    (otherwise 
     (destructuring-bind (dimension-name #+IGNORE-FOR-NOW composite-value-type
                          dimension-value-type slot-name-or-fn 
                          &optional slot-name)
         dv-spec
       (values dimension-name dimension-value-type nil
               nil slot-name-or-fn slot-name)))))   
 
;;; ---------------------------------------------------------------------------

(defun declared-function-designator (fnd)
  ;; Declares `fnd' is a function, unless it is a quoted symbol:
  (if (eq 'quote (first fnd))
      fnd
      `(the function ,fnd)))

;;; ---------------------------------------------------------------------------

(defun build-cannonical-dimensional-value-spec (unit-class-name dv-spec)
  ;;; Returns a cannonical dimension-value specification of `dv-spec' of the
  ;;; form:
  ;;;
  ;;;   (dimension-name dimension-value-type lookup-fn
  ;;;    composite-type ordering-dimension-name)
  ;;;
  ;;; Note that the cannonical value does not contain sufficient accessible
  ;;; information to recreate the original `dv-spec'.
  (multiple-value-bind (dimension-name dimension-value-type
                        composite-type ordering-dimension-name
                        slot-name-or-fn slot-name)
      (parse-dimensional-value-spec dv-spec)
    (unless (memq dimension-value-type (the list *dimension-value-types*))
      (error "Dimension-value-type ~s specified for ~
              dimension ~s of ~s is not one of ~
              ~{~#[~;~s~;~s and ~s~:;~@{~s~#[~;, or ~:;, ~]~}~]~}."
             dimension-value-type
             dimension-name
             unit-class-name
	     *dimension-value-types*))
    (when slot-name
      (unless (symbolp slot-name)
        (error "Invalid slot name ~s specified for ~
                       dimension ~s of ~s." 
               slot-name
               dimension-name
               unit-class-name)))
    ;; build the dimensional-value lookup function, called on a unit instance
    ;; to return the dimensional value for the `dv-spec' dimension:
    (let* ((unfixed-dv-lookup-function
            `(function
              (lambda (instance &optional into-cons)
                (declare (type ,unit-class-name instance)
                         (ignorable into-cons))
                ,(cond 
                  ;; an :interval dimension-value-type with two slot names:
                  ((and (eq dimension-value-type ':interval)
                        (symbolp slot-name-or-fn)
                        slot-name)
                   `(if (and (slot-boundp instance ',slot-name-or-fn)
                             (slot-boundp instance ',slot-name))
                        (cond 
                         (into-cons
                          (setf (car into-cons)
                                (slot-value instance ',slot-name-or-fn))
			  (setf (cdr into-cons)
				(slot-value instance ',slot-name))
			  into-cons)
			 (t (cons
			     (slot-value instance ',slot-name-or-fn)
			     (slot-value instance ',slot-name))))
			unbound-value-indicator))
                  ;; only a slot name:
                  ((symbolp slot-name-or-fn)
                   `(if (slot-boundp instance ',slot-name-or-fn)
			(slot-value instance ',slot-name-or-fn)
			unbound-value-indicator))
                  ;; a function with a slot name:
                  (slot-name
                   `(if (slot-boundp instance ',slot-name)
			(funcall ,(declared-function-designator slot-name-or-fn)
				 (slot-value instance ',slot-name))
			unbound-value-indicator))
                  ;; just a function
                  (t `(funcall ,(declared-function-designator slot-name-or-fn)
			       instance))))))
           (dv-lookup-function 
            (fixup-function-value-part1 unfixed-dv-lookup-function)))
      `(,dimension-name ,dimension-value-type ,dv-lookup-function
                        ,composite-type ,ordering-dimension-name))))

;;; ---------------------------------------------------------------------------

(defun fixup-function-objects-part2 (unit-class fixup-symbols)
  ;;; Part 2 of the function-fixup scheme.  Restores function initvals for
  ;;; :sort-function and :sort-key from *%%fixup-function-objects%%* based on
  ;;; the gensym'ed fixup-symbols
  (declare (type list fixup-symbols))
  (map-direct-link-slots 
   #'(lambda (dslotd)
       ;; fixup sort-function slot
       (let ((fn (direct-link-definition.sort-function dslotd)))
         (when (and fn (memq fn fixup-symbols))
           (setf (direct-link-definition.sort-function dslotd)
                 (symbol-value fn))))
       ;; fixup sort-key slot
       (let ((fn (direct-link-definition.sort-key dslotd)))
         (when (and fn (memq fn fixup-symbols))
           (setf (direct-link-definition.sort-key dslotd)
                 (symbol-value fn)))))
   unit-class)
  ;; fixup canonicalized :dimensional-values option:  
  (dolist (dv-spec (standard-unit-class.dimensional-values unit-class))
    (let ((maybe-fn (third dv-spec)))
      ;; NOTE: CLISP sometimes looses eq-ness of the uninterned fixup-symbols
      ;; (last observed in CLISP 2.44).  As a work-around until this is
      ;; tracked down, we look up the symbol in fixup-symbols by comparing the
      ;; symbol-name and then using the symbol from fixup-symbols:
      (when #-clisp (memq maybe-fn fixup-symbols)
            #+clisp (setf maybe-fn 
                          (find maybe-fn fixup-symbols :test #'string=))
        (setf (third dv-spec)
              (symbol-value maybe-fn)))))
  ;; fixup :initial-space-instances option
  (let* ((initial-space-instances-spec
          (standard-unit-class.initial-space-instances unit-class))
         (maybe-fn (car initial-space-instances-spec)))
    (when (memq maybe-fn fixup-symbols)
      (setf (standard-unit-class.initial-space-instances unit-class)
            (symbol-value 
             ;; check that the initial-space-instances specification contains
             ;; only a single function object!
             (sole-element initial-space-instances-spec))))))
  
;;; ---------------------------------------------------------------------------

(defun fixup-function-initargs (initargs)
  ;;; Part 1b of the function-fixup scheme.  Adds function initvals for
  ;;; :sort-function and :sort-key to *%%fixup-function-objects%%* and replaces
  ;;; them with gensyms
  (flet ((fixup-initarg (slot-def indicator)
           (let ((initval (getf (cdr slot-def) indicator)))
             (when initval 
               (setf (getf (cdr slot-def) indicator)
                     (fixup-function-value-part1 initval))))))
    (dolist (slot-def initargs)
      (fixup-initarg slot-def ':sort-function)
      (fixup-initarg slot-def ':sort-key))
    initargs))

;;; ---------------------------------------------------------------------------

(defun fixup-function-option-values (unit-class-name metaclass options)
  ;;; Part 1a of the function-fixup scheme.  
  ;;;
  ;;; Adds functional option values for :initial-space-instances to
  ;;; *%%fixup-function-objects%%* and replaces them with gensyms.
  ;;;
  ;;; Also adds functional option-values for :dimensional-values, as part of
  ;;; syntax checking and cannonicalization.
  (let ((dimensional-values-seen nil)
	(initial-space-instances-seen nil))
    (dolist (option options)
      (case (car option)
	(:dimensional-values 
	 (setq dimensional-values-seen 't)
	 (setf (cdr option)
	       ;; allow (:dimensional-values nil) to mean none:
	       (if (equal (cdr option) '(nil))
		   nil
		   (mapcar #'(lambda (dv-spec)
			       (build-cannonical-dimensional-value-spec
				unit-class-name dv-spec))
			   (cdr option)))))
	(:initial-space-instances
	 (setq initial-space-instances-seen 't)
	 (setf (cdr option)
	       ;; allow (:initial-space-instances nil) to mean none:
	       (if (equal (cdr option) '(nil))
		   nil
		   (mapcar #'fixup-function-value-part1 
			   (cdr option)))))
	;; Check that class option is valid (not mis-named or misspelled);
	;; this is a heavy-handed approach that doesn't pass through unknown
	;; options without using the validate-class-option API.
	(otherwise 
	 (unless (validate-class-option metaclass option)
	   (error "~s is an invalid option in the definition of ~s"
		  option unit-class-name)))))
    ;; We want to clear these in reinitialized class objects, if they
    ;; weren't explicitly specified:
    (unless dimensional-values-seen 
      (push '(:dimensional-values) options))
    (unless initial-space-instances-seen
      (push '(:initial-space-instances) options)))
  options)

;;; ---------------------------------------------------------------------------

(defun finish-unit-class-loading (unit-class fixup-symbols)
  (fixup-function-objects-part2 unit-class fixup-symbols)
  (compute-inherited-unit-class-values unit-class))

;;; ---------------------------------------------------------------------------

#+under-construction
(defun ensure-unit-class (event-class-name direct-superclass-names
			  &key direct-default-initargs direct-slots
			       direct-superclasses name metaclass 
			  &allow-other-keys)
  (nyi))

;;; ---------------------------------------------------------------------------

(defmacro define-unit-class (unit-class-name direct-superclass-names
                             direct-slots &rest options &environment env)
  ;; () value for direct-superclass-names is (standard-unit-instance):
  (when (and (null direct-superclass-names)
             (not (eq unit-class-name 'standard-unit-instance)))
    (setq direct-superclass-names '(standard-unit-instance)))
  (unless (every #'(lambda (element)
		     (and (symbolp element) (not (keywordp element))))
		 direct-superclass-names)
    (error "Illegal direct-superclasses syntax in define-unit-class: ~s"
	   direct-superclass-names))
  ;; To support function objects and closures in :initial-space-instances and
  ;; in :sort-function and :sort-key for links we create a let binding with
  ;; gensym'ed symbols and the function objects, replacing the function
  ;; objects in the defclass form with the gensyms.  Then, at load time, we
  ;; replace the gensyms in the generated class object with the compiled
  ;; function objects from the let binding.
  (let ((*%%fixup-function-objects%%* nil))
    (multiple-value-bind (clos-direct-slots clos-class-options exports)
        (parse-define-class 
         unit-class-name direct-slots 
         (if (member :metaclass (the list options)
                     :test #'eq
                     :key #'first)
             options
             (cons '(:metaclass standard-unit-class) options))
         *standard-define-class-options*
         env)
      ;; We must tell to use accessor methods that call slot-value-using-class
      ;; (ECL 0.9i *must* optimize slot access, as non-optimized accessors
      ;; always return nil):
      #+(or ecl lispworks)
      (setq clos-class-options 
        (cons '(:optimize-slot-access nil) clos-class-options))
      `(#-clisp progn
	;; CLISP requires let (rather than progn) to work around
	;; CLISP's non-eq uninterned symbols bug
	#+clisp let #+clisp ()
	#+sbcl
	(eval-when (:compile-toplevel :load-toplevel :execute)
	  (sb-pcl::preinform-compiler-about-class-type ',unit-class-name))
	,@(when exports
	    `((eval-when (:compile-toplevel :load-toplevel :execute)
		(export ',exports))))
	(defclass ,unit-class-name ,direct-superclass-names
	  ,(fixup-function-initargs clos-direct-slots)
	  ,@(fixup-function-option-values 
	     unit-class-name
	     (class-prototype
	      (ensure-finalized-class
	       (find-class 
		(second (assoc :metaclass clos-class-options :test #'eq)))))
	     clos-class-options))
	;; Some CLs have problems using the class object returned by
	;; defclass (e.g., CMUCL and SBCL).  So, we'll always use
	;; find-class to get it back...
	,(with-gensyms (unit-class)
	   (let ((fixup-symbols (mapcar #'first *%%fixup-function-objects%%*)))
	     `(let ((,unit-class (find-class ',unit-class-name))
		    ,.*%%fixup-function-objects%%*)
		(declare (special ,@fixup-symbols))
		(finish-unit-class-loading ,unit-class ',fixup-symbols)
		,unit-class)))))))
  
;;; ===========================================================================
;;;   Unit-class mappers, utilities, etc.

(defun find-unit-class (unit-class-name)
  ;;; A unit-class-only version of find-class.  Signals an error if
  ;;; `unit-class-name' does not specify a unit class:
  (if (typep unit-class-name 'standard-unit-class)
      unit-class-name
      (let ((unit-class (find-class unit-class-name nil)))
        (unless (typep unit-class 'standard-unit-class)
          (error "~s does not name a unit class." unit-class-name))
        unit-class)))

;;; ---------------------------------------------------------------------------

(defun parse-unit-class-specifier (unit-class-spec)
  (with-full-optimization ()
    (cond
     ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
     ((eq unit-class-spec 't) 
      (values (find-class 'standard-unit-instance) 't))
     ;; extended unit-class specification:
     ((consp unit-class-spec)
      (destructuring-bind (unit-class-name subclass-indicator)
          unit-class-spec
        (let ((unit-class (find-unit-class unit-class-name)))
          (values unit-class 
                  (ecase subclass-indicator
                    (:plus-subclasses 't)
                    (:no-subclasses nil))))))
     ;; anything else we assume is a unit-class-name or unit-class:
     (t (values (find-unit-class unit-class-spec) nil)))))

;;; ---------------------------------------------------------------------------

(defun parse-unit-classes-specifier (unit-classes-spec)
  (cond
   ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
   ((eq unit-classes-spec 't) 
    `((,(find-class 'standard-unit-instance) . t)))
   ((consp unit-classes-spec)
    (flet ((do-one-spec (unit-class-spec)
             (if (consp unit-class-spec)
                 (destructuring-bind (unit-class-name subclass-indicator)
                     unit-class-spec
                   (let ((unit-class (find-unit-class unit-class-name)))
                     `(,unit-class . ,(ecase subclass-indicator
                                        (:plus-subclasses 't)
                                        (:no-subclasses nil)))))
                 ;; anything else we assume is a unit-class-name or
                 ;; unit-class:
                 `(,(find-unit-class unit-class-spec) . nil))))
      (let ((possible-subclass-indicator (second unit-classes-spec)))
        (cond 
         ;; simply an extended unit-class specification?
         ((or (eq possible-subclass-indicator :plus-subclasses)
              (eq possible-subclass-indicator :no-subclasses))
          (list (do-one-spec unit-classes-spec)))
         ;; a list of specifications:
         (t (mapcar #'do-one-spec unit-classes-spec))))))
   ;; anything else we assume is a unit-class-name or unit-class:
   (t `((,(find-unit-class unit-classes-spec) . nil)))))

;;; ---------------------------------------------------------------------------

(defun ensure-unit-classes-specifiers (unit-classes-specifiers)
  (if (and (consp unit-classes-specifiers)
           (list-length-2-p unit-classes-specifiers)
           (let ((maybe-subclass-indicator (second unit-classes-specifiers)))
             (or (eq maybe-subclass-indicator :plus-subclasses)
                 (eq maybe-subclass-indicator :no-subclasses))))
      (list unit-classes-specifiers)
      unit-classes-specifiers))

;;; ---------------------------------------------------------------------------
;;;  Unit-class-in-specifier-p

(defun unit-class-in-specifier-p (unit-class specifier)
  ;; Returns true if `unit-class' is specified in the extended unit-classes
  ;; specifier list `specifier'. 
  (flet ((in-simple-specifier-p  (specifier)
	   (let ((plus-subclasses? nil))
	     ;; Convert extended-unit-class-specifier to simple and flag:
	     (when (consp specifier)
	       (destructuring-bind (unit-class-name subclass-indicator)
		   specifier
		 (setq specifier unit-class-name)
		 (ecase subclass-indicator
		   (:plus-subclasses (setq plus-subclasses? 't))
		   (:no-subclasses))))
	     (if (typep specifier 'standard-unit-class)
		 ;; we have a unit-class object
		 (if plus-subclasses?
		     (subtypep (class-name unit-class) (class-name specifier))
		     (eq (class-name unit-class) (class-name specifier)))
		 ;; anything else we assume is a unit-class-name:
		 (if plus-subclasses?
		     (subtypep (class-name unit-class) specifier)
		     (eq (class-name unit-class) specifier))))))
    (cond
     ;; 't is shorthand for '(standard-unit-instance :plus-subclasses):
     ((eq specifier 't) (typep unit-class 'standard-unit-class))
     ;; could be a single, extended unit-class specification or a list of
     ;; specifications:
     ((consp specifier)
      (if (and (list-length-2-p specifier)
	       (let ((possible-subclass-indicator (second specifier)))
		 (or (eq possible-subclass-indicator :plus-subclasses)
		     (eq possible-subclass-indicator :no-subclasses))))
	  ;; simply a single, extended unit-class specification:
	  (in-simple-specifier-p specifier)
	  ;; a list of specifications:
	  (some #'in-simple-specifier-p specifier)))
     ;; a simple specifier:
     (t (in-simple-specifier-p specifier)))))

;;; ---------------------------------------------------------------------------

(defun map-unit-classes (fn class)
  ;;; Applies `fn' to `class' and all its subclasses.  The order is undefined,
  ;;; but `fn' will not be applied more than once to a class.
  (with-full-optimization ()
    (let ((classes-seen nil)
          (fn (if (functionp fn) fn (fdefinition fn))))
      (declare (type list classes-seen))
      (labels ((doit (class)
                 (when (and (typep class 'standard-unit-class)
                            (not (memq class classes-seen)))
                   (push class classes-seen)
                   (funcall fn class 't)
                   (dolist (class (class-direct-subclasses class))
                     (doit class)))))
        (doit class)))))

;;; ---------------------------------------------------------------------------

(defun map-extended-unit-classes (fn unit-class-name)
  ;;; Handles mapping for functions that accept an extended unit-class-name.
  ;;; The order is undefined, but `fn' will not be applied more than once to a
  ;;; class.
  (with-full-optimization ()
    (let ((fn (if (functionp fn) fn (fdefinition fn))))
      (multiple-value-bind (unit-class plus-subclasses)
          (parse-unit-class-specifier unit-class-name)
        (if plus-subclasses
            (map-unit-classes fn unit-class)
            (funcall fn unit-class nil))))))

;;; ---------------------------------------------------------------------------

(defun map-extended-unit-classes-sorted (fn unit-class-name)
  ;;; Handles mapping for functions that accept an extended unit-class-name.
  ;;; The `fn' will applied once to each class in lexical order.
  (with-full-optimization ()
    (let* ((classes nil)
           (fn (if (functionp fn) fn (fdefinition fn)))
           (accumulation-fn #'(lambda (&rest class-spec)
                                (push class-spec classes))))
      (multiple-value-bind (unit-class plus-subclasses)
          (parse-unit-class-specifier unit-class-name)
        (if plus-subclasses
            (map-unit-classes accumulation-fn unit-class)
            (funcall accumulation-fn unit-class nil)))
      (setf classes (sort classes #'string< 
                          :key #'(lambda (class-spec)
                                   (class-name (first class-spec)))))
      (dolist (class-spec classes)
        (apply fn class-spec)))))

;;; ---------------------------------------------------------------------------

(defun map-unit-classes-specifier (fn unit-classes-specifier)
  ;;; Handles mapping for functions that accept a unit-classes-specifier
  (with-full-optimization ()
    (dolist (specifier (parse-unit-classes-specifier unit-classes-specifier))
      (destructuring-bind (unit-class . plus-subclasses)
          specifier
        (if plus-subclasses
            (map-unit-classes fn unit-class)
            (funcall (if (functionp fn) fn (fdefinition fn))
                     unit-class nil))))))

;;; ---------------------------------------------------------------------------

(defun find-effective-slot-definition-by-name (class slot-name)
  (find slot-name (the list (class-slots class))
        :key #'slot-definition-name
        :test #'eq))

#-full-safety
(define-compiler-macro find-effective-slot-definition-by-name (class slot-name)
  (let ((class-var '#:class))
    `(let ((,class-var (the list (class-slots ,class))))
       (find ,slot-name ,class-var :key #'slot-definition-name :test #'eq))))

;;; ---------------------------------------------------------------------------

(defun map-unit-class-slots (fn unit-class slot-names)
  ;;; Applies `fn' to the slots of `unit-class' specified by `slot-names'.
  ;;; `Slot-names' can be t (all slots), a single slot-name, or a list of
  ;;; slot-names. 
  (let ((class-slots (class-slots unit-class))) 
    (declare (type list class-slots))
    (if (eq slot-names 't)
        ;; all slots:
        (mapc (the function fn) class-slots)
        ;; one or more specific slot names:
        (let ((slot-names (ensure-list slot-names)))
          (dolist (slot-name slot-names)
            (let ((slot (find slot-name class-slots 
                              :key #'slot-definition-name 
                              :test #'eq)))
              (if slot 
                  (funcall (the function fn) slot)
                  (error "Slot ~s does not exist in ~s"
                         slot-name 
                         unit-class))))))))

;;; ---------------------------------------------------------------------------

(defmethod class-instances-count ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (class-instances-count '(standard-unit-instance :plus-subclasses))
      (class-instances-count (find-unit-class unit-class-name))))

(defmethod class-instances-count ((unit-class-spec cons))
  (let ((total 0))
    (map-extended-unit-classes
     #'(lambda (unit-class plus-subclasses)
         (declare (ignore plus-subclasses))
         (incf total (class-instances-count unit-class)))
     unit-class-spec)
    ;; return the computed total:
    total))
    
(defmethod class-instances-count ((unit-class standard-unit-class))
  ;;; Returns the number of instances (in the instance-hash-table) of
  ;;; `unit-class-name-or-class'
  (hash-table-count (standard-unit-class.instance-hash-table unit-class)))
    
;;; ---------------------------------------------------------------------------

(defmethod class-instances-summary ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (class-instances-summary '(standard-unit-instance :plus-subclasses))
      (class-instances-summary (find-unit-class unit-class-name))))

(defmethod class-instances-summary ((unit-class-spec cons))
  (map-extended-unit-classes-sorted
   #'(lambda (unit-class plus-subclasses)
       (declare (ignore plus-subclasses))
       (class-instances-summary unit-class))
   unit-class-spec))
    
(defmethod class-instances-summary ((unit-class standard-unit-class))
  ;;; Prints a descriptive summary of `unit-class-name-or-class'
  (let ((class-name (class-name unit-class)))
    (if (standard-unit-class.abstract unit-class)
        (format t "~&~s:~%~2tAbstract~%" class-name)
        (format t "~&~s:~%~2tInstances: ~d~%~2tCounter: ~d~%"
                class-name
                (class-instances-count unit-class)
                (standard-unit-class.instance-name-counter unit-class)))))

;;; ---------------------------------------------------------------------------

(defmethod reset-unit-class ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (reset-unit-class '(standard-unit-instance :plus-subclasses))
      (reset-unit-class (find-unit-class unit-class-name))))
    
(defmethod reset-unit-class ((unit-class-spec cons))
  (map-extended-unit-classes
   #'(lambda (unit-class plus-subclasses)
       (declare (ignore plus-subclasses))
       (reset-unit-class unit-class))
   unit-class-spec))
    
(defmethod reset-unit-class ((unit-class standard-unit-class))
  ;;; Resets the instance count of a unit-class-name-or-class and creates a new
  ;;; instance hash table, but only if the class is non-abstract
  ;;; and has no existing instances
  (unless (standard-unit-class.abstract unit-class)
    (let ((count (class-instances-count unit-class)))
      (cond
       ((zerop count)
        (setf (standard-unit-class.instance-name-counter unit-class) 0)
        (setf (standard-unit-class.instance-hash-table unit-class)
              (make-hash-table
               :test (standard-unit-class.instance-name-comparison-test 
                      unit-class))))
       (t (warn "Unit class ~s has ~s instance~:p; not reset"
                (class-name unit-class)
                count))))))

;;; ===========================================================================
;;;   Unit-class describing

(defmethod describe-unit-class ((unit-class-name symbol))
  (if (eq unit-class-name 't)
      (describe-unit-class '(standard-unit-instance :plus-subclasses))
      (describe-unit-class (find-unit-class unit-class-name))))

(defmethod describe-unit-class ((unit-class-spec cons))
  (map-extended-unit-classes
   #'(lambda (unit-class plus-subclasses)
       (declare (ignore plus-subclasses))
       (describe-unit-class unit-class))
   unit-class-spec))
    
(defmethod describe-unit-class ((unit-class standard-unit-class))
  (ensure-finalized-class unit-class)
  (format t "~&~@(~s~) ~s~:[~; (abstract)~]" 
          (class-name (class-of unit-class))
          unit-class
          (standard-unit-class.abstract unit-class))
  (let* ((supers (class-direct-superclasses unit-class))
         (subs (class-direct-subclasses unit-class))
         (hidden-slot-names
          (hidden-nonlink-slot-names (class-prototype unit-class)))
         (direct-slots
          (remove-if #'(lambda (slot)
                         (memq (slot-definition-name slot)
                               hidden-slot-names))
                     (class-direct-slots unit-class)))
         (effective-slots
          (remove-if #'(lambda (slot)
                         (memq (slot-definition-name slot)
                               hidden-slot-names))
                     (class-slots unit-class))))
    (flet ((name&abstract (class)
             (values (class-name class)
                     (and (typep class 'standard-unit-class)
                          (standard-unit-class.abstract class)))))
      (format t "~&~2tDirect superclasses:")
      (format-column 4 supers "~s~:[~; (abstract)~]" #'name&abstract)
      (format t "~&~2tDirect subclasses:")
      (format-column 4 subs "~s~:[~; (abstract)~]" #'name&abstract))
    ;; Direct slots:
    (format t "~&~2tDirect slots:~@[ None~%~]"
            (notany #'(lambda (slot) 
                        (not (typep slot 'direct-link-definition)))
                    direct-slots))
    (dolist (slot direct-slots)
      (unless (typep slot 'direct-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Direct link slots:
    (format t "~&~2tDirect link slots:~@[ None~%~]"
            (notany #'(lambda (slot) (typep slot 'direct-link-definition))
                    direct-slots))
    (dolist (slot direct-slots)
      (when (typep slot 'direct-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Effective slots:
    (format t "~&~2tEffective slots:")
    (dolist (slot effective-slots)
      (unless (typep slot 'effective-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Effective link slots:
    (format t "~&~2tEffective link slots:~@[ None~%~]"
            (notany #'(lambda (slot) (typep slot 'effective-link-definition))
                    effective-slots))
    (dolist (slot effective-slots)
      (when (typep slot 'effective-link-definition)
        (describe-unit-slot unit-class slot)))
    ;; Dimensional values:
    (let ((dvs (standard-unit-class.dimensional-values unit-class)))
      (format t "~&~2tDimensional values:~@[ None~%~]"
              (null dvs))
      (dolist (dv dvs)
        (describe-dimensional-value unit-class dv)))
    ;; Effective dimensional values:
    (let ((dvs (standard-unit-class.effective-dimensional-values unit-class)))
      (format t "~&~2tEffective dimensional values:~@[ None~%~]"
              (null dvs))
      (dolist (dv dvs)
        (describe-dimensional-value unit-class dv)))
    ;; Initial space instances:
    (let ((space-instances 
           (ensure-list
            (standard-unit-class.initial-space-instances
             unit-class))))
      (format t "~&~2tInitial space instances:~@[ None~%~]"
              (null space-instances))
      (dolist (space-instance space-instances)
        (describe-initial-space-instance unit-class space-instance)))
    ;; Effective initial space instances:
    (let ((space-instances 
           (ensure-list
            (standard-unit-class.effective-initial-space-instances
             unit-class))))
      (format t "~&~2tEffective initial space instances:~@[ None~%~]"
              (null space-instances))
      (dolist (space-instance space-instances)
        (describe-initial-space-instance unit-class space-instance)))
    ;; Retain:
    (format t "~&~2tRetain: ~s~%" (standard-unit-class.retain unit-class)))
  (values))
  
;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class (slot standard-slot-definition))
  (declare (ignore unit-class))
  (format t "~&~4t~s" (slot-definition-name slot))
  (let ((tab-column 6))
    (maybe-format-labeled-entry 
     tab-column (slot-definition-allocation slot) ':allocation)
    (maybe-format-labeled-entry 
     tab-column (documentation slot t) ':documentation)
    (maybe-format-labeled-entry
     tab-column (slot-definition-initargs slot) ':initargs)
    (maybe-format-labeled-entry 
     tab-column (slot-definition-initfunction slot) ':initform
     (slot-definition-initform slot))))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class 
                               (slot standard-direct-slot-definition))
  (declare (ignore unit-class))
  (call-next-method)
  (let ((tab-column 6))
    (maybe-format-labeled-entry 
     tab-column (slot-definition-readers slot) ':readers)
    (maybe-format-labeled-entry 
     tab-column (slot-definition-writers slot) ':writers)))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class (slot direct-link-definition))
  (call-next-method)
  (format t "~&~6t~s ~s ~:[<-- *** Inconsistent ***~;~]" 
          ':link 
          (direct-link-definition.inverse-link slot)
          (check-a-link unit-class slot nil))
  (when (direct-link-definition.singular slot)
    (format t "~&~6t~s ~s" ':singular 't)))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class
                               (slot standard-effective-slot-definition))
  (declare (ignore unit-class #+ecl slot))
  (call-next-method))

;;; ---------------------------------------------------------------------------

(defmethod describe-unit-slot (unit-class (slot effective-link-definition))
  (declare (ignore unit-class #+ecl slot))
  (call-next-method))

;;; ---------------------------------------------------------------------------

(defun describe-dimensional-value (unit-class dv)
  (declare (ignore unit-class))
  (destructuring-bind (dimension-name dimension-value-type value-fn
                       composite-type ordering-dimension-name)
      dv
    (declare (ignore value-fn))
    (format t "~&~6t~s ~s~@[ ~s~@[ ~s~]~]~%" 
            dimension-name 
            dimension-value-type
            composite-type
            ordering-dimension-name)))

;;; ---------------------------------------------------------------------------

(defun describe-initial-space-instance (unit-class space-instance-or-fn)
  (declare (ignore unit-class))
  (cond 
   ((functionp space-instance-or-fn)
    (format t "~&~6t")
    (print-pretty-function-object space-instance-or-fn)
    (terpri))
   (t (format t "~&~6t~s~%" space-instance-or-fn))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
