;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/storage.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat May 10 04:00:27 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                   Space-Instance Storage Management
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-18-03 File created.  (Corkill)
;;;  03-17-04 Fix applicable-storage-object-p.  (Corkill)
;;;  05-04-04 Converted unstructured storage to use an eq hash table rather
;;;           than a simple list.  (Corkill)
;;;  06-08-04 Add excess-locator counting.  (Corkill)
;;;  06-08-04 Add unit-classes-spec support.  (Corkill)
;;;  05-27-05 Change storage-objects-for-retrieval to allow space-instance-path
;;;           regexps. (Corkill)
;;;  02-20-06 Add hashed storage support.  (Corkill)
;;;  04-23-06 Split out individual storage mechanisms.  (Corkill)
;;;  06-11-07 Converted storage accessors from :prefix to modern "-of"
;;;           format.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(boolean
	    describe-space-instance-storage ; not yet documented
	    hashed
	    infinite-interval		; not yet documented
	    quadtree
	    uniform-buckets
	    unstructured)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Some implementations (SBCL) are very strict on eql constant redefinition,
  ;; so avoid redefinition by checking for a bound value:
  (unless (boundp 'infinite-interval)
    (defconstant infinite-interval '#.(list -infinity infinity))))

;;; ---------------------------------------------------------------------------

(defstruct (find-stats
            (:conc-name #.(dotted-conc-name 'find-stats))
            (:copier nil))
  (number-of-finds 0 :type integer)
  (number-using-marking 0 :type integer)
  (run-time 0 :type integer)
  (bucket-count 0 :type integer)
  (instances-touched 0 :type integer)
  (instances-considered 0 :type integer)
  (instances-accepted 0 :type integer))

(defvar *find-stats* nil)

;;; ===========================================================================
;;;   Storage objects
;;;

(define-class storage ()
  ((space-instance)
   (stores-classes)
   (dimension-names :initform 't)
   (stores-all-instances :initform 't)   
   (excess-locators :initform 0)
   (instance-counts :initform nil)))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((storage storage) stream)
  (cond 
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (storage stream :type nil)
	(let ((classes (when (slot-boundp storage 'stores-classes)
			 (stores-classes-of storage))))
	  (format stream "~:(~s~) ~:[???~;(~:*~{~{~s~@[+~*~]~}~})~] ~s"
		  (type-of storage)
		  (mapcar 
		   #'(lambda (class-spec)
		       (destructuring-bind (unit-class . plus-subclasses)
			   class-spec
			 `(,(class-name unit-class) ,plus-subclasses)))
		   classes)
		  (and (slot-boundp storage 'dimension-names)
		       (dimension-names-of storage)))))
      ;; Print-object must return object:
      storage)))

;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-storage :after ((instance standard-unit-instance)
					   storage
					   verbose)
  (declare (ignore verbose))
  (let ((unit-class-name (type-of instance)))
    (pushnew/incf-acons unit-class-name 1 (instance-counts-of storage)
                        :test #'eq)))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-storage :after 
	   ((instance standard-unit-instance)
	    storage old-dimension-values dimensions-being-changed verbose)
  (declare (ignore old-dimension-values dimensions-being-changed verbose))
  ;; Decf/delete the instance-count:
  (decf/delete-acons (type-of instance) 1 (instance-counts-of storage)
                     :test #'eq))

;;; ---------------------------------------------------------------------------

(defun storage-objects-for-add/move/remove (unit-class space-instance)
  ;;; Finds all storage objects for instances of `unit-class' on 
  ;;; `space-instance'.  Used for add/move/remove unit-instance operations.
  (let ((result nil)
	(plus-subclasses-storage nil))
    (dolist (storage (standard-space-instance.%%storage%% space-instance))
      (dolist (class-spec (stores-classes-of storage))
	(destructuring-bind (stores-class . plus-subclasses)
	    class-spec
	  (cond 
	   ;; always include exact class matches:
	   ((eq unit-class stores-class)
	    (push storage result))
	   ;; also include first ("best") plus-subclasses match:
	   ((and (not plus-subclasses-storage)
		 plus-subclasses
		 (memq stores-class
		       (the list (class-precedence-list unit-class))))
	    (push storage plus-subclasses-storage))))))
    (or result
	plus-subclasses-storage
	(internal-error "Unable to find storage for ~s on ~s."
			unit-class space-instance))))

;;; ---------------------------------------------------------------------------
;;;   Storage object lookup

(defun determine-retrieval-dimensions (space-instance 
				       disjunctive-dimensional-extents)
  ;;; Returns the dimensional intersection of `space-instance' and
  ;;; `disjunctive-dimensional-extents'
  (let ((space-instance-dimensions (dimensions-of space-instance)))
    (declare (type list space-instance-dimensions))
    (if (eq disjunctive-dimensional-extents 't)
	space-instance-dimensions 
	(let ((result nil))
	  (dolist (dimensional-extents disjunctive-dimensional-extents)
	    (dolist (extent dimensional-extents)
	      (let ((dimension (car extent)))
		(when (member dimension space-instance-dimensions 
			      :key #'car :test #'eq)
		  (pushnew dimension result)))))
	  result))))
    
;;; ---------------------------------------------------------------------------

(defun applicable-storage-object-p (storage unit-class plus-subclasses)
  ;;; Returns true if the storage object `storage' is relevant given
  ;;; `unit-class' and `plus-subclasses'
  (declare (inline class-name))
  (let* ((unit-class-name (class-name unit-class))
         (stores-classes (stores-classes-of storage)))
    ;; `storage' is a candidate if it:
    (member-if 
     #'(lambda (class-spec)
         (declare (inline class-name))
         (destructuring-bind (stores-class . stores-subclasses-p)
             class-spec
           (or
            ;; * stores the unit class:
            (eq unit-class stores-class)
            ;; * stores a parent class plus subclasses:
            (and stores-subclasses-p
                 (subtypep unit-class-name (class-name stores-class)))
            ;; * subclasses are desired (:plus-subclasses was specified for
            ;;   retrieval) and `storage' stores a subclasses of
            ;;   `unit-class':
            (and plus-subclasses 
                 (subtypep (class-name stores-class) unit-class-name)))))
     stores-classes)))

;;; ---------------------------------------------------------------------------

#+under-development
(defun best-retrieval-storage (unit-class plus-subclasses space-instance
			       disjunctive-dimensional-extents)
  (let ((retrieval-dimensions
	 (determine-retrieval-dimensions 
	  space-instance disjunctive-dimensional-extents))
	(exact-unit-storage nil)
	(best-dimensions nil)
	(subclass-unit-storage nil))
    (when retrieval-dimensions
      (dolist (storage (standard-space-instance.%%storage%% space-instance))
	(let ((stores-classes (stores-classes-of storage))
	      (dimension-names (dimension-names-of storage)))
	  ;; exact class match:
	  (when (eq unit-class stores-class)
	    (cond
	     ;; see if this one's better:
	     (exact-unit-storage nil)
	     ;; a new exact match:
	     (t (setf exact-unit-storage storage)))))))))
		
;;; ---------------------------------------------------------------------------

(defun storage-objects-for-retrieval (unit-classes-spec space-instances
				      disjunctive-dimensional-extents
				      pattern mapping-only invoking-fn-name)
  ;;; Returns the best storage objects for retrieving instances of 
  ;;; `unit-classes-spec' on `space-instances' given 
  ;;; `disjunctive-dimensional-extents'
  (when space-instances
    (let ((result nil))
      (dolist (unit-class-spec (parse-unit-classes-specifier 
				unit-classes-spec))
	(destructuring-bind (unit-class . plus-subclasses)
	    unit-class-spec
	  (flet ((do-si (space-instance)
		   (when (allowed-unit-classes-of space-instance)
		     (let ((retrieval-dimensions
			    (unless mapping-only
			      (determine-retrieval-dimensions 
			       space-instance 
			       disjunctive-dimensional-extents))))
		       (when (or mapping-only
				 retrieval-dimensions)
			 (dolist (storage (standard-space-instance.%%storage%%
					   space-instance))
			   (when (applicable-storage-object-p 
				  storage unit-class plus-subclasses)
			     (pushnew storage result))))))))
	    (cond ((eq space-instances 't)
		   (map-space-instances #'do-si '(*) invoking-fn-name))
		  (t (map-space-instances
		      #'do-si
		      (ensure-list space-instances)
		      invoking-fn-name))))))
      (when (and (null result) (not mapping-only)
		 *warn-about-unusual-requests*)
	(warn "None of the retrieval dimensions of pattern ~w ~_overlap with ~
               those of unit ~:[class~;classes~] ~s on ~s."
	      pattern
	      (consp unit-classes-spec)
	      unit-classes-spec
	      space-instances))      
      result)))

;;; ---------------------------------------------------------------------------
;;;  Syntax Reminder --
;;;
;;;  <unit-classes-spec> :== t | <single-class-spec> | (<single-class-spec>+)
;;;  <single-class-spec> :== <class-spec> | (<class-spec> <subclass-indicator>)
;;;  <class-spec> :==  <class> | <class-name>
;;;  <subclass-indicator> :==  :plus-subclasses | :no-subclasses
;;;
;;; ---------------------------------------------------------------------------

(defun ill-formed-unit-classes-spec (unit-classes-spec)
  (error "Ill-formed unit-classes specification: ~s"
         unit-classes-spec))

;;; ---------------------------------------------------------------------------

(defun set-all-mbr-instance-marks (storage disjunctive-dimensional-extents)
  (map-all-instances-on-storage #'set-mbr-instance-mark
                                storage disjunctive-dimensional-extents nil))

;;; ---------------------------------------------------------------------------

(defun print-storage-usage-message (storage)
  (format *trace-output* 
          "~&;; * Space instance: ~s~%"
          (instance-name-of (space-instance-of storage))))

;;; ---------------------------------------------------------------------------

(defun missing-layout-option (storage initargs)
  (error "Required storage ~s option was not specified ~
                   for dimension~p~:* ~s of unit-class ~s on ~s."
	 ':layout
	 (dimension-names-of storage)
	 (getf initargs ':stores-classes)
	 (instance-name-of (space-instance-of storage))))

;;; ---------------------------------------------------------------------------

(defun missing-test-option (storage initargs)
  (error "Required storage ~s option was not specified ~
                   for dimension~p~:* ~s of unit-class ~s on ~s."
	 ':test
	 (dimension-names-of storage)
	 (getf initargs ':stores-classes)
	 (instance-name-of (space-instance-of storage))))

;;; ---------------------------------------------------------------------------

(defun check-storage-dimensions/layout-lengths (storage layout initargs)
  (let* ((dimension-names (dimension-names-of storage))
	 (storage-dimension-length (length dimension-names))
	 (layout-length (length layout)))
    (flet ((the-error (many/few-string)
	     (error "Too ~a layout specifications supplied in ~w ~
                   for dimension~p~:* ~s of unit-class ~s on ~s."
		    many/few-string
		    layout
		    dimension-names
		    (getf initargs ':stores-classes)
		    (instance-name-of (space-instance-of storage)))))
      (when (>& layout-length storage-dimension-length)
	(the-error "many"))
      (when (<& layout-length storage-dimension-length)
	(the-error "few")))))

;;; ===========================================================================
;;;   Storage Initialization

(defmethod setup-instance-storage ((space-instance standard-space-instance)
                                   (storage-spec null))
  ;;; Sets up the default storage for `space-instance', when none was
  ;;; specified.  No storage is created for space instances that
  ;;; cannot store any unit instances.
  #+ecl (declare (ignore storage-spec))
  (setf (standard-space-instance.%%storage%% space-instance)
	(if (allowed-unit-classes-of space-instance)
	    (list (do-storage space-instance '(t t unstructured)))
	    nil)))

;;; ---------------------------------------------------------------------------

(defmethod setup-instance-storage ((space-instance standard-space-instance)
                                   (storage-spec cons))
  ;;; Sets up the specified storage for `space-instance'
  (unless (allowed-unit-classes-of space-instance)
    (error "Space instance ~s does not allow any unit instances."
	   space-instance))	 
  (let ((result nil))
    (cond
     ;; single storage specification:
     ((and (cddr storage-spec)
	   (atom (third storage-spec)))
      (push (do-storage space-instance storage-spec) result))
     ;; multiple storage specifications:
     (t (dolist (single-storage-spec storage-spec)
	  (push (do-storage space-instance single-storage-spec)
		result))))
    ;; be sure to include the full-coverage storage, just in case --
    ;; we could be a bit smarter about determining if we have full coverage
    ;; (checking against allowed-unit-classes/dimensions), but we simply
    ;; play it very safe for now:
    (unless (member-if
	     #'(lambda (storage)
		 (and (member
		       (load-time-value 
			`(,(find-class 'standard-unit-instance) . t))
		       (the list (stores-classes-of storage))
		       :test #'equal)
		      (eq (dimension-names-of storage) 't)))
	     result)
      (push (do-storage space-instance '(t t unstructured))
	    result))
    (setf (standard-space-instance.%%storage%% space-instance)
	  (nreverse result))))

;;; ---------------------------------------------------------------------------

(defun do-storage (space-instance storage-spec)
  ;;; Creates all of the storage objects for `space-instance' given
  ;;; `storage-spec'
  (destructuring-bind (unit-classes-spec dimension-names storage-class
		       &rest args)
      storage-spec
    (declare (dynamic-extent args))
    (unless (eq dimension-names 't)
      (setf dimension-names (ensure-list dimension-names)))
    (let ((stores-classes (parse-unit-classes-specifier unit-classes-spec)))
      (apply #'make-instance
	     (case storage-class
	       (unstructured 'unstructured-storage)
	       (boolean 'boolean-storage)
	       (uniform-buckets
		(case (length dimension-names)
		  (1 '1d-uniform-buckets)
		  (2 '2d-uniform-buckets)
		  (otherwise 'uniform-buckets)))
	       (hashed 'hashed-storage)
	       (quadtree 
		(nyi))
	       ;; Allow for user-defined storage-classes:
	       (otherwise storage-class))
	     :space-instance space-instance
	     :stores-classes stores-classes
	     :dimension-names dimension-names
	     ;; Storage options:
	     args))))

;;; ===========================================================================
;;;  Space-Instance Description

(defmethod describe-space-instance-storage
    ((space-instance standard-space-instance))
  (dolist (storage (standard-space-instance.%%storage%% space-instance))
    (let* ((instance-counts (instance-counts-of storage))
	   (sorted-instance-counts 
	    (and instance-counts
		 (sort instance-counts #'string< :key #'car)))
	   (total-instances (if instance-counts
				(reduce #'(lambda (a b) (+& a b))
					instance-counts
					:key #'cdr)
				0)))
      (format t "~&~s ~s ~s ~s~%~{~4t~s~%~}"
	      (class-name (class-of storage))
	      (stores-classes-of storage)
	      (dimension-names-of storage)
	      (if (zerop total-instances) 
		  1.0
		  (/ (+& total-instances (excess-locators-of storage))
		     (float total-instances)))
	      sorted-instance-counts))))

;;; ---------------------------------------------------------------------------

(defmethod describe-space-instance-storage ((space-instance cons))
  (describe-space-instance-storage
   (find-space-instance-by-path space-instance ':with-error)))

;;; ---------------------------------------------------------------------------

(defun print-space-instance-storage-summary (space-instance)
  ;;; Internal function called by describe-blackboard-repository
  (when (allowed-unit-classes-of space-instance)
    (let ((total 0)
          (instance-counts (standard-space-instance.instance-counts
                         space-instance)))
      (when instance-counts
        (setf total (reduce #'(lambda (a b) (+& a b))
                            instance-counts 
                            :key #'cdr)))
      (cond 
       ;; Empty space:
       ((zerop total) (format t "Empty"))
       ;; Show what we've got:
       (t (format t "~s instance~:p (" total)
          (let ((first-time-p t))
            (dolist (count-acons 
                        (sort instance-counts #'string< :key #'car)) 
              (if first-time-p (setf first-time-p nil) (format t "; "))
              (format t "~s ~s"
                      (cdr count-acons) 
                      (car count-acons))))
          (format t ")"))))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
