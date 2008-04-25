;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/unit-metaclasses.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Apr 25 01:58:22 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    GBBopen Unit-Class Metaclasses
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
;;;  06-13-05 Split out from units.lisp.  (Corkill)
;;;  05-31-06 Create a new instance-hash-table and copy existing instances
;;;           into it when instance-name-comparison-test function is changed.
;;;           (Corkill)
;;;  06-06-06 Rename conflicting instances resulting from a change to
;;;           instance-name-comparison-test (rather than asking for one to
;;;           be deleted), as suggested by Susan Lander. (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(dimensions-of
            direct-nonlink-slot-definition ; not yet documented
            direct-link-definition      ; not yet documented
            effective-link-definition   ; not yet documented
            effective-nonlink-slot-definition ; not yet documented
            gbbopen-node-state          ; not yet documented
            link-slot-p                 ; not yet documented
            restore-gbbopen-node-state  ; not yet documented
            save-gbbopen-node-state     ; not yet documented
            standard-space-class)))

;;; ---------------------------------------------------------------------------

(defun cannonical-hash-table-test (hash-table)
  (let ((hash-table-test (hash-table-test hash-table)))
    #-clisp hash-table-test
    ;; CLISP returns internal "fast" test names for eq, eql, and equal:
    #+clisp (case hash-table-test
              (ext:fasthash-eq 'eq)
              (ext:fasthash-eql 'eql)
              (ext:fasthash-equal 'equal)
              (otherwise hash-table-test))))

;;; ===========================================================================
;;;   Define variable for standard-unit-instance internal (regular) slot names 
;;;     (declared nil here, but added to in instances.lisp and spaces.lisp)

(defvar *internal-unit-instance-slot-names* nil)

;;; ===========================================================================
;;;   Standard-Unit-Class  (Metaclass of Standard-Unit-Instance)

(define-class standard-unit-class (standard-class)
  ((abstract :initform nil :type boolean)
   (instance-hash-table)
   (instance-name-counter :initform 0)
   (instance-name-comparison-test :initform 'eql)
   (initial-space-instances 
    :type (or function list)
    :initform nil)
   (effective-initial-space-instances :initform nil)
   (dimensional-values :initform nil)
   (effective-dimensional-values 
    :type list
    :initform nil)
   (unit-class-dimensions 
    :initform nil
    :reader dimensions-of
    :writer (setf standard-unit-class.unit-class-dimensions))
   (evfn-blks :initform nil)
   ;; Controls if instances are deleted by reset-gbbopen (unless overridden by
   ;; an :all-classes reset):
   (retain :initform nil))
  (:export-class-name t)
  (:generate-accessors-format :prefix)
  (:generate-accessors t :exclude unit-class-dimensions))

;;; ---------------------------------------------------------------------------

(defmethod dimensions-of ((unit-class-name symbol))
  ;;; Support unit-class name in addition to unit-class method (above)
  (if (eq unit-class-name 't)
      (dimensions-of '(standard-unit-instance :plus-subclasses))
      (dimensions-of (find-unit-class unit-class-name))))

;;; ---------------------------------------------------------------------------

(defmethod dimensions-of ((unit-classes-specifier cons))
  ;;; Support unit-classes-specifiers:
  (let ((result nil))
    (map-unit-classes-specifier
     #'(lambda (unit-class) 
         (dolist (dimension (dimensions-of unit-class))
           (pushnew dimension result :test #'equal)))
     unit-classes-specifier)
    result))

;;; ---------------------------------------------------------------------------

(defun print-unit-class-state-for-saving/sending (unit-class stream)
  (format stream "~&#GM(~s ~s)"
          (class-name unit-class)
          (standard-unit-class.instance-name-counter unit-class)))

;;; ---------------------------------------------------------------------------
;;;  Standard-unit-instance-updater reader

(defmethod saved/sent-object-reader ((char (eql #\M)) stream)
  (destructuring-bind (class-name instance-name-counter)
      (read stream t nil 't)
    (let ((unit-class (find-class class-name)))
      (if (and unit-class
               (typep unit-class 'standard-unit-class))
          (setf (standard-unit-class.instance-name-counter unit-class)
                instance-name-counter)
          (warn "Unable to update the instance-name count of unit-class ~s"
                class-name)))))
        
;;; ---------------------------------------------------------------------------

(defun generate-renaming-instance-name (instance-name counter)
  (format nil "~a.RENAMED~:[.~a~;~]"
          instance-name
          (= 1 counter)
          counter))

;;; ---------------------------------------------------------------------------

(defun reinsert-instance-into-hash-table (instance-name instance hash-table)
  ;; Helper function to deal with name-classes resulting from changes to
  ;; :instance-name-comparison-test (called by shared-initialize :around
  ;; method, below):
  (let ((existing-instance (gethash instance-name hash-table)))
    (when existing-instance
      ;; A conflict, rename with a renaming-name indicator:
      (let ((counter 0)                 ; we will start with 1...
            (existing-instance-name (instance-name-of existing-instance))
            renaming-instance-name)
        (while (gethash (setf renaming-instance-name 
                              (generate-renaming-instance-name 
                               existing-instance-name
                               (incf counter)))
                        hash-table))
        (warn "Renaming instance ~s of class ~s to ~s to avoid instance-name ~
               conflict"
              existing-instance-name
              (type-of instance)
              renaming-instance-name)
        (setf (instance-name-of existing-instance)
              renaming-instance-name)))
    (setf (gethash instance-name hash-table) instance)))

;;; ---------------------------------------------------------------------------

(defmethod shared-initialize :around ((class standard-unit-class) slot-names
                                      &rest initargs &key)
  (let ((class
         (apply #'call-next-method class slot-names
                (loop for (indicator value) on initargs by #'cddr
                    nconc
                      (case indicator
                        ;; single-value class options:
                        ((:abstract :instance-name-comparison-test :retain)
                         (list indicator (sole-element value)))
                        (otherwise (list indicator value)))))))
    ;; create/copy appropriate type of instance-hash-table:
    (cond 
     ;; must create a new instance-hash-table:
     ((or (eq slot-names 't)
          (memq 'instance-name-comparison-test slot-names))
      (setf (standard-unit-class.instance-hash-table class)
            (make-hash-table 
             :test (standard-unit-class.instance-name-comparison-test
                    class))))
     ;; must copy instances into a new instance-hash-table (due to a change in
     ;; instance-name-comparison-test):
     ((not (eq (standard-unit-class.instance-name-comparison-test class)
               (cannonical-hash-table-test
                (standard-unit-class.instance-hash-table class))))
      (let ((hash-table-count
             (hash-table-count
              (standard-unit-class.instance-hash-table class))))
        (unless (zerop hash-table-count)
          (format t "~&;; Converting instance-name comparison test of ~s ~
                          to ~s (do not interrupt)...~%"
                  (class-name class)
                  (standard-unit-class.instance-name-comparison-test class)))
        (with-lock-held (*master-instance-lock*)
          (let ((old-ht (standard-unit-class.instance-hash-table class))
                (new-ht 
                 (make-hash-table 
                  :test (standard-unit-class.instance-name-comparison-test
                         class))))
            (setf (standard-unit-class.instance-hash-table class) new-ht)
            (maphash 
             #'(lambda (instance-name instance)
                 (reinsert-instance-into-hash-table
                  instance-name instance new-ht))
             old-ht)))
        (unless (zerop hash-table-count)
          (format t "~&;; Conversion of instance-name comparison test ~
                          completed.~%")))))
    ;; Return the class:
    class))

;;; ---------------------------------------------------------------------------

(defmethod validate-superclass ((class standard-unit-class) 
                                (superclass standard-class))
  #+ecl (declare (ignore class superclass))
  't)

;;; ---------------------------------------------------------------------------

;;; Check on finalization that a unit-class inherits from 
;;; standard-unit-instance:
(defmethod finalize-inheritance :after ((class standard-unit-class))
  (let ((standard-ui-class (find-class 'standard-unit-instance nil)))
    ;; check that the standard-unit-instance class is present (it may not be
    ;; during standard-unit-instance's own definition if finalization is
    ;; called early -- MCL requires this check)
    (when standard-ui-class
      (unless (memq standard-ui-class (class-precedence-list class))
        (error "Unit class ~s does not inherit from ~s.  Add ~:*~s as a ~
               superclass in ~s's ~s definition or use ~s."
               class
               'standard-unit-instance
               (class-name class)
               'define-unit-class
               'define-class)))))

;;; ---------------------------------------------------------------------------

;;; Handle Lispwork's non-standard use of slot-name rather than slot-object.
;;; (Setf forms are handled individually in instances.lisp and links.lisp.)
#+lispworks
(defmethod slot-value-using-class ((class standard-unit-class)
                                   instance 
                                   (slot standard-slot-definition))
  (slot-value-using-class class instance (slot-definition-name slot)))
#+lispworks
(defmethod slot-boundp-using-class ((class standard-unit-class) 
                                    instance 
                                    (slot standard-slot-definition))
  (slot-boundp-using-class class instance (slot-definition-name slot)))
 
;;; ---------------------------------------------------------------------------
;;;  Multinode support

(define-class gbbopen-node-state (standard-gbbopen-instance)
  (node-name
   (unit-classes :initform nil)))
  
;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((node-state gbbopen-node-state) stream)
  (call-next-method)
  (format stream " ~s"
          (if (slot-boundp node-state 'node-name)
              (node-name-of node-state)
              "Uninitialized")))

;;; ---------------------------------------------------------------------------
  
(define-class unit-class-state ()
  (unit-class
   instance-name-counter
   instance-hash-table))

;;; ---------------------------------------------------------------------------
  
(defmethod save-gbbopen-node-state ((node-state gbbopen-node-state))
  (let ((unit-class-states nil))
    (map-extended-unit-classes
     #'(lambda (unit-class)
         (push (make-instance 'unit-class-state
                 :unit-class unit-class
                 :instance-name-counter 
                 (standard-unit-class.instance-name-counter unit-class)
                 :instance-hash-table
                 (standard-unit-class.instance-hash-table unit-class))
               unit-class-states))
     't)
    (setf (unit-classes-of node-state) unit-class-states)))

;;; ---------------------------------------------------------------------------

(defmethod restore-gbbopen-node-state ((node-state gbbopen-node-state))
  (dolist (unit-class-state (unit-classes-of node-state))
    (let ((unit-class (unit-class-of unit-class-state)))
      (setf (standard-unit-class.instance-name-counter unit-class)
            (instance-name-counter-of unit-class-state))
      (setf (standard-unit-class.instance-hash-table unit-class)
            (instance-hash-table-of unit-class-state)))))

;;; ===========================================================================
;;;   Direct link and non-link slot definitions
 
;; Common class for GBBopen direct link and non-link slots
(define-class gbbopen-direct-slot-definition
    (standard-direct-slot-definition)
  ()
  (:generate-accessors-format :prefix))

;;; ---------------------------------------------------------------------------

(define-class direct-nonlink-slot-definition (gbbopen-direct-slot-definition)
  ()
  (:generate-accessors-format :prefix))

;;; ---------------------------------------------------------------------------

(define-class direct-link-definition (gbbopen-direct-slot-definition)
  ((singular :type boolean :initform nil)
   (inverse-link :initarg :link :initform nil)
   (inverse-link-definition :initform nil)
   (sort-function :initform nil)
   (sort-key :initform nil))
  (:default-initargs :initform nil :initfunction #'(lambda () nil))
  (:generate-accessors-format :prefix))
  
;;; ---------------------------------------------------------------------------

;;; Check for accidentally quoted :singular values in link slots:
(defmethod shared-initialize :before ((slotd direct-link-definition)
                                      slot-names &rest initargs 
                                      &key singular name link)
  (declare (ignore #+ecl slotd slot-names initargs))
  ;; check :singular slot-option:
  (unless (or (eq singular 't)
              (eq singular 'nil))
    (error "The :singular value ~s for link-slot ~s is not t or nil"
           singular
           name))
  ;; check :singular value in inverse-link-definition:
  (cond ((eq link ':reflexive))
        (t (destructuring-bind (unit-class-name inverse-slot-name
                                &key singular)
               link
             (declare (ignore unit-class-name inverse-slot-name))
             (unless (or (eq singular 't)
                         (eq singular 'nil))
               (error "The :link value ~s for link-slot ~s contains a ~
                       :singular value that is not t or nil"
                      link
                      name))))))

;;; ---------------------------------------------------------------------------
;;;  Lispworks requires a slot-option handler method for all extended slot
;;;  options:

#+lispworks
(defmethod process-a-slot-option ((class standard-unit-class) 
                                  option value 
                                  already-processed-options slot)
  (case option
    (:link (list* :link `',value already-processed-options))
    (:singular (list* :singular `',value already-processed-options))
    (:sort-function (list* :sort-function `',value already-processed-options))
    (:sort-key (list* :sort-key `',value already-processed-options))
    (:initfunction (list* :initfunction `',value already-processed-options))
    (otherwise (call-next-method))))

;;; ---------------------------------------------------------------------------

(defmethod direct-slot-definition-class ((class standard-unit-class)
                                         &rest initargs)
  (declare (dynamic-extent initargs))
  (cond
   ;; Direct link slot
   ((getf initargs ':link)
    (load-time-value (find-class 'direct-link-definition)))
   ;; Direct non-link slot     
   ((not (memq (getf initargs ':name) *internal-unit-instance-slot-names*))
    (load-time-value (find-class 'direct-nonlink-slot-definition)))
   ;; Internal (regular CL) slot
   (t (call-next-method))))

;;; ===========================================================================
;;;   Effective link and non-link slot definitions
;;;
;;; Dynamic binding hack to make effective-link-definition inheritance passing
;;; work properly (bound in compute-effective-slot-definition).

(defvar *%%inherited-link-slot%%* nil)

;;; ---------------------------------------------------------------------------

;; Common class for GBBopen effective link and non-link slots
(define-class gbbopen-effective-slot-definition
    (standard-effective-slot-definition)
  ((evfn-blks :initform nil))
  (:generate-accessors-format :prefix))

;;; ---------------------------------------------------------------------------
  
(define-class effective-nonlink-slot-definition
    (gbbopen-effective-slot-definition)
  ()
  (:generate-accessors-format :prefix))
  
;;; ---------------------------------------------------------------------------
  
(define-class effective-link-definition (gbbopen-effective-slot-definition)
  ((direct-slot-definition :initform *%%inherited-link-slot%%*))
  (:generate-accessors-format :prefix))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((slot gbbopen-effective-slot-definition) stream)
  (cond (*print-readably* (call-next-method))
        (t (print-unreadable-object (slot stream :type nil)
             (format stream "~a ~s"
                     (if (typep slot 'effective-link-definition)
                         "Link-slot"
                         "Nonlink-slot")                
                     (slot-definition-name slot)))
           ;; Print-object must return object:
           slot)))

;;; ---------------------------------------------------------------------------

(defmethod link-slot-p ((slot slot-definition))
  nil)

(defmethod link-slot-p ((slot direct-link-definition))
  slot)

(defmethod link-slot-p ((slot effective-link-definition))
  slot)

;;; ---------------------------------------------------------------------------
  
(defmethod compute-effective-slot-definition ((class standard-unit-class) 
                                              slot-name
                                              direct-slot-definitions)
  (declare (ignore #+ecl class slot-name))
  (let ((most-specific-direct-slot-definition
         (first direct-slot-definitions))
        (*%%inherited-link-slot%%* nil))
    (when (typep most-specific-direct-slot-definition 'direct-link-definition)
      (setf *%%inherited-link-slot%%* most-specific-direct-slot-definition))
    (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod effective-slot-definition-class ((class standard-unit-class)
                                            &rest initargs)
  (declare (dynamic-extent #+ecl class initargs))
  (if *%%inherited-link-slot%%*
      (load-time-value (find-class 'effective-link-definition))
      (if (memq (getf initargs ':name) *internal-unit-instance-slot-names*)
          (call-next-method)
          (load-time-value (find-class 'effective-nonlink-slot-definition)))))

;;; ===========================================================================
;;;   Link and Nonlink Slot Writer Method Classes
;;;
;;; CMU and SBCL provide the writer-method-class GF, but they don't actually
;;; use it. (!).
;;;
;;; Currently, we only use GBBopen writer methods as an informative class name
;;; when supported (unfortunate, as specific writer methods would support some
;;; clean event-signaling interfaces...)

(define-class nonlink-writer-method (standard-writer-method)
  ())

(defmethod writer-method-class ((class standard-unit-class)
                                (slot standard-direct-slot-definition)
                                &rest initargs)
  #+(or clisp clozure cmu digitool-mcl ecl sbcl scl)
  (declare (ignore #+ecl class #+ecl slot initargs))
  #+(or cmu sbcl)
  (find-class 'nonlink-writer-method)
  #-(or cmu sbcl)
  (load-time-value (find-class 'nonlink-writer-method)))

;;; ---------------------------------------------------------------------------

(define-class link-writer-method (standard-writer-method)
  ())

(defmethod writer-method-class ((class standard-unit-class)
                                (slot direct-link-definition)
                                &rest initargs)
  #+(or clisp clozure cmu digitool-mcl ecl sbcl scl)
  (declare (ignore #+ecl class #+ecl slot initargs))
  #+(or cmu sbcl)
  (find-class 'link-writer-method)
  #-(or cmu sbcl)
  (load-time-value (find-class 'link-writer-method))) 
  
;;; ---------------------------------------------------------------------------
  
(defmethod validate-class-option ((metaclass standard-unit-class) option)
  #+ecl (declare (ignore metaclass))
  (or (memq (car option) 
            '(:abstract :instance-name-comparison-test :retain))
      (call-next-method)))

;;; ===========================================================================
;;;   Standard-Space-Class  (Metaclass of Standard-Space-Instance and friends)

(define-class standard-space-class (standard-unit-class)
  ()
  ;; Metaclass options must be lists (extraction of single-value class options
  ;; is done later on for single-value options):
  (:default-initargs :instance-name-comparison-test '(equal)))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================
