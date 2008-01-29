;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/gbbopen/spaces.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jan 29 10:58:56 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  Space Definition & Instance Functions
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
;;;  06-06-03 File Created.  (Corkill)
;;;  05-27-04 Added clear-space-instances.  (Corkill)
;;;  07-07-04 Renamed define-space-instance-class to define-space-class and
;;;           standard-space-instance-class to standard-space-class.  (Corkill)
;;;  07-15-04 Added map-space-instances.  (Corkill)
;;;  09-23-04 Added return value for add-instance-to-space-instance and
;;;           remove-instance-from-space-instance.  (Corkill)
;;;  04-12-05 Don't warn about no common dimensions when both instance and 
;;;           space have no dimensions at all.  (Corkill)
;;;  05-27-05 Add space-instance-path regexp support to map-space-instances. 
;;;           (Corkill)
;;;  11-19-05 Added delete-all-space-instances.  (Corkill)
;;;  02-24-06 Added allowed-unit-classes reader.  (Corkill)
;;;  08-20-06 Added do-space-instances syntactic sugar.  (Corkill)
;;;  10-05-06 Added space-class metaclass check.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(?                           ; path reg-exp operator
            ^                           ; path reg-exp operator
            add-instance-to-space-instance
            all-space-instances
            allowed-unit-classes
            clear-space-instances
            children                    ; standard-space-instance slot name
            define-space-class
            delete-all-space-instances
            delete-space-instance
            describe-blackboard-repository
            describe-space-instance     ; not yet documented
            dimensions                  ; standard-space-instance slot name
            do-space-instances
            find-space-instance-by-path
            find-space-instances
            make-instance               ; part of CL, but here as a reminder
                                        ; of importance!
            make-space-instance
            map-space-instances
            parent                      ; standard-space-instance slot name
            remove-instance-from-space-instance
            root-space-instance         ; not documented
            show-forward-path-event-functions ; debugging only; not documented
            space-instance-children
            space-instance-dimensions
            space-instance-parent
            space-name                  ; standard-space-instance slot name
            standard-space-instance)))

;;; ---------------------------------------------------------------------------
;;;  Symbols that cannot be used as space names

(defparameter *illegal-space-names* '(= ? + * ^ t nil root-space-instance))

;;; ===========================================================================
;;;   Space Instances
;;;
;;;   In GBBopen, space instances are subclasses (with their own
;;;   meta-subclasses) of unit instances, providing substantial
;;;   implementation-code reuse and great flexability in creating blackboard
;;;   repository structures

(defmacro define-space-class (space-class-name 
                              direct-superclass-names
                              direct-slots &rest options)
  `(define-unit-class ,space-class-name ,(or direct-superclass-names
                                             '(standard-space-instance))
     ,direct-slots          
     ,@(if (member :metaclass options :test #'eq :key #'first)
           options
           (cons `(:metaclass standard-space-class) options))))

;;; ===========================================================================
;;;   Root Space Instance

(define-space-class root-space-instance (standard-unit-instance)
  ;;; The class of the unique root space-instance.  A unique instance of this
  ;;; class is created automatically as the parent of all top-level space
  ;;; instances.  This is also the base class for standard-space-instance.
  ((space-name
    :accessor standard-space-instance.space-name)
   (children 
    :link (standard-space-instance parent :singular t)
    :reader space-instance-children))
  (:generate-accessors nil))

;;; ===========================================================================
;;;   Space-instance renaming
;;;
;;; This doesn't work on Lisps that optimize defclass slot-writer methods
;;; rather than calling the (setf slot-value-using-class) method.  With such
;;; Lisps, we would have to attach to all the slot writer methods.
;;;
;;; Note that Lispworks uses the :optimize-slot-access class option to control
;;; the use of slot reader/writer methods (so GBBopen must set this option to
;;; nil for unit classes--done in define-unit-class).

#-cmu
(defmethod (setf slot-value-using-class) :before
           (nv
            (class standard-unit-class)
            (instance root-space-instance)
            ;; instead of the effective-slot-definition, Lispworks provides
            ;; the slot name:
            (slot #+lispworks (eql 'instance-name)
                  #-lispworks effective-nonlink-slot-definition))
  (when #-lispworks (eq (slot-definition-name slot) 'instance-name)
        #+lispworks 't
     (when (and (slot-boundp-using-class class instance
                                         #+lispworks 'instance-name
                                         #-lispworks slot)
                (not (equal (slot-value-using-class class instance 
                                                    #+lispworks 'instance-name
                                                    #-lispworks slot)
                            nv)))
       (error "Attempt to rename the space-instance ~s as ~s.  ~
               Space-instance renaming is not currently supported in GBBopen."
              instance
              nv))))

;;; ---------------------------------------------------------------------------

(defvar *root-space-instance*   
    (make-instance 'root-space-instance 
      :instance-name 'root-space-instance))

;;; ===========================================================================
;;;   Standard Space Instance

(define-space-class standard-space-instance (root-space-instance)
  ((dimensions 
    :reader space-instance-dimensions
    :initform nil)      
   (allowed-unit-classes
    ;; actually, this slot contains a list of <unit-classes-spec>s
    :reader allowed-unit-classes
    :initform t)
   (%%storage-spec%%)
   (%%storage%%
    :initform nil)
   (%%evfn-unit-ht%% :initform (make-hash-table :test 'eq))
   (%%bb-widgets%% :initform nil)
   (parent 
    :link (root-space-instance children)
    :singular t
    :reader space-instance-parent))
  (:generate-accessors-format :prefix)
  (:generate-accessors t :exclude allowed-unit-classes dimensions parent))

(defmethod allowed-unit-classes ((space-instance cons))
  (allowed-unit-classes 
   (find-space-instance-by-path space-instance ':with-error)))

;;; ---------------------------------------------------------------------------

(defmethod hidden-nonlink-slot-names ((instance standard-space-instance))
  (append '(%%evfn-unit-ht%% %%bb-widgets%% %%storage-spec%% %%storage%%)
          (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance ((instance standard-space-instance)
                                &key instance-name make-parents storage
                                     (allowed-unit-classes 't))
  (declare (inline class-of))
  ;; Verify that user-defined space-classes have the correct metaclass:
  (let ((metaclass (class-of instance)))
    (check-type metaclass standard-space-class))
  (check-type instance-name cons)
  (check-type allowed-unit-classes (or list (eql t)))
  (multiple-value-bind (parent-path space-name)
      (splitting-butlast instance-name)
    ;; space-name is the car of the space-name butlast tail
    (setf space-name (car space-name))
    (when (memq space-name *illegal-space-names*)
      (error "Illegal space name ~s in ~s."
             space-name
             instance-name))
    (let ((parent-space-instance 
           (and parent-path
                (find-space-instance-by-path parent-path))))
      (when parent-path
        (unless parent-space-instance
          (if make-parents
              (setf parent-space-instance
                (make-space-instance 
                 parent-path
                 :allowed-unit-classes allowed-unit-classes
                 :make-parents t))
              (error "A parent space instance for ~s does not exist."
                     instance-name))))
      (call-next-method)
      ;; Fixup a single extended-unit-class specification:
      (setf (slot-value instance 'allowed-unit-classes)
            (ensure-unit-classes-specifiers allowed-unit-classes))
      (setf (standard-space-instance.space-name instance) space-name)
      (setf (standard-space-instance.%%storage-spec%% instance) storage)
      (linkf (slot-value instance 'parent)
             (if parent-space-instance
                 parent-space-instance
                 *root-space-instance*))))
  (setup-instance-storage instance storage))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((instance standard-space-instance) &key)
  ;; This *must* be done in an :after method, so that the new space-instance has
  ;; been added to the class hash-table (by the above :around/primary method): 
  (setup-space-instance-evfns instance))

;;; ---------------------------------------------------------------------------

(defmethod delete-instance ((space-instance standard-space-instance))
  (map-instances-on-space-instances
   #'(lambda (instance)
       ;; Inconsistent locators can result in storage pointers to deleted unit
       ;; instances, so we check for deletion before attempting removal:
       (unless (instance-deleted-p instance)
         (remove-instance-from-space-instance instance space-instance)))
   't
   space-instance)
  (mapc #'delete-space-instance 
        (space-instance-children space-instance))
  (call-next-method))
  
;;; ---------------------------------------------------------------------------
;;; Syntactic sugar space-instance functions...

(defun make-space-instance (path 
                            &rest initargs 
                            &key (class 'standard-space-instance) 
                                 instance-name
                            &allow-other-keys)
  (declare (dynamic-extent initargs))
  (when (and instance-name
             (not (equal path instance-name)))
    (error "The instance-name of a space instance must be its path; ~s was ~
            specified."
           instance-name))
  (apply #'make-instance class 
         :instance-name path 
         (remove-property initargs :class)))

;;; ---------------------------------------------------------------------------

(defun find-space-instance-by-path (path 
                                    ;; undocumented optional, for internal
                                    ;; error checking use
                                    &optional errorp)
  (or (find-instance-by-name path '(standard-space-instance :plus-subclasses))
      (when errorp
        (error "Space instance ~s does not exist." path))))

;;; ---------------------------------------------------------------------------

(defmethod delete-space-instance ((space-instance standard-space-instance))
  (delete-instance space-instance))

(defmethod delete-space-instance ((space-instance cons))
  (delete-space-instance 
   (find-space-instance-by-path space-instance ':with-error)))

;;; ---------------------------------------------------------------------------
;;;   Delete-all-space-instances
;;;
;;; Deletes all space instances (except for the "hidden" root-space-instance).
;;;
;;; Care must be taken when deleting instances that delete other instances
;;; by side-effect.  For example:
;;;   (map-instances-of-class #'delete-space-instance
;;;                           '(standard-space-instance :plus-subclasses))
;;; will most likely violate the bottom-up deletion contract for space
;;; instances from the space-instance-path tree, because deleting a
;;; space-instance automatically deletes all of its children.  A very careful
;;; way to delete all space instances is:
;;;    (dolist (space-instance (find-space-instances '(?)))
;;;       (delete-space-instance space-instance))
;;; However, the following traversal-based approach is also safe and slightly
;;; more efficient:

(defun delete-all-space-instances ()
  (dolist (space-instance (space-instance-children *root-space-instance*))
    (delete-space-instance space-instance)))

;;; ---------------------------------------------------------------------------
;;;   Clear-space-instances

(defun clear-space-instances (space-instances)
  ;;; Removes (but does not delete) all unit instances from `space-instances'
  (declare (dynamic-extent args))
  (map-instances-on-space-instances
   #'(lambda (instance)
       (dolist (space-instance (space-instances-of instance))
         (remove-instance-from-space-instance instance space-instance)))
   't space-instances))

;;; ===========================================================================
;;;   Saving/Sending Space Instances

(defmethod omitted-slots-for-saving/sending ((instance standard-space-instance))
  (append '(space-name                  ; recomputed from instance-name
            %%storage%%
            %%bb-widgets%%) 
          (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-gbbopen-save/send-instance
    ((instance standard-space-instance) slots slot-values missing-slot-names)
  (declare (ignore slots slot-values missing-slot-names))
  (setf (standard-space-instance.space-name instance)
        (car (last (instance-name-of instance))))
  (call-next-method)
  (setup-instance-storage
   instance (standard-space-instance.%%storage-spec%% instance)))

;;; ===========================================================================
;;;   Specialized regular-expression-like matcher used by find-space-instances

(defun path-match (complete-pattern candidate)
  (labels 
      ((match (pattern candidate)
         (cond 
          ((eq pattern 't) 't)
          ((and (null candidate) (null pattern)) 't)
          (t (let ((element (car pattern)))
               (cond 
                ((eq element (car candidate))
                 (match (cdr pattern) (cdr candidate)))
                ;; one occurence:
                ((eq element '=) 
                 (and candidate
                      (match (cdr pattern) (cdr candidate))))
                ;; zero or one occurence:
                ((eq element '?)
                 (or (match (cdr pattern) candidate)
                     (match (cdr pattern) (cdr candidate))))
                ;; one or more occurences:
                ((eq element '+) 
                 (let ((pattern (cdr pattern)))
                   (when candidate
                     (do ((candidate (cdr candidate) (cdr candidate)))
                         ((null candidate) (null pattern))
                       (when (match pattern candidate)
                         (return 't))))))
                ;; zero or more occurences:
                ((eq element '*) 
                 (let ((pattern (cdr pattern)))
                   (do ((candidate candidate (cdr candidate)))
                       ((null candidate) (null pattern))
                     (when (match pattern candidate)
                       (return 't)))))
                ;; move to parent (illegal in this context):
                ((eq element '^) 
                 (error "^ (move to parent) was not used relative to a ~
                         space instance in pattern ~s." 
                        complete-pattern))))))))
    (match complete-pattern candidate)))

;;; ---------------------------------------------------------------------------

(defun path-relative-match (pattern)
  ;;; A regular-expression-like matcher for paths
  (let ((reversed-pattern (reverse (instance-name-of (car pattern))))
        (remainder (cdr pattern))
        (no-more-^s nil))
    (while remainder
      (let ((element (car remainder)))
        (cond
         ((eq element '^)
          (when no-more-^s
            (error "^ (move to parent) follows a wildcard element in ~
                   pattern ~s." 
                   pattern))
          (unless reversed-pattern
            (error "No parent space instance for ^ (move to parent) in ~
                   pattern ~s."
                   pattern))
          (pop reversed-pattern)
          (pop remainder))
         ((memq element '(= ? + *))
          (push element reversed-pattern)
          (pop remainder)
          (setf no-more-^s t))
         (t (push element reversed-pattern)
            (pop remainder)))))
    (nreverse reversed-pattern)))

;;; ---------------------------------------------------------------------------

(defun subsumed-path-pattern-p (candidate-path-pattern path-pattern)
  ;;; Returns true if `candidate-path-pattern' is fully subsumed by 
  ;;; `path-pattern' (conservative guess, if uncertain).  Called by
  ;;; rmfrom-evfn-using-class (in events.lisp).
  (cond 
   ((equal path-pattern '(*)) 't)
   ((equal candidate-path-pattern path-pattern) 't)))

;;; ===========================================================================
;;;   Map/Find Space Instances
;;;
;;;   Example: find all space-instances (find-space-instances '(*))

(defun map-space-instances (fn pattern &optional invoking-fn-name)
  ;;; Applies `fn' to all space instances that match `pattern'.  If
  ;;; `invoking-fn-name' is supplied and no space instances match
  ;;; the pattern, a warning is issued.
  (flet ((no-space-instances-mapped ()
           (when (and invoking-fn-name *warn-about-unusual-requests*)
             (warn "No space instances were specified to ~s."
                   invoking-fn-name))))
    (cond 
     ((and (consp pattern)
           (typep (car pattern) 'standard-space-instance))
      (cond 
       ;; pattern is entirely a list of space-instances:
       ((every #'(lambda (element)
                   (typep element 'standard-space-instance))
               (cdr pattern))
        (mapc fn pattern)
        (unless pattern (no-space-instances-mapped)))
       ;; relative-path pattern:
       (t (map-space-instances fn (path-relative-match pattern)
                               invoking-fn-name))))
     ;; absolute-path pattern:
     (t (let ((found-a-space nil))
          (map-unit-classes
           #'(lambda (space-class plus-subclasses)
               (declare (ignore plus-subclasses))
               (maphash
                #'(lambda (key value)             
                    (when (path-match pattern key)
                      (setf found-a-space 't)
                      (funcall fn value)))
                (standard-unit-class.instance-hash-table space-class)))
           (load-time-value (find-class 'standard-space-instance)))
          (unless found-a-space (no-space-instances-mapped)))))))

;;; ---------------------------------------------------------------------------

(defmacro do-space-instances ((var pattern) &body body)
   ;;; Do-xxx variant of map-instances-of-class.
  `(map-space-instances #'(lambda (,var) ,@body) ,pattern))

;;; ---------------------------------------------------------------------------

(defun find-space-instances (pattern)
  (let ((result nil))
    (map-space-instances #'(lambda (value) (push value result)) 
                         pattern)
    result))

;;; ---------------------------------------------------------------------------

(defun traverse-space-instance-tree (fn &optional 
                                        (space-instance *root-space-instance*))
  ;;; Call `fn' on all space-instances in the subtree rooted by
  ;;; `space-instance' (the root, if not specified), in parent-first order
  ;;; (beginning with `space-instance')
  (labels ((do-node (space-instance)
             (funcall fn space-instance)
             (dolist (child (space-instance-children space-instance))
               (do-node child))))
    (do-node space-instance)))

;;; ===========================================================================
;;;   Add/remove unit instances to/from a space instance

(defmethod add-instance-to-space-instance 
    ((instance standard-unit-instance)
     (space-instance-path cons))
  (add-instance-to-space-instance-internal
   instance
   (find-space-instance-by-path space-instance-path ':with-error)))

;;; ---------------------------------------------------------------------------

(defmethod add-instance-to-space-instance 
    ((instance standard-unit-instance)
     (space-instance standard-space-instance))
  (add-instance-to-space-instance-internal instance space-instance))

;;; ---------------------------------------------------------------------------

(defun add-instance-to-space-instance-internal (instance space-instance)
  ;;; Enhancement note: Cache dimension-compatibility checks on
  ;;; space-instance after the first time an instance of a unit class
  ;;; is added.  Must be cleared if unit-class is redefined, however.
  (declare (inline class-of))
  (with-lock-held (*master-instance-lock*)
    (check-for-deleted-instance instance 'add-instance-to-space-instance)
    (check-for-deleted-instance
     space-instance 'add-instance-to-space-instance)
    (cond 
     ;; no-op if instance is already present on the space-instance:
     ((memq space-instance 
            (standard-unit-instance.%%space-instances%% instance))
      (when *warn-about-unusual-requests* 
        (warn "In ~s: ~s is already on space instance ~s."
              'add-instance-to-space-instance
              instance 
              space-instance)))
     (t
      ;; Is `instance' allowed on `space-instance'?
      (let ((allowed-unit-class-names (allowed-unit-classes space-instance)))
        (unless (or (eq allowed-unit-class-names 't)
                    (some #'(lambda (unit-class-name)
                              (extended-unit-type-p instance unit-class-name))
                          allowed-unit-class-names))        
          (error "Attempt to store instance ~s~_on the space instance ~s.~
                 ~_This space instance ~:[does not allow instance storage.~;~
                 can only hold instances of classes: ~:*~s~]"
                 instance
                 space-instance
                 allowed-unit-class-names)))
      ;; dimension compatability-checks (cache someday!):
      (let ((unit-class-dimensions 
             (unit-class-dimensions (class-of instance))))
        (when unit-class-dimensions
          (let ((space-instance-dimensions
                 (space-instance-dimensions space-instance))
                (dimension-in-common-p nil))
            (declare (type list space-instance-dimensions))
            (dolist (unit-class-dimension unit-class-dimensions) 
              (let ((matching-space-instance-dimension
                     (find (car unit-class-dimension) 
                           space-instance-dimensions
                           :key #'car :test #'eq)))
                (when matching-space-instance-dimension
                  (unless (eq (second unit-class-dimension)
                              (second matching-space-instance-dimension))
                    (error "Incompatible dimension types for dimension ~s.~%~
                            Unit-class dimension: ~s~%~
                            Space-instance dimension: ~s"
                           (car unit-class-dimension)
                           unit-class-dimension
                           matching-space-instance-dimension))
                  (setf dimension-in-common-p 't))))
            ;; any dimensions in common?  
            (unless dimension-in-common-p
              (when (and *warn-about-unusual-requests*
                         (or space-instance-dimensions
                             unit-class-dimensions))
                (warn "In ~s: ~s does not share any dimensions with space ~
                       instance ~s."
                      'add-instance-to-space-instance
                      instance 
                      space-instance))))))
      ;; do the add:
      (dolist (storage (storage-objects-for-add/move/remove
                        (class-of instance) space-instance))
        (add-instance-to-storage instance storage nil))
      (push space-instance 
            (standard-unit-instance.%%space-instances%% instance))
      (signal-event-using-class
       (load-time-value (find-class 'add-instance-to-space-instance-event))
       :instance instance
       :space-instance space-instance)
      (dolist (bb-widget 
                  (standard-space-instance.%%bb-widgets%% space-instance))
        (draw-instance-on-bb-widget instance bb-widget)))))
  instance)

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-space-instance 
    ((instance standard-unit-instance)
     (space-instance-path cons))
  (remove-instance-from-space-instance-internal
   instance
   (find-space-instance-by-path space-instance-path ':with-error)))

;;; ---------------------------------------------------------------------------

(defmethod remove-instance-from-space-instance
    ((instance standard-unit-instance)
     (space-instance standard-space-instance))
  (remove-instance-from-space-instance-internal instance space-instance))

;;; ---------------------------------------------------------------------------

(defun remove-instance-from-space-instance-internal (instance space-instance)
  (declare (inline class-of))
  (with-lock-held (*master-instance-lock*)
    (check-for-deleted-instance 
     instance 'remove-instance-from-space-instance)
    (check-for-deleted-instance 
     space-instance 'remove-instance-from-space-instance)
    (let ((instance-found nil))
      (setf (standard-unit-instance.%%space-instances%% instance)
            (delete space-instance 
                    (standard-unit-instance.%%space-instances%% instance)
                    :test #'(lambda (a b) 
                              (when (eq a b) (setf instance-found 't)))))
      (cond
       ;; no-op if instance is not present on the space-instance:
       ((not instance-found)
        (when *warn-about-unusual-requests* 
          (warn "In ~s: ~s is not on space instance ~s."
                'remove-instance-from-space-instance
                instance 
                space-instance)))
       (t (dolist (storage (storage-objects-for-add/move/remove
                            (class-of instance) space-instance))
            (remove-instance-from-storage instance storage nil nil nil))
          (signal-event-using-class
           (load-time-value
            (find-class 'remove-instance-from-space-instance-event))
           :instance instance
           :space-instance space-instance)))))
  instance)
 
;;; ---------------------------------------------------------------------------

(defun setup-space-instance-evfns (instance)
  ;; We are "re-running" add/remove-event-functions with the new space instance:
  (let ((*%%doing-path-event-functions%%* 't)) 
    (flet ((do-event-class (event-class plus-subevents)
             (declare (ignore plus-subevents))
             (dolist (entry (reverse 
                             (space-instance-event-class.path-event-functions
                              event-class)))
               (destructuring-bind (path-pattern add/remove-fn-name event-class 
                                    &rest args)
                   entry
                 (declare (dynamic-extent args))
                 (when (path-match path-pattern (instance-name-of instance))
                   (apply (the function (symbol-function add/remove-fn-name))
                          event-class
                          args))))))
      (map-event-classes #'do-event-class 
                         (find-class 'space-instance-event)))))

;;; ===========================================================================
;;;  Describers

(defmethod describe-space-instance ((space-instance standard-space-instance))
  (let ((dimensions (space-instance-dimensions space-instance))
        (allowed-unit-classes (allowed-unit-classes space-instance)))
    (format t "~&~@(~s~) ~s~
               ~%  Allowed unit classes:~
                     ~:[~:[ None~;~:*~{~%~4t~s~}~]~; ~s~]~
               ~%  Dimensions:~:[ None~;~:*~{~%    ~s~}~]~%"
            (class-name (class-of space-instance))
            space-instance
            (eq allowed-unit-classes 't)
            allowed-unit-classes
            dimensions))
  (values))

(defmethod describe-space-instance ((space-instance cons))
  (describe-space-instance 
   (find-space-instance-by-path space-instance ':with-error)))

;;; ---------------------------------------------------------------------------

(defun describe-blackboard-repository ()
  ;;; Print a description of the blackboard repository to *standard-output*
  (let* ((2nd-column-indent 30)
         (root-space-instance *root-space-instance*)
         (top-level-space-instances 
          (and root-space-instance
               (space-instance-children root-space-instance))))
    (labels 
        ((do-instances (list indent)
           (dolist (instance (sort (copy-list list) #'string<
                                   :key #'standard-space-instance.space-name))
             (format t "~%~v@t~s~vt" 
                     indent (standard-space-instance.space-name instance) 
                     2nd-column-indent)
             (print-space-instance-storage-summary instance)
             (do-instances (space-instance-children instance) 
               (+& 3 indent)))))
      (when top-level-space-instances
        (format t "~2&Space Instance ~vtContents~
                    ~%-------------- ~:*~vt--------" 2nd-column-indent)
        (do-instances top-level-space-instances 0)))
    ;; Now summarize the unit instances:
    (let ((header-displayed? nil))
      (map-extended-unit-classes-sorted
       #'(lambda (unit-class plus-subclasses)
           (declare (ignore plus-subclasses))
           ;; Don't show root-space-instance in this summary:
           (unless (eq (class-name unit-class) 'root-space-instance)
             (let ((count (class-instances-count unit-class)))
               (when (plusp& count)
                 (unless header-displayed?
                   (setf header-displayed? 't)
                   (unless top-level-space-instances
                     (format t "~&There are no space instances in the ~
                                  blackboard repository.~%"))
                   (format t "~2&Unit Class~vtInstances~
                              ~%----------~:*~vt---------~%"
                           2nd-column-indent))
                 (format t "~s~vt~9d~%" 
                         (class-name unit-class)
                         2nd-column-indent
                         count)))))
       't)
      (unless header-displayed?
        (format t "~&There are no space or unit instances in the ~
                     blackboard repository.~%"))))
  (fresh-line)
  (values))

;;; ---------------------------------------------------------------------------

(defun show-forward-path-event-functions
    (&optional (event-class-name
                'add-instance-to-space-instance-event))
  (dolist (entry (reverse 
                  (space-instance-event-class.path-event-functions
                   (find-class event-class-name))))
    (pprint entry)))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================

