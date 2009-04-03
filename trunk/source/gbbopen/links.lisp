;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/links.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Apr  2 18:07:27 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                            Link Functions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-16-02 File created.  (Corkill)
;;;  03-02-04 Ensure that LINKF! updates the link slot when no new links are 
;;;           added.  (Corkill)
;;;  03-09-04 Fix incorrect delete-all incoming pointers code.  (Corkill)
;;;  07-08-04 Fix CHECK-LINK-DEFINITIONS singular-link comparison.  (Corkill)
;;;  08-16-04 Allow %DO-IUNLINKS to delete links even when the link
;;;           singularity has been redefined.  (Corkill)
;;;  12-04-04 Fix to handle some link/non-link shadowing (not a general
;;;           solution, however).  (Corkill)
;;;  12-10-04 Updated for Lispworks 4.4. (Corkill)
;;;  01-06-05 Fix FIND-CLASS failure in set-inverse-link-definition for
;;;           inverse links in the class that is being defined (thanks to 
;;;           Earl Wagner for finding this problem!).  (Corkill)
;;;  03-23-05 Fix incorrect mapping class in CHECK-LINK-DEFINITIONS.  (Corkill)
;;;  07-30-05 Rename LINKF! to LINK-SETF.  (Corkill)
;;;  03-11-06 Add ECL support.  (Corkill)
;;;  06-24-06 Finally rewrote get-dlslotd-from-reader to handle object-specific
;;;           lookups.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(check-link-definitions
	    link-setf
	    linkf
	    unlinkf
	    unlinkf-all)))

;;; ===========================================================================
;;;
;;;  Important Note:
;;;
;;;  In GBBopen, link-slot pointer lists are destructively modified by
;;;  linkf/unlinkf operations for top performance.
;;;
;;; ---------------------------------------------------------------------------

(defun map-direct-link-slots (fn class)
  (declare (type function fn))
  (dolist (slot (class-direct-slots class))
    (when (typep slot 'direct-link-definition)
      (funcall fn slot))))

;;; ---------------------------------------------------------------------------
;;; Set all inverse-link-definition information for each direct link slot in
;;; the unit class.  This is done in initialize-instance &
;;; reinitialize-instance :after methods (rather than a shared-initialize
;;; :after method) to give ECL a chance to create-the direct-slot-definitions.

(defun set-all-inverse-link-definitions (class)
  (flet ((do-a-link (slot) 
	   (set-inverse-link-definition class slot)))
    (declare (dynamic-extent #'do-a-link))
    (map-direct-link-slots #'do-a-link class)))

(defmethod initialize-instance :after ((class standard-unit-class) 
				       &key)
  (set-all-inverse-link-definitions class))

(defmethod reinitialize-instance :after ((class standard-unit-class) 
					 &key)
  (set-all-inverse-link-definitions class))

;;; ===========================================================================
;;;   Link-slot setf sentinel

(defvar *%%allow-setf-on-link%%* nil)

;;; Catch unauthorized setf's of link slots.  This doesn't work on Lisps that
;;; optimize defclass link-writer methods rather than calling the (setf
;;; slot-value-using-class) method.  With those Lisps, we could attach to all
;;; the writer methods, but we haven't tried doing that yet.
;;;
;;; Note that Lispworks uses the :optimize-slot-access class option to control
;;; the use of slot reader/writer methods.

(defmethod (setf slot-value-using-class) 
    :before (nv
             (class standard-unit-class)
             instance
             ;; instead of the effective-slot-definition, Lispworks
             ;; provides the slot name:
             (link-slot #+lispworks symbol
                        #-lispworks effective-link-definition))
  (declare (ignore nv))
  (unless (or *%%allow-setf-on-link%%*
              ;; determine if the slot is a link slot (Lispworks):
              #+lispworks
              (let ((link-slot (find link-slot (class-slots class)
                                     :test #'eq
                                     :key 'slot-definition-name)))
                (not (typep link-slot 'effective-link-definition))))
    (error "~s attempted on a link slot ~s of unit class ~s"
           'setf 
           #+(or ecl lispworks)
           link-slot
           #-(or ecl lispworks)
           (slot-definition-name link-slot)
           (type-of instance))))

;;; ---------------------------------------------------------------------------

(defun set-inverse-link-definition (class dlinkd)
  (let* ((inverse-link (direct-link-definition.inverse-link dlinkd))
         (ilinkd
          (if (eq inverse-link ':reflexive)
              dlinkd
	      (let* ((other-unit-class-name (first inverse-link))
		     (other-unit-class 
		      ;; find-class does not find the class currently
		      ;; being defined in some CLs (thanks to Earl Wagner
		      ;; for catching this!):
		      (if (eq other-unit-class-name (class-name class))
			  class
			  (find-class other-unit-class-name nil))))
		(when other-unit-class
		  (find (second inverse-link)
			(the list (class-direct-slots other-unit-class))
			:test #'eq
			:key #'slot-definition-name))))))
    (when ilinkd
      ;; when possible, set the inverse-link's inverse-link-definition:
      (unless (eq inverse-link ':reflexive)
	(let ((idlinkd (direct-link-definition.inverse-link ilinkd)))
	  (when (and (eq (class-name class)
			 (first idlinkd))
		     (eq (slot-definition-name dlinkd)
			 (second idlinkd)))
	    (setf (direct-link-definition.inverse-link-definition ilinkd)
		  dlinkd))))
      ;; set the inverse-link-definition (set to nil if inverse is forward 
      ;; referenced):
      (setf (direct-link-definition.inverse-link-definition dlinkd)
	    ilinkd))))
                
;;; ===========================================================================
;;;   Link/unlink support functions

(defun build-link-macro-code (fn event-fn place env 
                              &optional 
                              (other-instances nil other-instances-p)
                              (forced-unlink-event-fn nil forced))
  ;;; Generates expansion code for all link macros (linkf & friends)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    `(let* (,.(mapcar #'list vars vals)
            ,@store-vars
            (.new. ,other-instances) 
            .change.
	    .dslotd.
            ,@(when forced '(.forced-removal.
                             .forced-unlinked.)))
       (multiple-value-setq (,(first store-vars) 
                             .change.
                             ,@(when forced '(.forced-removal. .forced-unlinked.)))
         (,fn 
	  ;; determine the direct-link-definition:
	  (setf .dslotd.
	    ,(if (and (consp reader-form)
		      (eq (first reader-form) 'slot-value))
		 `(get-slot-value-dlslotd 
		   ,(second reader-form) 
		   ,(third reader-form))
		 `(get-dlslotd-from-reader 
		   ',(first reader-form)
		   ,(first vars))))
	  ,(first vars) ,reader-form
	  ,@(when other-instances-p '(.new.))
	  ,@(when forced `(',forced-unlink-event-fn))))
       (when (or .change. ,@(when forced '(.forced-removal.)))
         (let ((*%%allow-setf-on-link%%* t))
           ,writer-form)
         ,@(when forced
             `((when .forced-unlinked.
                 (,forced-unlink-event-fn ,(second reader-form)
                                          .dslotd.
                                          ,(first store-vars)
                                          .forced-unlinked.))))
         (when ,@(if forced '(.change.) '(t))
               (,event-fn ,(second reader-form)
                          .dslotd.
                          ,(first store-vars)
                          .change.)))
       .new.)))

;;; ---------------------------------------------------------------------------

(defun non-empty-singular-link-cerror (dslotd instance existing-value)
  (cerror "Unlink instance ~1@*~s."
          "Singular link ~s in instance ~s is not empty. ~
           It contains ~s."
          (slot-definition-name dslotd)
          instance 
          existing-value))

;;; ---------------------------------------------------------------------------

(defun %do-ilinks (dslotd instance other-instances force)
  ;;; Set the inverse pointers to `instance' in each other-instance, if it is
  ;;; not present already
  (let* ((idslotd (direct-link-definition.inverse-link-definition dslotd))
         (singular (direct-link-definition.singular idslotd))
         (slot-name (slot-definition-name idslotd))
         (*%%allow-setf-on-link%%* t)
	 (added-instances (list instance)))
    (dolist (other-instance other-instances)
      (let ((pointer-added? nil)
            new-value)
        (cond
         ;; ilink is singular:
         (singular 
          (let ((existing (slot-value other-instance slot-name)))
            ;; Do nothing, if the inverse pointer is present already:
            (unless (eq instance existing)
              ;; Handle another inverse pointer present:
              (when existing 
                (unless force
                  (non-empty-singular-link-cerror idslotd other-instance existing))
                ;; unlink the existing value
                (%do-iunlink idslotd other-instance existing))
              ;; Note the addition and set the value:
              (setf pointer-added? 't)
              (setf new-value instance) 
              (setf (slot-value other-instance slot-name) instance))))
         ;; multi-link ilink:
         (t (let* ((sort-function (direct-link-definition.sort-function idslotd))
                   (sort-key (direct-link-definition.sort-key idslotd))
                   (slot-value (slot-value other-instance slot-name)))
              ;; Do nothing, if the inverse pointer is present already:
              (unless (memq instance slot-value)
                ;; Note the addition and set the value:
                (setf pointer-added? 't)
                (setf new-value
                      (if sort-function
                          (nsorted-insert instance slot-value
                                          sort-function sort-key)
                          (cons instance slot-value)))
                (setf (slot-value other-instance slot-name) new-value)))))
        ;; signal the indirect link event, if the inverse pointer was added:
        (when pointer-added?
          (%signal-indirect-link-event 
           other-instance idslotd new-value added-instances))))))

;;; ---------------------------------------------------------------------------

(defun %do-iunlinks (dslotd instance other-instances)
  ;;; Remove the inverse pointers to `instance' in each other-instance
  (let* ((idslotd (direct-link-definition.inverse-link-definition dslotd))
         (slot-name (slot-definition-name idslotd))
         (*%%allow-setf-on-link%%* t)
         (removed-instances (list instance)))
    (dolist (other-instance other-instances)
      (let* ((previous-value (slot-value other-instance slot-name))
	     (current-value
	      (if (consp previous-value)
		  (setf (slot-value other-instance slot-name) 
			(delq instance previous-value))
		  (setf (slot-value other-instance slot-name) nil))))
        (%signal-indirect-unlink-event 
         other-instance idslotd current-value removed-instances)))))

;;; ---------------------------------------------------------------------------

(defun %do-iunlink (dslotd instance other-instance)
  ;;; Single value version of %do-iunlinks (above) --
  ;;; removes the inverse pointer to `instance' in `other-instance'
  (let* ((idslotd (direct-link-definition.inverse-link-definition dslotd))
         (slot-name (slot-definition-name idslotd))
         (*%%allow-setf-on-link%%* t)
         (removed-instances (list instance)))
    (let* ((previous-value (slot-value other-instance slot-name))
           (current-value
            (if (consp previous-value)
                (setf (slot-value other-instance slot-name) 
                      (delq instance previous-value))
                (setf (slot-value other-instance slot-name) nil))))
      (%signal-indirect-unlink-event
       other-instance idslotd current-value removed-instances))))

;;; ---------------------------------------------------------------------------

(defun %do-linkf (dslotd instance existing new &optional force)
  ;;; Determine the new links to add to `instance' and perform setting of
  ;;; inverse pointers.  Ensure that a link is never added more than once, no
  ;;; matter how many times it appears in `new'.  Link-setf behavior is
  ;;; specified via `force'.
  ;;;
  ;;; Return the new link value, list of added instances, an indicator of
  ;;; forced removal, and the list of forced unlinked instances.
  (let ((forced-removal nil)
        (forced-unlinked-instances nil)
        (operation (if force 'link-setf 'linkf)))
    (check-for-deleted-instance instance operation)
    (cond
     ;; nothing new to add:
     ((and (null new) (not force))
      (values existing nil nil))
     ;; singular link
     ((direct-link-definition.singular dslotd)
      ;; ensure atomic new value:
      (when (consp new)
        (setf new (sole-element new)))
      (when new 
        (check-for-deleted-instance new operation))
      (cond
       ;; no-op if already present:
       ((eq existing new) (values existing nil nil))
       (t 
        ;; non-empty existing value:
        (when existing 
          (unless force
            (non-empty-singular-link-cerror dslotd instance existing))
          ;; unlink the existing value
          (%do-iunlink dslotd instance existing)
          (push existing forced-unlinked-instances)
          (setf forced-removal t))
        (let ((change (ensure-list new)))
          (%do-ilinks dslotd instance change force)
          ;; return the result values:
          (values new change forced-removal forced-unlinked-instances)))))
     ;; multi-link:
     (t (let ((change nil)
              (sort-function (direct-link-definition.sort-function dslotd))
              (sort-key (or (direct-link-definition.sort-key dslotd)
                            #'identity)))
          ;; ensure new is a list:
          (unless (listp new) (setf new (list new)))
          ;; unlink any extra links:
          (when force
            (dolist (existing-instance existing)
              (unless (memq existing-instance new)
                (setf existing (delq existing-instance existing))
                (%do-iunlink dslotd instance existing-instance)
                (push existing-instance forced-unlinked-instances)
                (setf forced-removal t))))
          ;; add in new links:
          (dolist (new-instance new)
            (unless (memq new-instance existing)
              (check-for-deleted-instance new-instance operation)
              (if sort-function
                  (setf existing
                    (nsorted-insert new-instance existing sort-function sort-key))
                  (push new-instance existing))
              (push new-instance change)))
          (%do-ilinks dslotd instance change force)
          (values 
           ;; Use new to as the value for link-setf (unless a sort-function
           ;; was used); otherwise the updated existing value:
           (if (and force (not sort-function)) 
               (delete-duplicates new :test 'eq)
               existing)
           change
           forced-removal
           forced-unlinked-instances))))))

;;; ---------------------------------------------------------------------------

(defun %do-unlinkf (dslotd instance existing remove)
  ;;; Unlink the specified instances from `dslotd' in `instance' and
  ;;; unlink the inverse pointers.
  ;;;
  ;;; Return the new link value and list of removed instances.
  (check-for-deleted-instance instance 'unlinkf)
  (cond
   ;; nothing to remove
   ((null remove) (values existing nil))
   ;; singular link
   ((direct-link-definition.singular dslotd)
    (cond ((or (eq existing remove)
               (and (consp remove)
                    (memq existing remove)))
           (check-for-deleted-instance existing 'unlinkf)
           ;; unlink the inverse pointer
           (%do-iunlink dslotd instance existing)
           ;; return the new link value and changes
           (values nil (list existing)))
          (t (values existing nil))))
   ;; multi-link
   (t (let ((change nil))
        ;; ensure remove is a list
        (unless (listp remove) (setf remove (list remove)))
        (flet ((when-eq-push (a b)
                 (when (eq a b)
                   (push a change))))
          (declare (dynamic-extent #'when-eq-push))
          (dolist (rinstance remove)
            (check-for-deleted-instance rinstance 'unlinkf)
            (setf existing (delete rinstance existing :test #'when-eq-push))))
        (when change
          ;; unlink the inverses
          (%do-iunlinks dslotd instance change))
        ;; return the new link value and changes
        (values existing change)))))
  
;;; ---------------------------------------------------------------------------

(defun %do-unlinkf-all (dslotd instance existing)
  ;;; Unlink all links from `dslotd' in `instance' and unlink the inverse 
  ;;; pointers.
  ;;;
  ;;; Return the new link value (nil) and list of removed instances.
  (check-for-deleted-instance instance 'unlinkf-all)
  (cond
   ;; singular link:
   ((direct-link-definition.singular dslotd)
    (when existing
      ;; unlink the inverse pointer of the existing value:
      (%do-iunlink dslotd instance existing)
      ;; return the new link value and changes:
      (values nil (list existing))))
   ;; multi-link:
   (t
    ;; unlink the inverse pointers:
    (%do-iunlinks dslotd instance existing)
    ;; return the new link value and changes:
    (values nil existing))))
  
;;; ---------------------------------------------------------------------------

;;; This approach is temporary and nasty!  Need to do a much better job of
;;; accessing the effective & definition objects directly within the accessor
;;; code...

(defun find-eslotd-given-dslotd (instance dslotd)
  (find (slot-definition-name dslotd) 
	(the list (class-slots (class-of instance)))
	:key #'slot-definition-name
	:test #'eq))

;;; ---------------------------------------------------------------------------

(defun %signal-direct-link-event (instance dslotd current-value 
				  added-instances)
  (signal-event-using-class (load-time-value (find-class 'link-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :added-instances added-instances
                            :directp 't
                            :initialization *%%doing-initialize-instance%%*))

;;; ---------------------------------------------------------------------------

(defun %signal-indirect-link-event (instance dslotd current-value
				    added-instances)
  (signal-event-using-class (load-time-value (find-class 'link-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :added-instances added-instances
                            :directp nil
                            :initialization *%%doing-initialize-instance%%*))

;;; ---------------------------------------------------------------------------

(defun %signal-direct-unlink-event (instance dslotd current-value
				    removed-instances)
  (signal-event-using-class (load-time-value (find-class 'unlink-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :removed-instances removed-instances
                            :directp 't))

;;; ---------------------------------------------------------------------------

(defun %signal-indirect-unlink-event (instance dslotd current-value
				      removed-instances)
  (signal-event-using-class (load-time-value (find-class 'unlink-event))
                            :instance instance 
                            :slot (find-eslotd-given-dslotd instance dslotd)
                            :current-value current-value
                            :removed-instances removed-instances
                            :directp nil))

;;; ---------------------------------------------------------------------------

(defun delete-incoming-link-pointer (instance link-slot)
  ;;; Internal function to delete the incoming pointers to `instance'
  ;;; associated with `link-slot'
  (declare (inline class-of))
  (let ((dslotd (effective-link-definition.direct-slot-definition link-slot))
	(inverse-instances
	 (slot-value-using-class (class-of instance) instance link-slot)))
    (when inverse-instances
      (if (direct-link-definition.singular dslotd)
          (%do-iunlink dslotd instance inverse-instances)
          (%do-iunlinks dslotd instance inverse-instances)))))

;;; ---------------------------------------------------------------------------

(defun delete-all-incoming-link-pointers (instance) 
  ;;; Internal function to delete all incoming pointers to `instance'
  (declare (inline class-of))
  (let ((unit-class (class-of instance)))
    (dolist (slot (class-slots unit-class))
      (when (typep slot 'effective-link-definition)
	(delete-incoming-link-pointer instance slot)))))

;;; ===========================================================================
;;;   Direct-link definition lookups
;;;
;;; The following two function peform direct-link-definition lookups on every
;;; link-slot operation, so they always reflect the latest definitions.
;;; Performance is generally good, but they are candidates for hashing and/or
;;; caching.

(defun get-slot-value-dlslotd (object slot-name)
  ;;; Return the direct-link definition for `slot-name' in `object' by looking
  ;;; for the closest (CPL) definition
  (declare (inline class-of))
  (some #'(lambda (class)
            (find-if #'(lambda (dslotd)
			 (when (and (eq slot-name 
					(slot-definition-name dslotd))
				    (typep dslotd 'direct-link-definition)) 
			   dslotd))
		     (the list (class-direct-slots class))))
        (class-precedence-list (class-of object))))
    
;;; ---------------------------------------------------------------------------

(defun get-dlslotd-from-reader (reader-method-name object)
  ;;; Return the direct-link definition associated with `reader-method-name'
  ;;; and `object'
    
  (let* ((reader-methods
          (compute-applicable-methods
           (symbol-function reader-method-name) (list object)))
         (reader-method
          #+ecl (first (last reader-methods))
          #-ecl (first reader-methods)))
    (or (when (typep reader-method 'standard-reader-method)
	  (let ((dslotd
		 (accessor-method-slot-definition reader-method)))
	    (when (typep dslotd 'direct-link-definition)
	      dslotd)))
	(error "Unable to determine the link slot of place ~s for ~s" 
	       reader-method-name
	       object))))
  
;;; ===========================================================================
;;;   Linkf & friends

(defmacro linkf (place other-instances &environment env)
  (build-link-macro-code '%do-linkf '%signal-direct-link-event
                         place env other-instances))

(defmacro link-setf (place other-instances &environment env)
  (build-link-macro-code '%do-linkf '%signal-direct-link-event 
                         place env other-instances
                         '%signal-direct-unlink-event ))

(defmacro unlinkf (place other-instances &environment env)
  (build-link-macro-code '%do-unlinkf '%signal-direct-unlink-event 
                         place env other-instances))

(defmacro unlinkf-all (place &environment env)
  (build-link-macro-code '%do-unlinkf-all '%signal-direct-unlink-event 
                         place env))

;;; ===========================================================================
;;;   Link consistency checking

(defun check-a-link (class linkd &optional silent)
  (let ((inverse-link (direct-link-definition.inverse-link linkd)))
    (cond 
     ;; reflexive links are OK:
     ((eq inverse-link ':reflexive) t)
     ;; compare the inverse-link specification in `linkd' with the actual
     ;; ilinkd values:
     (t (let* ((iunit-class-name (first inverse-link))
               (iunit-class (find-class iunit-class-name nil)))
          (cond 
           ;; missing class for inverse link:
           ((not iunit-class)
            (unless silent
              (warn "The inverse of link slot ~a in unit-class ~s refers to ~
                     unit class ~s, which is not defined."
                    (slot-definition-name linkd)
                    (class-name class)
                    iunit-class-name)))
           (t (let* ((islot-name (second inverse-link))
                     (ilinkd (find islot-name 
                                   (the list (class-direct-slots iunit-class))
                                   :test #'eq
                                   :key #'slot-definition-name))
                     (singular-inverse
                      (getf inverse-link :singular nil)))
                (cond
                 ;; missing inverse link slot:
                 ((or (not ilinkd)
                      (not (typep ilinkd 'direct-link-definition)))
                  (unless silent
                    (warn "The inverse of link slot ~a in unit-class ~s ~
                           refers to link slot ~s which is not present in ~
                           unit class ~s."
                          (slot-definition-name linkd)
                          (class-name class)
                          islot-name
                          iunit-class-name)))
                 ;; mismatched singularity:
                 ((xor singular-inverse
		       (direct-link-definition.singular ilinkd))
                  (unless silent
                    (warn "Link slot ~a in unit class ~s incorrectly ~
                           declares its inverse link slot ~s in unit class ~
                           ~s as ~:[not ~;~]singular."
                          (slot-definition-name linkd)
                          (class-name class)
                          (slot-definition-name ilinkd)
                          iunit-class-name
                          singular-inverse)))
                 (t t))))))))))

;;; ---------------------------------------------------------------------------

(defun check-link-definitions (&optional silent)
  (let ((result 't))
    (map-unit-classes
     #'(lambda (class plus-subclasses)
         (declare (ignore plus-subclasses))
         (map-direct-link-slots 
          #'(lambda (link) 
              (unless (check-a-link class link silent)
                (if silent 
                    (setf result nil)
                    (return-from check-link-definitions nil))))
	  (ensure-finalized-class class)))
     ;; Note: standard-unit-instance isn't defined until after this file is
     ;;       loaded, so we can't use load-time-value on this class "constant"
     (find-class 'standard-unit-instance))
    (when (and result (not silent))
      (format t "~&;; All link definitions are consistent.~%"))
    result))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

