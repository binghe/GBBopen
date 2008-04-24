;;;; -*- Mode:Common-Lisp; Package:CLOS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/ecl-mop-patches.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Apr 24 09:57:36 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                   Required MOP Patches for ECL 0.9j
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Part of the GBBopen Project (see LICENSE for GBBopen license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  02-06-06 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (si:package-lock :common-lisp nil))

;;; ===========================================================================
;;;  Add a quick and dirty accessor-method-slot-definition and 
;;;  standard-accessor-method class and reader/writer subclasses:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass standard-accessor-method (standard-method)
    ((slot-name :initform nil :initarg :slot-name
		:reader accessor-method-slot-name)
     (slot-definition :initarg :slot-definition
		      :initform nil 
		      :reader accessor-method-slot-definition)))
  (defclass standard-reader-method (standard-accessor-method) ())
  (defclass standard-writer-method (standard-accessor-method) ()))

(defmethod accessor-method-class ((method standard-accessor-method))
  (first (slot-value method 'specializers)))

(defmethod accessor-method-class ((method standard-writer-method))
  (second (slot-value method 'specializers)))

;;; ---------------------------------------------------------------------------
;;;  Initialize the accessor-method-slot-definition value:

(defmethod shared-initialize :after ((method standard-accessor-method)
                                     slot-names
                                     &key)
  (declare (ignore slot-names))
  (with-slots (slot-name slot-definition)
      method
    (unless slot-definition
      (let ((class (accessor-method-class method)))
	(setf slot-definition (find slot-name (class-direct-slots class)
				    :key #'slot-definition-name))))))

;;; ---------------------------------------------------------------------------
;;;  Adapted from make-method (src/clos/fixup.lsp):

(defun make-accessor-method (qualifiers specializers arglist function plist
			     options gfun method-class slot-name)
  (declare (ignore gfun options))
  (make-instance method-class
		 :generic-function nil
		 :qualifiers qualifiers
		 :lambda-list arglist
		 :specializers specializers
		 :function function
		 :plist plist
		 ;; Added slot-name (Corkill):
		 :slot-name slot-name
		 :allow-other-keys t))

;;; ---------------------------------------------------------------------------
;;;  Adapted from install-method (src/clos/kernel.lsp):

(defun install-accessor-method (name qualifiers specializers lambda-list doc plist
                                fun
                                ;; *****************************************
                                ;; Extra arguments added by Corkill:
				reader-method-class slot-name
                                ;; *****************************************
                                &rest options)
  (declare (ignore doc)
	   (notinline ensure-generic-function))
;  (record-definition 'method `(method ,name ,@qualifiers ,specializers))
  (let* ((gf (ensure-generic-function name))
	 (specializers (mapcar #'(lambda (x)
				   (cond ((null x) x)
					 ((consp x) x)
					 ((si::instancep x) x)
					 (t (find-class x))))
			       specializers))
	 (method (make-accessor-method 
		  qualifiers specializers lambda-list
		  fun plist options gf
                  ;; *********************************************************
                  ;; Changed the following arguments:
                  #+original
                  (generic-function-method-class gf)
                  #-original
                  reader-method-class 
                  #-original slot-name
                  ;; *********************************************************
                  )))
    (add-method gf method)
    method))

;;; ---------------------------------------------------------------------------
;;;  Changed to call new install-accessor-method in place of install-method
;;;  (from src/clos/standard.lsp):

(defun std-class-generate-accessors (standard-class)
  (declare (si::c-local))
  ;;
  ;; The accessors are closures, which are generated every time the
  ;; slots of the class change. The accessors are safe: they check that
  ;; the slot is bound after retreiving the value, and they may take
  ;; the liberty of using SI:INSTANCE-REF because they know the class of
  ;; the instance.
  ;;
  (dolist (slotd (class-slots standard-class))
    (let* ((slot-name (slot-definition-name slotd))
	   (index (slot-definition-location slotd))
	   reader setter)
      (declare (fixnum index))
      (if (and (eql (slot-definition-allocation slotd) :instance)
	       (si:fixnump index)
	       (slot-value standard-class 'optimize-slot-access))
	  (setf reader #'(lambda (self)
			   (let ((value (si:instance-ref self index)))
			     (if (si:sl-boundp value)
				 value
				 (values (slot-unbound (class-of self) self slot-name)))))
		setter #'(lambda (value self)
			   (si:instance-set self index value)))
	  (let ((slotd slotd))
	    ;; Note that in order to save this value in the closure we have to copy
	    ;; the variable, because the value of SLOTD is going to change!
	    (setf reader #'(lambda (self)
			     (slot-value-using-class (si:instance-class self)
						     self slotd))
		  setter #'(lambda (value self)
			     (setf (slot-value-using-class (si:instance-class self)
							   self slotd) value)))))
      ;; **********************************************************************
      ;; Changed the following to call install-accessor-method (Corkill):
      #+original
      (dolist (fname (slot-definition-readers slotd))
	(install-method fname nil `(,standard-class) '(self) nil nil
			reader))
      #-original
      (dolist (fname (slot-definition-readers slotd))
	(install-accessor-method
	 fname nil `(,standard-class) '(self) nil nil reader 	
	 'standard-reader-method slot-name))
      ;; **********************************************************************
      (dolist (fname (slot-definition-writers slotd))
	(install-method fname nil `(,(find-class t) ,standard-class) '(value self)
			nil nil setter)))))

;;; ---------------------------------------------------------------------------
;;; Needed only to be called from finalize-inheritance.  Copied verbatim from
;;; src/clos/standard.lsp:

(defun std-create-slots-table (class)
  (let* ((all-slots (class-slots class))
	 (table (make-hash-table :size (max 32 (length all-slots)))))
    (dolist (slotd (class-slots class))
      (setf (gethash (slot-definition-name slotd) table) slotd))
    (setf (slot-table class) table)))

;;; ---------------------------------------------------------------------------
;;;  Needed only to call the above fix, copied verbatim from
;;;  src/clos/standard.lsp:

(defmethod finalize-inheritance ((class standard-class))
  (call-next-method)
  (std-create-slots-table class)
  (std-class-generate-accessors class))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (si:package-lock :common-lisp 't))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
