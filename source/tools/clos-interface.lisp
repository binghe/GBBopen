;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/clos-interface.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu May 15 18:03:33 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     CLOS Uniform-Access Interface 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-15-08 Split from tools.lisp.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

;;; ---------------------------------------------------------------------------
;;;  Create CLOS package nickname (or package!) where needed

#+(or clozure digitool-mcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':clos)
    (make-package ':clos
                  :use '(:common-lisp)))
  
  (defparameter *clos-symbols*
      '(ccl:accessor-method-slot-definition
        ccl:add-dependent
        ccl:add-direct-method
        ccl:add-direct-subclass 
        ccl:class-default-initargs
        ccl:class-direct-default-initargs 
        ccl:class-direct-slots 
        ccl:class-direct-subclasses
        ccl:class-direct-superclasses
        ccl:class-finalized-p 
        ccl:class-precedence-list
        ccl:class-prototype 
        ccl:class-slots
        ccl:compute-applicable-methods-using-classes
        ccl:compute-class-precedence-list
        ccl:compute-default-initargs
        ccl:compute-discriminating-function 
        ccl:compute-effective-method
        ccl:compute-effective-slot-definition
        ccl:compute-slots
        ;; Not exported in Digitool MCL:
        ccl::direct-slot-definition 
        ccl:direct-slot-definition-class
        ;; Not exported in Digitool MCL:
        ccl::effective-slot-definition
        ccl:effective-slot-definition-class
        ccl:ensure-class 
        ccl:ensure-class-using-class
        ccl:ensure-generic-function-using-class
        ccl::eql-specializer
        ccl:eql-specializer-object
        ccl:extract-lambda-list
        ccl:extract-specializer-names
        ccl:finalize-inheritance
        ccl:find-method-combination
        ccl:forward-referenced-class
        ccl:funcallable-standard-class 
        ccl:funcallable-standard-instance-access
        ccl::funcallable-standard-object
        ccl:generic-function-argument-precedence-order
        ccl:generic-function-declarations
        ccl:generic-function-lambda-list
        ccl:generic-function-method-class
        ccl:generic-function-method-combination
        ccl:generic-function-methods
        ccl:generic-function-name
        ccl:intern-eql-specializer
        ccl:make-method-lambda
        ccl:map-dependents
        ccl:metaobject 
        ccl:method-function
        ccl:method-generic-function
        ccl:method-lambda-list
        ccl:method-specializers 
        ccl:reader-method-class
        ccl:remove-dependent
        ccl:remove-direct-method
        ccl:remove-direct-subclass
        ccl:set-funcallable-instance-function
        ccl:slot-boundp-using-class
        ;; Not exported in Digitool MCL:
        ccl::slot-definition
        ccl:slot-definition-allocation
        ccl:slot-definition-initargs
        ccl:slot-definition-initform
        ccl:slot-definition-initfunction
        ccl:slot-definition-location
        ccl:slot-definition-name
        ccl:slot-definition-readers
        ccl:slot-definition-type
        ccl:slot-definition-writers
        ccl:slot-makunbound-using-class 
        ccl:slot-value-using-class
        ccl:specializer
        ccl:specializer-direct-generic-functions
        ccl:specializer-direct-methods
        ccl:standard-accessor-method
        ccl:standard-direct-slot-definition
        ccl:standard-effective-slot-definition
        ccl:standard-instance-access
        ccl:standard-reader-method
        ;; Not exported in Digitool MCL:
        ccl::standard-slot-definition
        ccl:standard-writer-method
        ccl:update-dependent
        ccl:validate-superclass
        ccl:writer-method-class)))

#+(or clozure digitool-mcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import *clos-symbols* :clos)
  (export *clos-symbols* :clos))
  
#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext::without-package-locks
   (add-package-nickname "CLOS" :pcl)))
  
#+cormanlisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-nickname "CLOS" :common-lisp))

#+gcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-package-nickname "CLOS" :pcl))

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

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


