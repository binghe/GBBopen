;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/tools/mop-interface.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Feb 13 04:21:40 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                         MOP Symbol Importing
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; Porting Notice:
;;;   [Description of MOP and implementation-specific porting details still
;;;    needed...]
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  04-20-04 Split out from units.lisp.  (Corkill)
;;;  12-04-04 Added missing standard-reader-method and reader-method-class
;;;           imports.  (Corkill)
;;;  12-10-04 Updated for Lispworks 4.4.  (Corkill)
;;;  06-15-05 Consolidated interface.  (Corkill)
;;;  09-22-06 Updated for CormanLisp 3.0, but MOP is still incomplete.
;;;           (Corkill)
;;;  01-15-08 Moved into :gbbopen-tools.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)
  
#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(clos::canonicalize-defclass-slot
	    clos:class-direct-methods
	    clos:process-a-slot-option)))

;;; Class-direct-methods is not AMOP.  Lispworks provides it, and we provide
;;; a portable one for convenience (and possible use in the future):
#-ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(class-direct-methods)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mop-symbols*
      '(#-cormanlisp
	clos:accessor-method-slot-definition
	clos:class-direct-slots
	clos:class-direct-subclasses
	clos:class-direct-superclasses
	clos:class-precedence-list
	clos:class-prototype
	clos:class-slots
	clos:compute-effective-slot-definition
	#-cormanlisp
	clos:direct-slot-definition-class
	#-cormanlisp
	clos:effective-slot-definition-class
	clos:finalize-inheritance
	clos:generic-function-method-class
	clos:generic-function-methods
        clos:generic-function-name
	#-(or cormanlisp ecl)
	clos:reader-method-class	; not currently used in GBBopen
	clos:slot-boundp-using-class
	clos:slot-definition
	clos:slot-definition-allocation
	clos:slot-definition-name
	clos:slot-definition-initargs
	clos:slot-definition-initform
	clos:slot-definition-initfunction
	clos:slot-definition-readers
	clos:slot-definition-writers
	clos:slot-value-using-class
	#-cormanlisp
	clos:standard-direct-slot-definition
	#-cormanlisp
	clos:standard-effective-slot-definition
	#-cormanlisp
	clos:standard-reader-method
	#-cormanlisp
	clos::standard-slot-definition
	#-cormanlisp
	clos:standard-writer-method
	#-(or cormanlisp ecl)
	clos:validate-superclass
	#-(or cormanlisp ecl)
	clos:writer-method-class	; not currently used in GBBopen
	)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import *mop-symbols*))

;;; ---------------------------------------------------------------------------
;;;  Note: class-direct-methods is not currently used by GBBopen, it is here
;;;  for convenience and possible use in the future...
;;;  
;;;  Lispworks provides class-direct-methods.
;;;
;;;  ECL 0.9i currently does not record direct-methods of a class, so we can't
;;;  implement class-direct-methods on ECL.

#-(or ecl lispworks)
(defun class-direct-methods (class)
  (let ((direct-methods 
	 (slot-value class 
		     #+allegro 'excl::direct-methods
		     #+clisp 'clos::$direct-methods
		     #-(or allegro clisp) 'clos::direct-methods)))
    ;; the direct-methods slot value is a list of the methods
    ;; list, except in Lispworks:
    #-clisp
    (when (consp (car direct-methods))
      (setq direct-methods (sole-element direct-methods)))
    ;; and in CLISP:
    #+clisp
    (let ((result nil))
      (maphash #'(lambda (key value)
		   (declare (ignore key))
		   value)
	       direct-methods)
      result)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export *mop-symbols*))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
