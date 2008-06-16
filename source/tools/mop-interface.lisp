;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/mop-interface.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Jun 15 06:53:12 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

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
;;;  06-15-05 Consolidated interface.  (Corkill)
;;;  09-22-06 Updated for CormanLisp 3.0, but MOP is still incomplete.
;;;           (Corkill)
;;;  01-15-08 Moved into :gbbopen-tools.  (Corkill)
;;;  06-15-08 Removed non-AMOP CLASS-DIRECT-METHODS in favor of
;;;           SPECIALIZER-DIRECT-METHODS (thanks to Bruno Haible).  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *mop-symbols*
      '(#-cormanlisp
	clos:accessor-method-slot-definition
	clos:class-direct-slots
	clos:class-direct-subclasses
	clos:class-direct-superclasses
        clos:class-finalized-p
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
        clos:specializer-direct-methods ; ECL has the symbol, but no function
	#-cormanlisp
        clos:specializer-direct-generic-functions ; ECL has the symbol, but no
                                                  ; function
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export *mop-symbols*))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
