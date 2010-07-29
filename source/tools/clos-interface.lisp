;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/clos-interface.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul 29 18:04:00 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                   CLOS/MOP Uniform-Access Interface 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2002-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-15-08 Split from tools.lisp.  (Corkill)
;;;  06-17-08 Rewritten to eliminate separate mop-interface.lisp file; :clos 
;;;           package nicknaming eliminated.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

;;; ===========================================================================
;;;   Import into & export from the :gbbopen-tools package the MOP symbols

(defparameter *clos/mop-symbols*
    '(#:accessor-method-slot-definition ; not provided in ABCL
      #:add-dependent                   ; not provided in ABCL
      #:add-direct-method               ; not provided in ABCL
      #:add-direct-subclass             ; not provided in ABCL
      #:class-default-initargs
      #:class-direct-default-initargs 
      #:class-direct-slots
      #:class-direct-subclasses
      #:class-direct-superclasses
      #:class-finalized-p
      #:class-precedence-list
      #:class-prototype
      #:class-slots
      #:compute-applicable-methods-using-classes
      #:compute-class-precedence-list
      #:compute-default-initargs        ; not provided in ABCL
      #:compute-discriminating-function 
      #:compute-effective-method        ; not provided in ABCL
      #:compute-effective-slot-definition
      #:compute-slots
      #:direct-slot-definition 
      #:direct-slot-definition-class
      #:effective-slot-definition
      #:effective-slot-definition-class
      #:ensure-class
      #:ensure-class-using-class        ; not provided in ABCL
      #:ensure-generic-function-using-class ; not provided in ABCL
      #:eql-specializer
      #:eql-specializer-object
      #:extract-lambda-list
      #:extract-specializer-names       ; named EXTRACT-SPECIALIZERS in ABCL
      #:finalize-inheritance
      #:find-method-combination         ; not provided in ABCL
      #:forward-referenced-class
      #:funcallable-standard-class      ; not provided in ABCL
      #:funcallable-standard-instance-access ; not provided in ABCL
      #:funcallable-standard-object     ; not provided in ABCL
      #:generic-function-argument-precedence-order
      #:generic-function-declarations   ; not provided in ABCL
      #:generic-function-lambda-list
      #:generic-function-method-class
      #:generic-function-method-combination
      #:generic-function-methods
      #:generic-function-name
      #:intern-eql-specializer
      #:make-method-lambda              ; not provided in ABCL & CLISP
      #:map-dependents                  ; not provided in ABCL
      #:metaobject                      ; not provided in ABCL
      #:method-function
      #:method-generic-function         ; not provided in ABCL
      #:method-lambda-list
      #:method-specializers 
      #:reader-method-class             ; not provided in ABCL
      #:remove-dependent                ; not provided in ABCL
      #:remove-direct-method            ; not provided in ABCL
      #:remove-direct-subclass          ; not provided in ABCL
      #:set-funcallable-instance-function
      #:slot-boundp-using-class
      #:slot-definition
      #:slot-definition-allocation
      #:slot-definition-initargs
      #:slot-definition-initform
      #:slot-definition-initfunction
      #:slot-definition-location
      #:slot-definition-name
      #:slot-definition-readers
      #:slot-definition-type            ; not provided in ABCL
      #:slot-definition-writers
      #:slot-makunbound-using-class 
      #:slot-value-using-class
      #:specializer      
      #:specializer-direct-generic-functions ; not provided in ABCL -- ECL has
                                             ; the symbol, but no function
      #:specializer-direct-methods      ; not provided in ABCL -- ECL has
                                        ; the symbol, but no function
      #:standard-accessor-method        ; not provided in ABCL
      #:standard-direct-slot-definition ; not provided in ABCL
      #:standard-effective-slot-definition ; not provided in ABCL
      #:standard-instance-access
      #:standard-reader-method
      #:standard-slot-definition        ; not provided in ABCL
      #:standard-writer-method          ; not provided in ABCL
      #:update-dependent                ; not provided in ABCL
      #:validate-superclass             ; not provided in ABCL
      #:writer-method-class))           ; not provided in ABCL

;;; ---------------------------------------------------------------------------

;; Lispworks is missing intern-eql-specializer:
#+(or cormanlisp lispworks)
(defun intern-eql-specializer (x)
  `(eql ,x))

;;; ---------------------------------------------------------------------------

(let ((clos-package (find-package 
                     #+abcl ':mop
                     #+allegro ':clos
                     #+clisp ':clos
                     #+clozure ':ccl
                     #+cmu ':pcl
                     #+cormanlisp ':common-lisp ; Not supported yet!
                     #+digitool-mcl ':ccl
                     #+ecl ':clos
                     #+gcl ':pcl
                     #+lispworks ':clos
                     #+sbcl ':sb-pcl
                     #+scl ':clos))
      (gbbopen-tools-package (find-package ':gbbopen-tools)))
  (dolist (uninterned-symbol *clos/mop-symbols*)
    (let* ((symbol-name (symbol-name uninterned-symbol))
           (symbol (find-symbol symbol-name clos-package)))
      (cond
       ;; Good to go:
       (symbol
        (import symbol gbbopen-tools-package)
        (export symbol gbbopen-tools-package))
       ;; Unsupported CLOS/MOP symbol:
       (t (warn "CLOS/MOP ~a is not supported on ~a~@[ running on ~a~]."
                      uninterned-symbol
                      (lisp-implementation-type) 
                      (machine-type))
          (let ((gbbopen-tools-symbol
                 (find-symbol symbol-name gbbopen-tools-package)))
            ;; GBBopen Tools is providing its own version:
            (when gbbopen-tools-symbol
              (warn "Defining :gbbopen-tools equivalent for ~a."
                    uninterned-symbol)
              (export gbbopen-tools-symbol gbbopen-tools-package))))))))

;;; ===========================================================================
;;;                               End of File
;;; ===========================================================================


