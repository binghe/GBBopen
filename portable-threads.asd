;;;; -*- Mode:Common-Lisp; Package:Portable-Threads-System; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/portable-threads.asd *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Nov 16 05:00:34 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Stand-Alone ASDF for Portable Threads 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Attila Lendval
;;;
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;;  Note: This ASDF system definition is for stand-alone Portable Threads
;;;        use.  The current gbbopen.asd interface does not play nice with
;;;        this one.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-15-07 File created.  (Lendval)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(require :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :portable-threads-system)
    (defpackage :portable-threads-system
      (:use :common-lisp :asdf))))

(in-package :portable-threads-system)

;;; ---------------------------------------------------------------------------

(defsystem :portable-threads
    :author "The GBBopen Project <gbbopen@GBBopen.org>"
    :maintainer "Dan Corkill <corkill@GBBopen.org>"
    :licence 
      "Part of the GBBopen Project (see LICENSE for license information)."
    :description "Portable Threads"
    :components ((:static-file "COPYING")
                 (:static-file "LICENSE")
                 (:static-file "portable-threads-test"
                               :pathname
                               "source/tools/test/portable-threads-test.lisp")
                 (:file "portable-threads"
                        :pathname
                        "source/tools/portable-threads.lisp")))

;;; ---------------------------------------------------------------------------

(defsystem :portable-threads-test
    :depends-on (:portable-threads)
    :components ((:file "portable-threads-test"
                        :pathname 
                        "source/tools/test/portable-threads-test.lisp")))

;;; ---------------------------------------------------------------------------

(defmethod perform ((op test-op)
                    (system (eql (find-system :portable-threads))))
  (operate 'load-op ':portable-threads-test)
  (funcall (intern (symbol-name '#:portable-threads-tests) 
                   :portable-threads-user))
  (values))

(defmethod operation-done-p ((op test-op) 
                             (system (eql (find-system :portable-threads))))
  nil)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
