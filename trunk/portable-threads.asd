;;;; -*- Mode:Common-Lisp; Package:Portable-Threads-System; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/portable-threads.asd *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue May 27 23:18:00 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Stand-Alone ASDF for Portable Threads 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Attila Lendvai
;;;
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;;  Note: This ASDF system definition is for stand-alone Portable Threads
;;;        use.  The current gbbopen.asd interface does not play nice with
;;;        this one.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-15-07 File created.  (Lendvai)
;;;  11-23-07 Replaced non-portable :pathnames with :modules.  (Costanza)
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
		 (:module
		  "source"
		  :components 
		  ((:module
		    "tools"
		    :components
		    ((:file "portable-threads")
		     (:module
		      "test"
		      :components
		      ((:static-file "portable-threads-test.lisp")))))))))

;;; ---------------------------------------------------------------------------

(defsystem :portable-threads-test
    :depends-on (:portable-threads)
    :components ((:module
		  "source"
		  :components
		  ((:module
		    "tools"
		    :components
		    ((:module
		      "test"
		      :components
		      ((:file "portable-threads-test")))))))))

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
