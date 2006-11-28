;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/os-interface.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Sep 22 06:58:56 2006 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Operating System Extensions
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2006, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-17-05 File created.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(browse-hyperdoc
	    close-external-program-stream
	    kill-external-program	; not yet documented
	    run-external-program)))

;;; ---------------------------------------------------------------------------
;;;  Unify some implementation differences

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   #+allegro
   '()
   #+clisp
   '(ext:run-program)
   #+cmu
   '(ext:run-program
     ext:process-input
     ext:process-kill
     ext:process-output)
   #+cormanlisp
   '()
   #+digitool-mcl
   '()
   #+ecl
   '()
   #+lispworks
   '()
   #+openmcl
   '(ccl:run-program 
     ccl:external-process-output-stream 
     ccl:external-process-input-stream)
   #+sbcl
   '(sb-ext:run-program 
     sb-ext:process-input 
     sb-ext:process-kill 
     sb-ext:process-output)
   #+scl
   '(ext:run-program
     ext:process-input
     ext:process-kill
     ext:process-output)))

#+(or cmu sbcl scl)
(progn
  (defun external-process-input-stream (process)
    (process-input process))
 
  (defun external-process-output-stream (process)
    (process-output process)))

;;; ===========================================================================
;;;  Run-external-program
;;;
;;;  Returns two values:
;;;   1) the io-stream, if appropriate
;;;   2) the process identifier, if available
;;;
;;;  Note that Lispworks and SBCL do not search PATH for programs, the path 
;;;  must be explicitly included in the `program' string.
;;;
;;;  Support for Corman Common Lisp and Digitool MCL is incomplete.

(defun run-external-program (program args &key (input ':stream)
					       (output ':stream)
					       wait)
  #+(or cormanlisp digitool-mcl)
  (declare (ignore input output wait))  
  #+allegro
  (system:reap-os-subprocess)
  #+allegro
  (multiple-value-bind (io-stream error-stream process)
      (excl:run-shell-command 
       #+(and x86 (not linux86))
       (apply #'concatenate #'string program args)
       #-(and x86 (not linux86))
       (apply #'vector program program args)
       :input input
       :output output
       :wait wait)
    (declare (ignore error-stream))
    (values io-stream process))
  #+clisp
  (values
   (run-program program 
		:arguments args
		:input input
		:output output
		:wait (or wait (eq input ':stream) (eq output ':stream)))
   ;; no process id is available in CLISP:
   nil)
  #+cormanlisp
  (progn
    (win32:shell (format nil "~A ~{~A ~}" program args))
    nil)
  #+digitool-mcl-not-yet
  (let ((process (ccl:do-shell-script (format nil "~A ~{~A ~}" program args))))
    (values nil process))
  #+ecl
  (progn
    (when wait
      (error "~s on ECL does not support :wait ~s"
	     'run-external-program 't))
    (values (si:run-program program args
			    :input input
			    :output output)
	    ;; no process id is available in ECL:
	    nil))
  #+(and lispworks (not win32))
  (multiple-value-bind (io-stream error-stream process)
      (system:run-shell-command 
       `(,program ,@args)
       :input input
       :output output
       :wait wait)
    (declare (ignore error-stream))
    (values io-stream process))
  #+(and lispworks win32)
  ;; 'program' is duplicated here due to bug found in LWW 4.4.6 where 'program'
  ;; was not passed to ARGV when args as non NIL. The bug may or may not be
  ;; present in unix versions. Environment variable PATH *IS* searched.
  (system:open-pipe `(,program ,program ,@args) :direction :io)
  #+(or cmu openmcl sbcl scl)
  (let ((process (run-program program args
			      :input input
			      :output output
			      :wait wait)))
    (values
     (make-two-way-stream 
      (external-process-output-stream process)              
      (external-process-input-stream process))
     process))
  #-(or allegro clisp cmu digitool-mcl ecl lispworks openmcl sbcl scl)
  (port-needed 'run-external-program))

;;; ---------------------------------------------------------------------------

(defun close-external-program-stream (stream)
  ;; CLISP requires individual closing of all 3 streams:
  #+clisp
  (progn
    (close (two-way-stream-input-stream stream))
    (close (two-way-stream-output-stream stream)))
  (prog1 (close stream)
    #+allegro
    (system:reap-os-subprocess)))
   
;;; ---------------------------------------------------------------------------

(defun kill-external-program (os-process)
  #-(or cmu sbcl scl)
  (declare (ignore os-process))
  #+(or cmu scl)
  (process-kill os-process ':sigint)
  #+sbcl
  (process-kill os-process)
  #-(or cmu sbcl scl)
  (port-needed 'kill-external-program))

;;; ---------------------------------------------------------------------------

(defun browse-hyperdoc (symbol)
  (let ((url (hyperdoc-url symbol)))
    (cond 
     (url (run-external-program *preferred-browser* (list url))
	  't)
     (t (values)))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
