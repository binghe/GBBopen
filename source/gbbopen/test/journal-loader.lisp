;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/journal-loader.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Mar 25 10:52:54 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    Journal Reader (Loader) Example
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2011, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  02-17-11 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :cl-user)

;; Compile/load GBBopen's :streaming module:
(streaming :create-dirs)

;; Compile/load the :tutorial module (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; Define a link pointer:
(define-class link-ptr-with-value (standard-link-pointer)
  ((value :initform nil)))

;; Silly read-queued-streaming-block method:
(defmethod read-queued-streaming-block :around ((tag t) string-stream)
  (declare (ignorable string-stream))
  (format t "~&;; Beginning queued ~a reading...~%" tag)
  (call-next-method)
  (format t "~&;; Ending queued ~a reading.~%" tag))

;; Silly command-form method:
(defmethod handle-streamed-command-form (streamer 
                                         (command (eql ':print)) &rest args)
  (declare (ignorable streamer))
  (format t "~&;; Print:~{ ~s~}~%" args))

;; Don't warn that the Agenda Shell isn't running to process trigger events on
;; received goodies:
(setf *warn-about-unusual-requests* nil)

(defun journal-loading-completion-printer (stream percent-loaded)
  (declare (ignore stream))
  (format t "~&;; ~3d% loaded~%" percent-loaded))

(pushnew 'journal-loading-completion-printer 
         *journal-load-percentage-hook-functions*)

;; Load the journal:
(load-journal "tutorial")

;; Check the results:
(unless (find-instances 'location '(known-world) '(= time -10))
  (error "Location ~s was not properly positioned"
         (find-instance-by-name 3 'location)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
