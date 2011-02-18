;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/journal-loader.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Feb 18 10:09:34 2011 *-*
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

;; Silly queued-reception methods:
(defmethod beginning-queued-read ((tag (eql ':tutorial)))
  (format t "~&;; Beginning ~s queued receive...~%" tag))
(defmethod ending-queued-read ((tag (eql ':tutorial)))
  (format t "~&;; Ending ~s queued receive.~%" tag))
(defmethod beginning-queued-read ((tag t))
  (format t "~&;; Beginning ~a receive...~%" tag))
(defmethod ending-queued-read ((tag t))
  (format t "~&;; Ending ~a receive.~%" tag))

;; Silly command form method:
(defmethod handle-streamed-command-form ((command (eql ':print)) &rest args)
  (format t "~&;; Print:~{ ~s~}~%" args))

;; Don't warn that the Agenda Shell isn't running to process trigger events on
;; received goodies:
(setf *warn-about-unusual-requests* nil)

;; Load the journal:
(load-journal "~/tutorial.jnl")

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
