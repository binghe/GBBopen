;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-streaming-slave.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Mar 21 15:03:48 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                    GBBopen Network Streaming Slave 
;;;; *                  (start this slave before the master!)
;;;; *
;;;; *                   [Experimental! Subject to change]
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
;;;  02-01-11 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :cl-user)

;; Compile/load GBBopen's :network-streaming module:
(network-streaming :create-dirs)

;; Compile/load the :tutorial module (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; The host (me!):
(define-streamer-node "slave"
    :host "127.0.0.1"
    :package ':common-lisp
    :passphrase "Open, says me!"
    :authorized-nodes '("master"))

;; The master host:
(define-streamer-node "master"
    :port (1+ (port-of (find-streamer-node "slave")))
    :read-default-float-format 'long-float
    :package ':gbbopen)

;; Help 
#+IF-DEBUGGING
(setf gbbopen:*break-on-receive-errors* 't)

;; Define a link pointer:
(define-class link-ptr-with-value (standard-link-pointer)
  ((value :initform nil)))

;; Silly queued-reception methods:
(defmethod beginning-queued-read ((tag (eql ':tutorial)))
  (format t "~&;; Beginning ~s queued receive...~%" tag))
(defmethod ending-queued-read ((tag (eql ':tutorial)))
  (format t "~&;; Ending ~s queued receive.~%" tag))
(defmethod beginning-queued-read ((tag t))
  (format t "~&;; Beginning ~a receive...~%" tag))
(defmethod ending-queued-read ((tag t))
  (format t "~&;; Ending ~a receive.~%" tag))

;; Silly command-form method:
(defmethod handle-streamed-command-form (streamer (command (eql ':print)) &rest args)
  (format t "~&;; Print: ~s~{ ~s~}~%" (streamer-node-of streamer) args))

;; Slightly more useful command-form method:
(defmethod handle-streamed-command-form (streamer (command (eql ':pa)) &rest args)
  (declare (ignorable streamer) (ignore args))
  (format t "~&;; Package: ~s ~%" *package*))

;; A more useful command-form method:
(defmethod handle-streamed-command-form (streamer 
                                         (command (eql ':disable-event-printing))
                                         &rest args)
  (declare (ignorable streamer) (dynamic-extent args))
  (apply #'disable-event-printing args))

;; Silly connection-exiting method:
(defmethod handle-stream-connection-exiting ((connection stream) exit-status)
  (format t "~&;; Connection ~s closing~@[: (~s)~]~%"
          connection exit-status))

;; Show what is happening once streaming begins!
(enable-event-printing 'instance-created-event 'location)
(enable-event-printing 'delete-instance-event 'location)
(add-event-function
 ;; Enable update-nonlink-slot-event printing only after the delete-instance
 ;; has been received:
 #'(lambda (&rest args)
     (declare (ignore args))
     (enable-event-printing 'nonlink-slot-updated-event 'location :slot-name 'time)
     (enable-event-printing '(link-slot-event +) 'location :slot-name 'previous-location)
     (enable-event-printing '(link-slot-event +) 'location :slot-name 'next-location)
     (enable-event-printing 'instance-added-to-space-instance-event 'location)
     (enable-event-printing 'instance-removed-from-space-instance-event 'location))
 'delete-instance-event 'location)

;; Don't warn that the Agenda Shell isn't running to process trigger events on
;; received goodies:
(setf *warn-about-unusual-requests* nil)

;; Prepare to receive from the master:
(defparameter *network-stream-server* (start-network-stream-server "slave"))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


