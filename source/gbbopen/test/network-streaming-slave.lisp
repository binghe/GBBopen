;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-streaming-slave.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Feb 10 14:12:58 2011 *-*
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
(network-streaming :create-dirs)
;; Help 
(setf gbbopen:*break-on-receive-errors* 't)

;; Load up the :tutorial (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; Silly queued-reception methods:
(defmethod beginning-queued-receive ((tag (eql ':tutorial)))
  (format t "~&;; Beginning ~s queued receive...~%" tag))
(defmethod ending-queued-receive ((tag (eql ':tutorial)))
  (format t "~&;; Ending ~s queued receive.~%" tag))
(defmethod beginning-queued-receive ((tag (eql ':with-queued)))
  (format t "~&;; Beginning ~a receive...~%" tag))
(defmethod ending-queued-receive ((tag (eql ':with-queued)))
  (format t "~&;; Ending ~a receive.~%" tag))

;; Show what is happening once streaming begins!
(enable-event-printing 'create-instance-event 'location)
(enable-event-printing 'delete-instance-event 'location)
(add-event-function
 ;; Enable update-nonlink-slot-event printing only after the delete-instance
 ;; has been received:
 #'(lambda (&rest args)
     (declare (ignore args))
     (enable-event-printing 'update-nonlink-slot-event 'location :slot-name 'time)
     (enable-event-printing '(link-slot-event +) 'location :slot-name 'previous-location)
     (enable-event-printing '(link-slot-event +) 'location :slot-name 'next-location))
 'delete-instance-event 'location)

;; Don't warn that the Agenda Shell isn't running to process trigger events on
;; received goodies:
(setf *warn-about-unusual-requests* nil)

;; Create the known-world space instance (until sending space-instances is
;; supported):
#-REMOVE-WHEN-SPACE-INSTANCE-SENDING-IS-FIXED
(make-space-instance 
    '(known-world)
    :allowed-unit-classes '(location path)
    :dimensions (dimensions-of 'location)
    :storage '((location (x y) uniform-buckets :layout ((0 100 5)
                                                        (0 100 5)))))
;; Prepare to receive from the master:
(defparameter *gbbopen-network-server*
    (start-gbbopen-network-server))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


