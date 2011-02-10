;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-mirroring-master.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Feb 10 03:55:10 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     GBBopen Network Streaming Master
;;;; *                  (start the slave before this master!)
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

;; Load up the :tutorial (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; The slave host:
(defparameter *slave-host* "127.0.0.1")

;; Connect to slave image:
(defparameter *streamer*
    (make-gbbopen-network-streamer *slave-host*))

(add-mirroring *streamer* 'path)
(add-mirroring *streamer* 'location)

;; Generate some data (locally):
(begin-queued-streaming *streamer* ':tutorial)
(take-a-walk)
(end-queued-streaming *streamer*)

;; Mirror the space instances (once sending space-instances is supported):
#+FIX-THIS-AND-THEN-CHANGE-SLAVE-FILE
(stream-instances (find-space-instances 't) *streamer*)

;; Delete an instance, also testing WITH-QUEUED-STREAMING:
(with-queued-streaming (*streamer* ':with-queued)
  (delete-instance (find-instance-by-name 10 'location)))

;; Change a nonlink-slot value:
(setf (time-of (find-instance-by-name 11 'location)) 9)

;; Perform an unlink:
(unlinkf (previous-location-of (find-instance-by-name 9 'location))
         (find-instance-by-name 8 'location))

;; Perform a link:
(linkf (next-location-of (find-instance-by-name 8 'location))
       (find-instance-by-name 9 'location))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================









