;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-streaming-master.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Feb 11 09:48:15 2011 *-*
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

;; Generate some data (locally):
(take-a-walk)

;; Mirror the space instances:
(stream-instances (find-space-instances 't) *streamer*)

;; Send everything else (as a single queued block):
(begin-queued-streaming *streamer* ':tutorial)
(stream-instances (find-instances 't 't 't) *streamer*)
(end-queued-streaming *streamer*)

;; Delete an instance on the slave (but not here), also testing
;; WITH-QUEUED-STREAMING:
(with-queued-streaming (*streamer* ':with-queued)
  (stream-delete-instance (find-instance-by-name 10 'location) *streamer*))

;; Change a nonlink-slot value on the slave (but not here):
(stream-slot-update (find-instance-by-name 11 'location) 'time 9 *streamer*)

;; Perform an unlink on the slave (but not here):
(stream-unlink (find-instance-by-name 9 'location) 
               'previous-location
               (find-instance-by-name 8 'location) 
               *streamer*)

;; Perform a link on the slave (but not here):
(stream-link (find-instance-by-name 8 'location) 
             'next-location
             (find-instance-by-name 9 'location) 
             *streamer*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================









