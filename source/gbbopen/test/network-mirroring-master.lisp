;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-mirroring-master.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Feb 18 18:08:50 2011 *-*
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

;; Compile/load GBBopen's :streaming module:
(streaming :create-dirs)

;; Compile/load the :tutorial module (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; The slave host:
(define-streamer-node "slave"
    :host "127.0.0.1"
    :package ':tutorial)

;; The master host (me!):
(define-streamer-node "master"
    :localnodep 't
    :host "127.0.0.1"
    :port (1+ (port-of (find-streamer-node "slave")))
    :package ':tutorial)

;; Connect to slave image:
(defparameter *streamer* (find-or-make-network-streamer "slave"))

(add-mirroring *streamer* 'standard-space-instance)
(add-mirroring *streamer* 'path)
(add-mirroring *streamer* 'location)

;; Generate some data (locally), with BEGIN/END-QUEUED-STREAMING:
(let ((queued-streamer
       (begin-queued-streaming *streamer* ':tutorial)))
  (take-a-walk)
  (end-queued-streaming queued-streamer))
  
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

;; Send a silly command:
(stream-command-form '(:print "All done!") *streamer*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================









