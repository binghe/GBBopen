;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-streaming-master.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Feb 22 04:14:05 2011 *-*
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

;; Make a (trivial) broadcast streamer:
(defparameter *broadcast-streamer* (make-broadcast-streamer *streamer*))

;; Generate some data (locally):
(take-a-walk)

;; Mirror the space instances:
(stream-instances (find-space-instances 't) *streamer*)

;; Send everything else (as a single queued block):
(with-queued-streaming (*streamer* ':tutorial)
  (stream-instances (find-instances 't 't 't) *broadcast-streamer*))

;; Test empty queue writing:
(with-queued-streaming (*streamer* ':empty-queue-that-should-not-be-written)
  nil)
(with-queued-streaming (*streamer* ':empty-queue-that-should-be-written 't)
  nil)

;; Delete an instance on the slave (but not here), also testing
;; some nested WITH-QUEUED-STREAMING macros:
(with-queued-streaming (*streamer* ':another-empty-queue-that-should-be-written 't)
  (with-queued-streaming (*streamer* ':empty-queue-that-should-not-be-written)
    (with-queued-streaming (*streamer* ':empty-queue-that-should-be-written 't)
      (with-queued-streaming (*streamer* ':with-queued)
        (stream-delete-instance (find-instance-by-name 10 'location) *streamer*)))))

;; Change some nonlink-slot values on the slave (but not here), also testing a
;; unit-instance tag and WRITE-STREAMER-QUEUE:
(with-queued-streaming (*streamer* (find-instance-by-name 11 'location))
  (stream-slot-update 
   (find-instance-by-name 11 'location) 'time 9 *streamer*)
  (write-streamer-queue *streamer*)
  (stream-slot-update 
   (find-instance-by-name 12 'location) 'time 10 *streamer*)
  (write-streamer-queue *streamer* :tag (find-instance-by-name 13 'location))
  (stream-slot-update 
   (find-instance-by-name 13 'location) 'time 10 *streamer*))

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

;; Remove a location from the known-world on the slave (but not here):
(stream-remove-from-space (find-instance-by-name 8 'location) 
                          (find-space-instance-by-path '(known-world))
                          *streamer*)

;; Add the location back to the known-world on the slave (but not here):
(stream-add-to-space (find-instance-by-name 8 'location) 
                     (find-space-instance-by-path '(known-world))
                     *streamer*)

;; Remove another location from the known-world on the slave (but not here):
(stream-remove-from-space (find-instance-by-name 5 'location) 
                          (find-space-instance-by-path '(known-world))
                          *streamer*)

;; Send a silly command:
(stream-command-form '(:print "All done!") *streamer*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================









