;;;; -*- Mode:Common-Lisp; Package:CL-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/test/network-mirroring-master.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Feb 28 11:03:26 2011 *-*
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

;; Compile/load GBBopen's :network-streaming module:
(network-streaming :create-dirs)

;; Compile/load the :tutorial module (without running it):
(cl-user::tutorial-example :create-dirs :noautorun)

;; The slave host:
(define-streamer-node "slave"
    :host "127.0.0.1"
    :package ':tutorial)

;; The master host (me!):
(define-streamer-node "master"
    :host "127.0.0.1"
    :port (1+ (port-of (find-streamer-node "slave")))
    :package ':tutorial)

;; Connect to slave image:
(defparameter *streamer* (find-or-make-network-streamer "slave" "master"))

(add-mirroring *streamer* 'standard-space-instance)
(add-mirroring *streamer* 'path)
(add-mirroring *streamer* 'location)

;; Generate some data (locally), sending everything as a single queued block:
(with-queued-streaming (*streamer* ':tutorial)
  (take-a-walk))
  
;; Delete an instance:
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

;; Remove a location from the known-world:
(remove-instance-from-space-instance 
 (find-instance-by-name 8 'location) 
 (find-space-instance-by-path '(known-world)))

;; Add the location back to the known-world:
(add-instance-to-space-instance
 (find-instance-by-name 8 'location) 
 (find-space-instance-by-path '(known-world)))

;; Remove another location from the known-world:
(remove-instance-from-space-instance 
 (find-instance-by-name 5 'location) 
 (find-space-instance-by-path '(known-world)))

;; Create a new world:
(make-space-instance 
    '(new-world)
    :allowed-unit-classes '(location path)
    :dimensions (dimensions-of 'location)
    :storage '((location (x y) uniform-buckets :layout ((0 100 5)
                                                        (0 100 5)))))

;; Add a location to the new world:
(add-instance-to-space-instance 
 (find-instance-by-name 6 'location) 
 (find-space-instance-by-path '(new-world)))

;; Move a location in time:
(let ((instance (find-instance-by-name 3 'location)))
  (with-changing-dimension-values (instance time)
    (setf (time-of instance) -10)))

(defun create-a-bunch (n) 
  (declare (fixnum n))
  (dotimes (i n)
    (make-instance 'location
      :time (+& 1 100)
      :x (-& (random 100) 50)
      :y (-& (random 100) 50))))
(compile 'create-a-bunch)

;; Create a bunch of new locations (with event-printing disabled on the
;; slave):
(stream-command-form '(:disable-event-printing t) *streamer*)
(time (create-a-bunch 1000))
#+NON-REMOVAL-TEST
(progn
  (setf *remove-mirroring-when-streamer-closes* nil)
  (time (create-a-bunch 10000)))

(defun update-a-bunch (n) 
  (declare (fixnum n))
  (let ((location (find-instance-by-name 100 'location)))
    (dotimes (i n)
      (setf (x-of location)
            (-& (random 100) 50)))))
(compile 'update-a-bunch)

;; Update a bunch of new locations:
(time (update-a-bunch 20000))

;; Send a silly command:
(stream-command-form '(:print "All done!") *streamer*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================









