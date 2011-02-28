;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/extensions/network-streaming.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Feb 28 10:32:18 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                      Network Streaming Entities
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
;;;  02-28-11 Separated from journaling entities.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package ':portable-sockets))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::*recorded-class-descriptions-ht*
            gbbopen-tools::write-saving/sending-block-info)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*break-on-receive-errors*
            *default-network-stream-server-port*
            *remove-mirroring-when-streamer-closes*
            define-streamer-node
            find-or-make-network-streamer
            find-streamer-node
            handle-stream-connection-exiting
            host-of
            kill-network-stream-server
            name-of
            network-stream-receiver
            network-stream-server-running-p
            network-streamer            ; class-name
            port-of
            start-network-stream-server
            streamer-node)))            ; class-name


;;; ---------------------------------------------------------------------------

(defvar *default-network-stream-server-port* 1968)

;;; ===========================================================================
;;;   Streamer Node

(defvar *streamer-nodes-ht* (make-hash-table :test 'equal))

;;; ---------------------------------------------------------------------------

(define-class streamer-node (standard-gbbopen-instance)
  (name
   host
   port
   passphrase
   package
   external-format
   read-default-float-format 
   authorized-nodes
   streamer-class
   (streamer :initform nil)
   (server-thread :initform nil)))

;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((streamer-node streamer-node) stream)
  (call-next-method)
  (when (slot-boundp streamer-node 'name)
    (format stream " ~s" (name-of streamer-node))))

;;; ---------------------------------------------------------------------------

(defun define-streamer-node (name &key
                                  (host "localhost")
                                  (port *default-network-stream-server-port*)
                                  (passphrase nil)
                                  (package ':cl-user)
                                  (external-format ':default)                                  
                                  (read-default-float-format 
                                   *read-default-float-format*)
                                  (streamer-class 'network-streamer)
                                  (authorized-nodes ':all))
  (let ((streamer-node
         (make-instance 'streamer-node
           :name name
           :host host
           :port port
           :passphrase passphrase
           :package package
           :external-format external-format
           :read-default-float-format read-default-float-format 
           :streamer-class streamer-class
           :authorized-nodes authorized-nodes)))
    (setf (gethash name *streamer-nodes-ht*) streamer-node)
    ;; Return the streamer-node:
    streamer-node))

;;; ---------------------------------------------------------------------------

(defun find-streamer-node (name &optional errorp)
  (or (gethash name *streamer-nodes-ht*)
      (when errorp
        (error "No streamer node named  ~s" name))))

;;; ===========================================================================
;;;   Network Streamer

(define-class network-streamer (streamer)
  (streamer-node 
   (connection-thread :initform nil)))
   
;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((streamer network-streamer) stream)
  (call-next-method)
  (when (slot-boundp streamer 'streamer-node)
    (let ((streamer-node (streamer-node-of streamer)))
      (format stream " ~a:~a"
              (host-of streamer-node)
              (port-of streamer-node)))))

;;; ===========================================================================
;;;   Network Streaming Server

(defparameter *network-stream-format-version* 1)
(defvar *break-on-receive-errors* nil)
(defvar *remove-mirroring-when-streamer-closes* 't)

;;; ---------------------------------------------------------------------------

(defun safe-read (connection)
  (with-error-handling (read connection nil ':eof)
    (format t "~&;; Read error occurred: ~a~%" (error-message))
    ':error))

;;; ---------------------------------------------------------------------------

(defun find-or-make-network-streamer (streamer-node local-streamer-node
                                      &rest initargs)
  ;; Lookup streamer nodes, if needed:
  (unless (typep streamer-node 'streamer-node)
    (setf streamer-node (find-streamer-node streamer-node 't)))
  (unless (typep local-streamer-node 'streamer-node)
    (setf local-streamer-node (find-streamer-node local-streamer-node 't)))
  (let ((streamer (streamer-of streamer-node)))
    (or 
      ;; A streamer already exists, return it:
     streamer
     ;; A new streamer is needed; try to connect to GBBopen Network Server:
     (let ((connection 
            ;; TODO: ** Extend open-connection to accept external-format
            (open-connection (host-of streamer-node) (port-of streamer-node) 
                             :keepalive 't))
           (package (ensure-package (package-of streamer-node)))
           (external-format (external-format-of streamer-node))
           (read-default-float-format 
            (read-default-float-format-of streamer-node)))
       (format connection "(:gbbopen ~s ~s ~s)"
               *network-stream-format-version*
               (passphrase-of streamer-node)
               (name-of local-streamer-node))
       (let ((*package* package)
             (*read-default-float-format* read-default-float-format))
         (write-saving/sending-block-info connection))
       ;; Why is this needed to prevent reading problems...?
       (princ " " connection)
       (force-output connection)
       ;; If connection is established, make and return the streamer:
       (when connection
         (setf (streamer-of streamer-node)
               (apply #'make-instance
                      (streamer-class-of streamer-node)
                      :streamer-node streamer-node
                      :lock (make-recursive-lock 
                             :name (concatenate 'simple-string 
                                     (name-of streamer-node) 
                                     " lock"))
                      :package package
                      :external-format external-format
                      :read-default-float-format read-default-float-format 
                      :stream connection
                      :connection-thread (spawn-thread
                                          "Network streamer connection endpoint"
                                          #'start-streaming-connection-endpoint
                                          streamer-node 
                                          connection
                                          't)
                      initargs)))))))
          
;;; ---------------------------------------------------------------------------
;;;  Connection exiting methods

(defgeneric handle-stream-connection-exiting (network-streamer exit-status))

;; Default handler method:
(defmethod handle-stream-connection-exiting ((network-streamer network-streamer)
                                             exit-status)
  (format t "~&;; Network stream connection ~s closing~@[: (~s)~]~%"
          (name-of (streamer-node-of network-streamer)) exit-status))

;;; ---------------------------------------------------------------------------

(defgeneric network-stream-receiver (network-streamer connection))

(defmethod network-stream-receiver ((network-streamer network-streamer) 
                                    connection)
  (declare (ignorable network-streamer))
  (let ((maximum-contiguous-errors 2)
        (contiguous-errors 0)
        *queued-read-tag*
        (*package* (ensure-package (package-of network-streamer)))
        (*read-default-float-format*
         (read-default-float-format-of network-streamer))
        form)
    (loop
      (setf form 
            (if *break-on-receive-errors*
                (read connection)
                (safe-read connection)))
      (case form
        (:eof (return nil))
        (:error
         (format *trace-output* "~&;; Read error: ~s~%" form)
         (force-output *trace-output*)
         (when (>=& (incf& contiguous-errors) maximum-contiguous-errors)
           (format *trace-output* "~&;; Maximum contiguous errors exceeded; ~
                                        closing connection ~s.~%"
                   connection)
           (force-output *trace-output*)
           (return ':error)))
        (t (setf contiguous-errors 0))))))
  
;;; ---------------------------------------------------------------------------

(defun start-streaming-connection-endpoint (streamer-node connection 
                                            skip-block-info-reading)
  (with-reading-saved/sent-objects-block 
      (connection :skip-block-info-reading skip-block-info-reading)
    (let ((network-streamer (streamer-of streamer-node)))
      (cond 
       (network-streamer
        (let (exit-status)
          (unwind-protect 
              (setf exit-status
                    (network-stream-receiver network-streamer connection))
            (let* ((streamer (streamer-of streamer-node))
                   (broadcast-streamer (broadcast-streamer-of streamer)))
              (setf (closed-of streamer) 't)
              (when *remove-mirroring-when-streamer-closes*
                (remove-mirroring streamer 't))
              ;; Remove from broadcast-streamer:
              (when broadcast-streamer
                (remove-from-broadcast-streamer 
                 streamer broadcast-streamer)))
            (setf (streamer-of streamer-node) nil)
            (setf (stream-of network-streamer) ':closed)
            (handle-stream-connection-exiting network-streamer exit-status))))
       (t (error "Missing network-streamer at ~s" streamer-node)))
      ;; Clean up:
      (when (and (streamp connection)
                 (open-stream-p connection))
        (close connection)))))

;;; ---------------------------------------------------------------------------

(defun validate-passcode (passcode connection)
  (declare (ignore connection))
  ;; Good enough for now!
  (eq passcode nil))

;;; ---------------------------------------------------------------------------

(defun network-streaming-client-connection (connection)
  (let ((authentication-form (safe-read connection)))
    (when (and (consp authentication-form)
               (=& (length authentication-form) 4))
      (destructuring-bind (client version passcode streamer-node-name)
          authentication-form
        (when (and (eq client ':gbbopen)
                   (eql version 1)
                   (validate-passcode passcode connection))
          (let ((streamer-node (find-streamer-node streamer-node-name)))
            (if streamer-node
                (let ((streamer (streamer-of streamer-node)))
                  (when streamer
                    (error "Unexpected connection request from node ~s"
                           streamer-node-name))
                  ;; Create the streamer:
                  (setf (streamer-of streamer-node)
                        (apply 
                         #'make-instance
                         (streamer-class-of streamer-node)
                         :streamer-node streamer-node
                         :lock (make-recursive-lock 
                                :name (concatenate 'simple-string 
                                        (name-of streamer-node) 
                                        " lock"))
                         :package (package-of streamer-node)
                         :external-format (external-format-of streamer-node)
                         :read-default-float-format (read-default-float-format-of 
                                                     streamer-node)
                         :stream connection
                         :connection-thread (current-thread)
                         nil))
                  (start-streaming-connection-endpoint 
                   streamer-node connection nil))
                (warn "Connection request from unknown node ~s"
                      streamer-node-name))))))))

;;; ---------------------------------------------------------------------------

(defun network-stream-connection-server (connection) 
  (flet ((connect-it (connection)
           (unwind-protect (network-streaming-client-connection connection)
             (when (open-stream-p connection)               
               (close connection)))))
    (spawn-thread "Client GBBopen Connection" #'connect-it connection)))

;;; ---------------------------------------------------------------------------

(defun network-stream-server-running-p (streamer-node)
  (unless (typep streamer-node 'streamer-node)
    (setf streamer-node (find-streamer-node streamer-node 't)))
  (let ((server-thread (server-thread-of streamer-node)))
    (and server-thread (thread-alive-p server-thread))))

;;; ---------------------------------------------------------------------------

(defun start-network-stream-server (streamer-node)
  (unless (typep streamer-node 'streamer-node)
    (setf streamer-node (find-streamer-node streamer-node 't)))
  (setf (server-thread-of streamer-node)
        (start-connection-server 'network-stream-connection-server
                                 (port-of streamer-node)
                                 :name "GBBopen Network Connection Server"
                                 :keepalive 't
                                 :reuse-address 't)))

;;; ---------------------------------------------------------------------------

(defun kill-network-stream-server (streamer-node)
  (unless (typep streamer-node 'streamer-node)
    (setf streamer-node (find-streamer-node streamer-node 't)))
  (let ((server-thread (server-thread-of streamer-node)))
    (when server-thread 
      (when (thread-alive-p server-thread)
        (kill-thread server-thread))
      (setf (server-thread-of streamer-node) nil)
      ;; indicate success:
      't)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
