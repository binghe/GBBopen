;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/extensions/network-streaming.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Mar  7 13:43:15 2011 *-*
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
  (import '(gbbopen-tools::write-saving/sending-block-info)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*break-on-receive-errors*   ; not yet documented
            *default-network-stream-server-port* ; not yet documented
            *remove-mirroring-when-streamer-closes* ; not yet documented
            close-network-streamer
            define-streamer-node
            ensure-streamer-node        ; not yet documented
            find-or-make-network-streamer ; old name, remove soon
            find-streamer-node
            handle-stream-connection-exiting ; not yet documented
            host-of                     ; not yet documented
            kill-network-stream-server
            name-of                     ; not yet documented
            network-stream-receiver     ; not yet documented
            network-stream-server-running-p
            network-streamer            ; class-name (not yet documented)
            open-network-streamer
            port-of                     ; not yet documented
            start-network-stream-server
            streamer-node)))            ; class-name (not yet documented)

;;; ---------------------------------------------------------------------------

(defvar *default-network-stream-server-port* 1968)

;;; ===========================================================================
;;;   Streamer Node

(defvar *streamer-nodes-ht* (make-hash-table :test 'equal))

;;; ---------------------------------------------------------------------------

(define-class streamer-node (standard-gbbopen-instance)
  (name
   (host :initform "localhost")
   (port :initform *default-network-stream-server-port*)
   (documentation :initform nil)
   (passphrase :type (or simple-string null) :initform nil)
   (package :initform (ensure-package ':common-lisp-user))
   (external-format :initform ':default)
   (read-default-float-format :initform *read-default-float-format*)
   (streamer-class :initform 'network-streamer)
   (authorized-nodes :initform ':all)
   (streamer :initform nil)
   (server-thread :initform nil)))

;;; ---------------------------------------------------------------------------

(defmethod print-instance-slots ((streamer-node streamer-node) stream)
  (call-next-method)
  (when (slot-boundp streamer-node 'name)
    (format stream " ~s" (name-of streamer-node))))

;;; ---------------------------------------------------------------------------

(defun ensure-streamer-node (name &rest initargs)
  (let ((streamer-node
         (apply #'make-instance 'streamer-node :name name initargs)))
    (setf (gethash name *streamer-nodes-ht*) streamer-node)
    ;; Return the streamer-node:
    streamer-node))

;;; ---------------------------------------------------------------------------

(defmacro define-streamer-node (name &rest initargs)
  `(ensure-streamer-node ',name ,@initargs)) 

;;; ---------------------------------------------------------------------------

(defun find-streamer-node (name &optional errorp)
  (or (gethash name *streamer-nodes-ht*)
      (when errorp
        (error "No streamer node named ~s" name))))

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

(defun open-network-streamer (streamer-node local-streamer-node
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
     ;; A new streamer is needed; try to connect to the network server:
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
       ;; Transmit the particulars used in writing to the connection:       
       (let ((*package* package)
             (*read-default-float-format* read-default-float-format))
         (write-saving/sending-block-info connection)
         (force-output connection))
       ;; If connection is established, make and return the streamer:
       (when connection
         (setf (streamer-of streamer-node)
               (apply #'make-instance
                      (streamer-class-of streamer-node)
                      :streamer-node streamer-node
                      :lock (make-lock 
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
                                          connection)
                      initargs)))))))
          
;;; ---------------------------------------------------------------------------
;;;  Old name, remove soon:

(defun find-or-make-network-streamer (streamer-node local-streamer-node
                                      &rest initargs)
  (declare (dynamic-extent initargs))
  (apply 'open-network-streamer streamer-node local-streamer-node initargs))

;;; ---------------------------------------------------------------------------

(defun close-network-streamer (network-streamer)
  (let ((connection-thread (connection-thread-of network-streamer)))
    (when (and connection-thread (thread-alive-p connection-thread))
      (run-in-thread connection-thread #'(lambda () (throw 'close nil))))))

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

(defun start-streaming-connection-endpoint (streamer-node connection)
  (with-reading-saved/sent-objects-block 
      (connection)
    (let ((network-streamer (streamer-of streamer-node)))
      (cond 
       (network-streamer
        (let (exit-status)
          (unwind-protect 
              (catch 'close
                (setf exit-status
                      (network-stream-receiver network-streamer connection)))
            (let* ((streamer (streamer-of streamer-node))
                   (broadcast-streamer (broadcast-streamer-of streamer)))
              (setf (closed-of streamer) 't)
              (when *remove-mirroring-when-streamer-closes*
                (remove-mirroring streamer))
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

(defun validate-passphrase (passphrase local-streamer-node)
  ;; Good enough for now!
  (or (equal passphrase (passphrase-of local-streamer-node))
      (warn "Incorrect passphrase ~s supplied for ~s"
            passphrase
            local-streamer-node)))

;;; ---------------------------------------------------------------------------

(defun network-streaming-client-connection-3 (connection
                                              connecting-streamer-node-name)
  (let ((streamer-node
         (find-streamer-node connecting-streamer-node-name)))
    (cond
     (streamer-node
      (let ((streamer (streamer-of streamer-node))
            (package (ensure-package (package-of streamer-node)))
            (read-default-float-format
             (read-default-float-format-of streamer-node)))
        (when streamer
          (error "Unexpected connection request from ~s: already connected"
                 connecting-streamer-node-name))
        ;; Create the streamer:
        (setf (streamer-of streamer-node)
              (apply 
               #'make-instance
               (streamer-class-of streamer-node)
               :streamer-node streamer-node
               :lock (make-lock :name (concatenate 'simple-string 
                                        (name-of streamer-node) 
                                        " lock"))
               :package package
               :external-format (external-format-of streamer-node)
               :read-default-float-format read-default-float-format
               :stream connection
               :connection-thread (current-thread)
               nil))
        ;; Transmit the particulars used in writing to the connection:
        (let ((*package* package)
              (*read-default-float-format* read-default-float-format))
          (write-saving/sending-block-info connection)
          (force-output connection))
        (start-streaming-connection-endpoint streamer-node connection)))
     (t (warn "Connection request from unknown streamer node ~s"
              connecting-streamer-node-name)))))

;;; ---------------------------------------------------------------------------

(defun network-streaming-client-connection-2 (local-streamer-node connection
                                              authentication-form)
  (destructuring-bind (client version passphrase 
                       connecting-streamer-node-name)
      authentication-form
    (cond 
     ((and (eq client ':gbbopen)
           (eql version 1)
           (let ((authorized-nodes
                  (authorized-nodes-of local-streamer-node)))
             (or (eq authorized-nodes ':all)
                 (member connecting-streamer-node-name authorized-nodes
                         :test #'equalp)))
           (validate-passphrase passphrase local-streamer-node))
      (network-streaming-client-connection-3
       connection connecting-streamer-node-name))
      (t (warn "Authorization failure: ~s" authentication-form)))))

;;; ---------------------------------------------------------------------------

(defun network-streaming-client-connection (local-streamer-node connection)
  (let ((authentication-form (safe-read connection)))
    (cond
     ((and (consp authentication-form)
           (=& (length authentication-form) 4))
      (network-streaming-client-connection-2
       local-streamer-node connection authentication-form))
     (t (warn "Protocol failure: ~s" authentication-form)))))

;;; ---------------------------------------------------------------------------

(defun network-stream-connection-server (streamer-node connection) 
  (flet ((connect-it (connection)
           (unwind-protect (network-streaming-client-connection 
                            streamer-node connection)
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
        (start-connection-server
         #'(lambda (connection)         ; indefinite-extent fn
             (network-stream-connection-server streamer-node connection))
         (port-of streamer-node)
         :name "Network Connection Server"
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
