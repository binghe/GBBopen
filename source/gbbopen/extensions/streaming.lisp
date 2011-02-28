;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/extensions/streaming.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Feb 28 05:10:15 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *              GBBopen Network Streaming & Journaling Entities
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
;;;  01-19-21 File created.  (Corkill)
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
            add-mirroring
            add-to-broadcast-streamer
            begin-queued-streaming
            broadcast-streamer          ; class-name
            beginning-queued-read
            define-streamer-node
            describe-mirroring          ; not yet documented`
            end-queued-streaming
            ending-queued-read
            find-or-make-network-streamer
            find-streamer-node
            handle-stream-connection-exiting
            handle-streamed-command-atom
            handle-streamed-command-form
            host-of
            journal-streamer            ; class-name
            kill-network-stream-server
            load-journal
            make-broadcast-streamer
            make-journal-streamer
            name-of
            network-stream-receiver
            network-stream-server-running-p
            network-streamer            ; class-name
            open-streamer-p
            port-of
            remove-from-broadcast-streamer
            remove-mirroring
            start-network-stream-server
            stream-command-form
            stream-add-to-space
            stream-delete-instance
            stream-instance
            stream-instances
            stream-instances-of-class
            stream-instances-on-space-instances
            stream-link
            stream-remove-from-space
            stream-slot-update
            stream-unlink
            streamer                    ; class-name
            streamer-error              ; condition-name
            streamer-node               ; class-name
	    with-mirroring-disabled
	    with-mirroring-enabled
            with-queued-streaming
            write-streamer-queue)))

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
   (streamer :initform nil)))

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
;;;   Streamer queues

(defvar *%%streamer-queues%%* nil)      ; records thread-local streamer queues

;;; ---------------------------------------------------------------------------

(defstruct (streamer-queue 
            (:conc-name #.(dotted-conc-name 'streamer-queue))
            (:copier nil))
  stream
  tag
  write-empty-queue-p
  tag-string)

;;; ===========================================================================
;;;   Streamers

(define-class basic-streamer (%trivial-streamer%)
  (lock
   (stream :initform nil)
   (closed :initform nil)
   package
   read-default-float-format 
   (recorded-class-descriptions-ht :initform (make-hash-table :test 'eq))))

;;; ---------------------------------------------------------------------------

(defun open-streamer-p (streamer)
  (let ((stream (stream-of streamer)))
    (and (streamp stream) (open-stream-p stream))))

;;; ---------------------------------------------------------------------------

(define-class streamer (basic-streamer)
  (external-format
   (broadcast-streamer :initform nil)))

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

(define-class journal-streamer (streamer)
  ())
   
;;; ---------------------------------------------------------------------------

(define-condition streamer-error (error)
  ((streamer :reader streamer-error-streamer :initarg :streamer))
  (:report
   (lambda (condition stream)
     (format stream "Operation on closed streamer ~s"
             (streamer-error-streamer condition)))))

;;; ===========================================================================
;;;   Broadcast streamers

(define-class broadcast-streamer (basic-streamer)
  ((streamers :initform nil)))

;;; ---------------------------------------------------------------------------

(defun make-broadcast-streamer-given-initargs 
    (&key (package ':cl-user)
          (read-default-float-format *read-default-float-format*))
  (make-instance 'broadcast-streamer
    :lock (make-recursive-lock :name "Broadcast streamer lock")
    :package (ensure-package package)
    :read-default-float-format read-default-float-format))

;;; ---------------------------------------------------------------------------

(defun set-broadcast-streamers (broadcast-streamer streamers)
  (setf (stream-of broadcast-streamer)
        (apply #'make-broadcast-stream 
               (flet ((add-it (streamer)
                        (setf (broadcast-streamer-of streamer)
                              broadcast-streamer)
                        (stream-of streamer)))
                 (declare (dynamic-extent #'add-it))
                 (mapcar #'add-it streamers))))
  (setf (streamers-of broadcast-streamer) streamers))

;;; ---------------------------------------------------------------------------

(defun make-broadcast-streamer-given-streamers (streamers)
  ;; Check if any of the streamers are queued (at least in this thread); TODO:
  ;; deal with queueing better than this!
  (dolist (streamer streamers)
    (when (assq streamer *%%streamer-queues%%*)
      (error "A queued streamer cannot be added to a broadcast streamer: ~s"
             streamer)))
  ;; TODO: Check that all streamers have the same package/default-float, no
  ;; duplicates, creating without any streamers, etc.
  (let ((broadcast-streamer
         (make-broadcast-streamer-given-initargs
          ;; For now, use the attributes of the 1st streamer:
          :package (package-of (car streamers))
          :read-default-float-format (read-default-float-format-of (car streamers)))))
    (set-broadcast-streamers broadcast-streamer streamers)
    ;; Return the broadcast streamer:
    broadcast-streamer))

;;; ---------------------------------------------------------------------------

(defun make-broadcast-streamer (&rest streamers-or-initargs)
  (if (and (consp streamers-or-initargs)
           (typep (car streamers-or-initargs) 'streamer))
      (make-broadcast-streamer-given-streamers streamers-or-initargs)
      (apply #'make-broadcast-streamer-given-initargs streamers-or-initargs)))

;;; ---------------------------------------------------------------------------

(defun check-broadcast-streamer-compatibility (streamer broadcast-streamer)
  ;; Check :package:
  (let ((streamer-package (package-of streamer))
        (broadcast-package (package-of broadcast-streamer)))
    (unless (eq streamer-package broadcast-package)
    (error "The ~s ~s of ~s does not match ~s of ~s"
           ':package 
           streamer-package
           streamer
           broadcast-package
           broadcast-streamer)))
  ;; Check :read-default-float-format:
  (let ((streamer-rdff (read-default-float-format-of streamer))
        (broadcast-rdff (read-default-float-format-of broadcast-streamer)))
    (unless (eq streamer-rdff broadcast-rdff)
    (error "The ~s ~s of ~s does not match ~s of ~s"
           ':read-default-float-format
           streamer-rdff
           streamer
           broadcast-rdff
           broadcast-streamer))))

;;; ---------------------------------------------------------------------------

(defun add-to-broadcast-streamer (streamer broadcast-streamer)
  (cond 
   ((broadcast-streamer-of streamer)
    (error "Streamer ~s is a member of broadcast streamer ~s"
           streamer broadcast-streamer))
   (t (with-lock-held ((lock-of broadcast-streamer))
        (let ((streamers (cons streamer (streamers-of broadcast-streamer))))
          (check-broadcast-streamer-compatibility streamer broadcast-streamer)
          (setf (broadcast-streamer-of streamer) broadcast-streamer)
          (set-broadcast-streamers broadcast-streamer streamers)))
      ;; Return streamer:
      streamer)))

;;; ---------------------------------------------------------------------------

(defun remove-from-broadcast-streamer (streamer broadcast-streamer)
  (with-lock-held ((lock-of broadcast-streamer))
    (let ((streamers (streamers-of broadcast-streamer)))
      (when (memq streamer streamers)
        (setf (broadcast-streamer-of streamer) nil)
        (set-broadcast-streamers 
         broadcast-streamer
         (remove streamer streamers :test #'eq))
        ;; Return streamer on success:
        streamer))))

;;; ===========================================================================
;;;   Streamer entities

(defun %do-with-streamer-stream (streamer body-form-fn)
  (let ((streamer-for-writing streamer)) 
    (when (closed-of streamer)
      (error 'streamer-error :streamer streamer))
    (when (typep streamer 'streamer)
      (let ((broadcast-streamer (broadcast-streamer-of streamer)))
        ;; If streamer is a constituent of a broadcast-streamer, use the
        ;; streamer for writing/queuing but the broadcast-streamer for locking:
        (when broadcast-streamer
          (setf streamer broadcast-streamer))))
    (let* ((streamer-acons (assq streamer-for-writing *%%streamer-queues%%*))
           (streamer-queue (cdr streamer-acons)))
      (with-standard-io-syntax 
        (setf *package* (package-of streamer))
        (setf *read-default-float-format* 
              (read-default-float-format-of streamer))
        (let ((*recorded-class-descriptions-ht* 
               (recorded-class-descriptions-ht-of streamer-for-writing)))
          (flet ((do-it ()
                   (if streamer-queue
                       ;; queued streaming:
                       (funcall body-form-fn (streamer-queue.stream streamer-queue))
                       ;; regular streaming:
                       (with-lock-held ((lock-of streamer))
                         (let ((stream (stream-of streamer-for-writing)))
                           (funcall body-form-fn stream)
                           (force-output stream))))))
            ;; Quick & dirty handling of stream errors -- timeout checks and
            ;; notifying other endpoint needed!!
            (with-error-handling ((do-it) :conditions 'stream-error)
              (let ((*print-readably* nil))
                (princ (error-message) *error-output*)
                (terpri *error-output*)
                (let ((connection (stream-of streamer-for-writing)))
                  (when (and (streamp connection) (open-stream-p connection))
                    (format *error-output* "~&;; Closing ~s due to error...~%"
                            connection)
                    (force-output *error-output*)
                    (setf (closed-of streamer-for-writing)
                          ':closed-due-to-errors)
                    (when streamer-queue
                      (format *error-output* 
                              "~&;; Terminating queued streamer ~s...~%"
                              streamer-queue)
                      (force-output *error-output*)
                      (setf *%%streamer-queues%%*
                            (delq streamer-acons *%%streamer-queues%%*)))
                    (close connection)
                    (error 'streamer-error 
                           :streamer streamer-for-writing)))))))))))
    
;;; ---------------------------------------------------------------------------

(defmacro %with-streamer-stream ((var streamer) &body body)
  ;; Internal-use macro establishing standard-streaming-synatx & lock
  ;; grabbing, if needed:
  `(flet ((body-form (.stream.)
            (let ((,var .stream.))
              ,@body)))
     (declare (dynamic-extent #'body-form))
     (%do-with-streamer-stream ,streamer #'body-form)))

;;; ---------------------------------------------------------------------------

(defun begin-queued-streaming (streamer tag write-empty-queue-p)
  (with-lock-held ((lock-of streamer))
    (force-output (stream-of streamer))
    (let* ((queue-stream (make-string-output-stream))
           (streamer-queue
            (make-streamer-queue
             :stream queue-stream
             :tag tag
             :write-empty-queue-p write-empty-queue-p)))
      (let ((*recorded-class-descriptions-ht* 
             (recorded-class-descriptions-ht-of streamer)))
        (with-standard-io-syntax 
          (setf *package* (package-of streamer))
          (setf *read-default-float-format* 
                (read-default-float-format-of streamer))
          (princ "#G!(:BB " queue-stream)
          (print-object-for-saving/sending tag queue-stream)
          (princ ") " queue-stream)))
      ;; Push the new streamer-queue for this streamer:
      (push-acons streamer streamer-queue *%%streamer-queues%%*)
      ;; Stash the tag-string:
      (setf (streamer-queue.tag-string streamer-queue)
            (get-output-stream-string queue-stream)))))

;;; ---------------------------------------------------------------------------

(defun no-streamer-queue-error (streamer)
  (error "Streamer ~s is not being queued." streamer))

;;; ---------------------------------------------------------------------------

(defun end-queued-streaming (streamer)
  (let* ((streamer-queue 
          (or (cdr (assq streamer *%%streamer-queues%%*))
              (no-streamer-queue-error streamer)))
         (queue-stream (streamer-queue.stream streamer-queue))
         (stream (stream-of streamer)))
    (let ((string (get-output-stream-string queue-stream)))
      ;; Leave evidence that this queue has ended:
      (setf (streamer-queue.stream streamer-queue) ':ended)
      (cond
       ;; Empty queue:
       ((zerop& (length string))
        (when (streamer-queue.write-empty-queue-p streamer-queue)
          (with-lock-held ((lock-of streamer))
            (write-sequence (streamer-queue.tag-string streamer-queue) stream)
            (princ "#G!(:EB) " stream)
            (force-output stream))))
       ;; Non-empty queue:
       (t (with-lock-held ((lock-of streamer))
            (write-sequence (streamer-queue.tag-string streamer-queue) stream)
            (write-sequence string stream)
            (princ "#G!(:EB) " stream)
            (force-output stream)))))))
      
;;; ---------------------------------------------------------------------------

(defmacro with-queued-streaming ((streamer &optional tag write-empty-queue-p)
                                 &body body)
  (with-once-only-bindings (streamer)
    `(let ((*%%streamer-queues%%* *%%streamer-queues%%*))
       (begin-queued-streaming ,streamer ,tag ,write-empty-queue-p)
       (unwind-protect (progn ,@body)
         (end-queued-streaming ,streamer)))))

;;; ---------------------------------------------------------------------------

(defun write-streamer-queue (streamer 
                             &key (tag nil tag-supplied-p)
                                  (write-empty-queue-p nil weqp-supplied-p))
  (let* ((streamer-queue 
          (or (cdr (assq streamer *%%streamer-queues%%*))
              (no-streamer-queue-error streamer)))
         (queue-stream (streamer-queue.stream streamer-queue))
         (stream (stream-of streamer)))
    ;; End the current queuing block:
    (let ((string (get-output-stream-string queue-stream)))
      (cond
       ;; Empty queue:
       ((zerop& (length string))
        (when (streamer-queue.write-empty-queue-p streamer-queue)
          (with-lock-held ((lock-of streamer))
            (write-sequence (streamer-queue.tag-string streamer-queue) stream)
            (princ "#G!(:EB) " stream)
            (force-output stream))))
       ;; Non-empty queue:
       (t (with-lock-held ((lock-of streamer))
            (write-sequence (streamer-queue.tag-string streamer-queue) stream)
            (write-sequence string stream)
            (princ "#G!(:EB) " stream)
            (force-output stream)))))
    ;; Setup tag & write-empty-queue-p values, using the previous tag and
    ;; write-empty-queue-p values if they weren't supplied:
    (if tag-supplied-p
        (setf (streamer-queue.tag streamer-queue) tag)
        (setf tag (streamer-queue.tag streamer-queue)))
    (if weqp-supplied-p
        (setf (streamer-queue.write-empty-queue-p streamer-queue) 
              write-empty-queue-p)
        (setf write-empty-queue-p 
              (streamer-queue.write-empty-queue-p streamer-queue)))
    ;; Begin a new queuing block:    
    (let ((*recorded-class-descriptions-ht* 
           (recorded-class-descriptions-ht-of streamer)))
      (with-standard-io-syntax 
        (setf *package* (package-of streamer))
        (setf *read-default-float-format* 
              (read-default-float-format-of streamer))
        (princ "#G!(:BB " queue-stream)
        (print-object-for-saving/sending tag queue-stream)
        (princ ") " queue-stream)))
      ;; Stash the tag-string:
      (setf (streamer-queue.tag-string streamer-queue)
            (get-output-stream-string queue-stream))))

;;; ---------------------------------------------------------------------------
;;;  Delete unit instance reader

(defmethod saved/sent-object-reader ((char (eql #\X)) stream)
  (destructuring-bind (class-name instance-name)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't)))
      (delete-instance instance))))

;;; ---------------------------------------------------------------------------
;;;  Unit-instance slot-update reader

(defmethod saved/sent-object-reader ((char (eql #\S)) stream)
  (destructuring-bind (class-name instance-name slot-name new-value)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't)))
      ;; TODO: CHECK FOR CHANGE TO A LINK SLOT OR MISSING SLOT...
      (let ((source-slot 
             (cdr (assq slot-name
                        (standard-unit-class.effective-dv-source-slots
                         (class-of instance))))))
        (if source-slot
            ;; TODO: Optimize this!
            (with-changing-dimension-values (instance)  
              (setf (slot-value instance slot-name) new-value))
            (setf (slot-value instance slot-name) new-value))))))
        
;;; ---------------------------------------------------------------------------
;;;  Unit-instance link reader

(defmethod saved/sent-object-reader ((char (eql #\+)) stream)
  (destructuring-bind (class-name instance-name slot-name other-instances)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't)))
      ;; TODO: CHECK FOR CHANGE TO A NON-LINK SLOT OR MISSING SLOT...
      (let ((source-slot 
             (cdr (assq slot-name
                        (standard-unit-class.effective-dv-source-slots
                         (class-of instance))))))
        (if source-slot
            ;; TODO: Optimize this!
            (with-changing-dimension-values (instance)  
              (linkf (slot-value instance slot-name) other-instances))
            (linkf (slot-value instance slot-name) other-instances))))))

;;; ---------------------------------------------------------------------------
;;;  Unit-instance unlink reader

(defmethod saved/sent-object-reader ((char (eql #\-)) stream)
  (destructuring-bind (class-name instance-name slot-name other-instances)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't)))
      ;; TODO: CHECK FOR CHANGE TO A NON-LINK SLOT OR MISSING SLOT...
      (let ((source-slot 
             (cdr (assq slot-name
                        (standard-unit-class.effective-dv-source-slots
                         (class-of instance))))))
        (if source-slot
            ;; TODO: Optimize this!
            (with-changing-dimension-values (instance)  
              (unlinkf (slot-value instance slot-name) other-instances))
            (unlinkf (slot-value instance slot-name) other-instances))))))

;;; ---------------------------------------------------------------------------
;;;  Add instance to space-instance reader

(defmethod saved/sent-object-reader ((char (eql #\a)) stream)
  (destructuring-bind (class-name instance-name 
                       space-instance-class-name space-instance-name)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (setf space-instance-class-name
          (possibly-translate-class-name space-instance-class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't))
          (space-instance (find-instance-by-name 
                           space-instance-name space-instance-class-name 't)))
      (add-instance-to-space-instance instance space-instance))))
        
;;; ---------------------------------------------------------------------------
;;;  Remove instance from space-instance reader

(defmethod saved/sent-object-reader ((char (eql #\r)) stream)
  (destructuring-bind (class-name instance-name 
                       space-instance-class-name space-instance-name)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (setf space-instance-class-name
          (possibly-translate-class-name space-instance-class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't))
          (space-instance (find-instance-by-name 
                           space-instance-name space-instance-class-name 't)))
      (remove-instance-from-space-instance instance space-instance))))
        
;;; ---------------------------------------------------------------------------
;;;  Command form reader

(defmethod saved/sent-object-reader ((char (eql #\.)) stream)
  (destructuring-bind (form)
      (read stream t nil 't)
    (if (consp form)
        (apply #'handle-streamed-command-form form)
        (handle-streamed-command-atom form))))
        
;;; ---------------------------------------------------------------------------
;;;  Command form methods

(defgeneric handle-streamed-command-form (command &rest args))
(defgeneric handle-streamed-command-atom (command))

;; Default error methods:
(defmethod handle-streamed-command-form (command &rest args)
  (error "Unhandled streamed command: ~s" (cons command args)))

(defmethod handle-streamed-command-atom (command)
  (error "Unhandled streamed command: ~s" command))
         
;;; ===========================================================================
;;;   Network Streaming Server

(defparameter *network-stream-format-version* 1)
(defvar *network-stream-connection-server-thread* nil)
(defvar *break-on-receive-errors* nil)
(defvar *remove-mirroring-when-streamer-closes* 't)
(defvar *queued-read-tag*)

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

(defun network-stream-server-running-p ()
  (and *network-stream-connection-server-thread*
       (thread-alive-p *network-stream-connection-server-thread*)))

;;; ---------------------------------------------------------------------------

(defun start-network-stream-server (streamer-node)
  (unless (typep streamer-node 'streamer-node)
    (setf streamer-node (find-streamer-node streamer-node 't)))
  (setf *network-stream-connection-server-thread*
        (start-connection-server 'network-stream-connection-server
                                 (port-of streamer-node)
                                 :name "GBBopen Network Connection Server"
                                 :keepalive 't
                                 :reuse-address 't)))

;;; ---------------------------------------------------------------------------

(defun kill-network-stream-server ()
  (when *network-stream-connection-server-thread*
    (kill-thread *network-stream-connection-server-thread*)
    ;; indicate success:
    't))

;;; ---------------------------------------------------------------------------
;;;  Queued block methods

(defgeneric beginning-queued-read (tag))
(defgeneric ending-queued-read (tag))

;; Default do-nothing methods:
(defmethod beginning-queued-read (tag)
  tag)
(defmethod ending-queued-read (tag)
  tag)
         
;;; ---------------------------------------------------------------------------
;;;  GBBopen streamer-command reader
         
(defmethod saved/sent-object-reader ((char (eql #\!)) stream)
  (let ((form (read stream 't nil 't)))
    (case (first form)
      (:bb (beginning-queued-read (setf *queued-read-tag* (second form))))
      (:eb (ending-queued-read *queued-read-tag*))
      (otherwise (printv form)))))

;;; ===========================================================================
;;;   Senders

(defun stream-instance (instance streamer)
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (print-object-for-saving/sending instance stream))))

;;; ---------------------------------------------------------------------------

(defun stream-instances (instances streamer)
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (dolist (instance instances)
        (print-object-for-saving/sending instance stream)))))

;;; ---------------------------------------------------------------------------

(defun stream-instances-of-class (unit-class-specifier streamer)
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (do-instances-of-class (instance unit-class-specifier)
        (print-object-for-saving/sending instance stream)))))

;;; ---------------------------------------------------------------------------

(defun stream-instances-on-space-instances (unit-class-specifier
                                            space-instances 
                                            streamer
                                            &key (pattern ':all)
                                                 filter-before filter-after
                                                 (use-marking *use-marking*)
                                                 (verbose *find-verbose*))
  (%with-streamer-stream (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (do-instances-on-space-instances (instance unit-class-specifier
                                                 space-instances
                                                 :pattern pattern
                                                 :filter-before filter-before
                                                 :filter-after filter-after
                                                 :use-marking use-marking
                                                 :verbose verbose)
        (print-object-for-saving/sending instance stream)))))

;;; ---------------------------------------------------------------------------

(defun stream-delete-instance (instance streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "#GX(~s "
            (if (typep instance 'deleted-unit-instance)
                (class-name (original-class-of instance))
                (type-of instance)))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-slot-update (instance slot/slot-name new-value streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "#GS(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (if (symbolp slot/slot-name)
                              slot/slot-name
                              (slot-definition-name slot/slot-name)))
    (print-object-for-saving/sending new-value stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-link (instance slot/slot-name other-instances streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "#G+(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (if (symbolp slot/slot-name)
                              slot/slot-name
                              (slot-definition-name slot/slot-name)))
    (print-object-for-saving/sending other-instances stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-unlink (instance slot/slot-name other-instances streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "#G-(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (if (symbolp slot/slot-name)
                              slot/slot-name
                              (slot-definition-name slot/slot-name)))
    (print-object-for-saving/sending other-instances stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-add-to-space (instance space-instance streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "#Ga(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (type-of space-instance))
    (print-object-for-saving/sending (instance-name-of space-instance) stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-remove-from-space (instance space-instance streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "#Gr(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (type-of space-instance))
    (print-object-for-saving/sending (instance-name-of space-instance) stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-command-form (form streamer)
  (%with-streamer-stream (stream streamer)
    (format stream "#G.(")
    (print-object-for-saving/sending form stream)
    (princ ")" stream)))

;;; ===========================================================================
;;;   Mirroring

(defvar *%%mirroring-enabled%%* 't)

;;; ---------------------------------------------------------------------------
;;;   Mirroring disable/enable macros

(defmacro with-mirroring-disabled ((&key) &body body)
  ;;; Disables mirroring during execution of `body'
  `(let ((*%%mirroring-enabled%%* nil))
     ,@body))

;;; ---------------------------------------------------------------------------

(defmacro with-mirroring-enabled ((&key) &body body)
  ;;; Enables mirroring during execution of `body'
  `(let ((*%%mirroring-enabled%%* 't))
     ,@body))

;;; ---------------------------------------------------------------------------
;;;   Mirroring setup

(defun add-mirroring (streamer unit-class-spec &optional (slot-names 't))
  (add-event-function streamer '(instance-created-event +) unit-class-spec)
  (add-event-function streamer '(instance-deleted-event +) unit-class-spec)
  (add-event-function streamer '(nonlink-slot-updated-event +) unit-class-spec)
  (add-event-function streamer '(link-event +) unit-class-spec 
                      :slot-names slot-names)
  (add-event-function streamer '(unlink-event +) unit-class-spec
                      :slot-names slot-names)
  (add-event-function streamer '(instance-added-to-space-instance-event +)
                      unit-class-spec)
  (add-event-function streamer '(instance-removed-from-space-instance-event +)
                      unit-class-spec))

;;; ---------------------------------------------------------------------------

(defun remove-mirroring (streamer unit-class-spec &optional (slot-names 't))
  (remove-event-function streamer '(instance-created-event +) unit-class-spec)
  (remove-event-function streamer '(instance-deleted-event +) unit-class-spec)
  (remove-event-function streamer '(nonlink-slot-updated-event +) unit-class-spec)
  (remove-event-function streamer '(link-event +) unit-class-spec
                         :slot-names slot-names)
  (remove-event-function streamer '(unlink-event +) unit-class-spec
                         :slot-names slot-names)
  (remove-event-function streamer '(instance-added-to-space-instance-event +)
                         unit-class-spec)
  (remove-event-function streamer '(instance-removed-from-space-instance-event +)
                         unit-class-spec))

;;; ---------------------------------------------------------------------------

(defun do-instance-mirroring (evstreamers event-class 
                              &key instance &allow-other-keys)
  (when *%%mirroring-enabled%%*
    ;; TODO: Deal with subevents in ecase:
    (ecase (class-name event-class)
      (instance-created-event
       (dolist (evstreamer evstreamers)
         (stream-instance instance (evstreamer.streamer evstreamer))))
      (instance-deleted-event
       (dolist (evstreamer evstreamers)
         (stream-delete-instance instance (evstreamer.streamer evstreamer)))))))

;;; ---------------------------------------------------------------------------

(defun do-space-instance-mirroring (evstreamers event-class 
                                    &key instance space-instance
                                         initialization
                                    &allow-other-keys)
  (when (and (not initialization) *%%mirroring-enabled%%*)
    ;; TODO: Deal with subevents in ecase:
    (ecase (class-name event-class)
      (instance-added-to-space-instance-event
       (dolist (evstreamer evstreamers)
         (stream-add-to-space
          instance space-instance (evstreamer.streamer evstreamer))))
      (instance-removed-from-space-instance-event
       (dolist (evstreamer evstreamers)
         (stream-remove-from-space
          instance space-instance (evstreamer.streamer evstreamer)))))))

;;; ---------------------------------------------------------------------------

(defun do-nonlink-slot-mirroring (evstreamers event-class 
                                  &key instance slot current-value
                                       initialization
                                  &allow-other-keys)
  (declare (ignore event-class))
  (when (and (not initialization) *%%mirroring-enabled%%*)
    (dolist (evstreamer evstreamers)
      (stream-slot-update 
       instance slot current-value (evstreamer.streamer evstreamer)))))

;;; ---------------------------------------------------------------------------

(defun do-link-slot-mirroring (evstreamers event-class 
                               &key instance slot added-instances
                                    removed-instances initialization
                               &allow-other-keys)
  (when (and (not initialization) *%%mirroring-enabled%%*)
    ;; TODO: Deal with subevents in ecase:
    (ecase (class-name event-class)
      (link-event
       (dolist (evstreamer evstreamers)
         (stream-link 
          instance slot added-instances (evstreamer.streamer evstreamer))))
      (unlink-event
       (dolist (evstreamer evstreamers)
         (stream-unlink
          instance slot removed-instances (evstreamer.streamer evstreamer)))))))

;;; ---------------------------------------------------------------------------

(defun describe-mirroring (&optional (event-classes-spec 't)
                           &rest args)
  ;;; Prints streamers for the specified event signature(s).
  (declare (dynamic-extent args))
  (multiple-value-bind (unit-class-spec slot-names paths)
      (parse-event-function-args args)
    (multiple-value-bind (unit-class/instance plus-subclasses)
        (parse-unit-class/instance-specifier unit-class-spec)
      (flet ((fn (event-class plus-subevents) 
               (ds-evfn-using-class 'describe-mirroring
                                    event-class plus-subevents 
                                    unit-class/instance plus-subclasses
                                    slot-names paths)))
        (declare (dynamic-extent #'fn))
        (map-extended-event-classes #'fn event-classes-spec))))
  (values))

;;; ===========================================================================
;;;   Journal entities

(defun make-jnl-pathname (pathname) 
  ;; Adds type "jnl", if not supplied ; then adds defaults from
  ;; (user-homedir-pathname), as needed:
  (merge-pathnames 
   pathname
   (make-pathname :type "jnl"
                  :defaults (user-homedir-pathname))))

;;; ---------------------------------------------------------------------------

(defun make-journal-streamer (pathname
                              &rest initargs
                              &key (if-exists ':supersede)                   
                                   (package ':cl-user)
                                   (external-format ':default)
                                   (read-default-float-format 
                                    *read-default-float-format*)
                                   (streamer-class 'journal-streamer))
  (let ((stream (if (streamp pathname)
                    pathname
                    (open (make-bb-pathname pathname)
                          ;; TODO: ** Deal with appended journaling **
                          :direction ':output
                          :if-exists if-exists
                          :external-format external-format
                          #+clozure :sharing #+clozure ':external)))
        (*package* (ensure-package package))
        (*read-default-float-format* read-default-float-format))
    (write-saving/sending-block-info stream)
    ;; Why is this needed to prevent reading problems...?
    (princ " " stream)
    (force-output stream)
    ;; Make and return the streamer:
    (apply #'make-instance
           streamer-class
           :lock (make-lock :name "Journal streamer lock")
           :package *package*
           :external-format external-format
           :read-default-float-format read-default-float-format 
           :stream stream
           (remove-properties initargs
                              '(:if-exists :package :streamer-class)))))

;;; ---------------------------------------------------------------------------

(defun load-journal (pathname
                     &key (class-name-translations nil)
                          (coalesce-strings nil)
                          (estimated-peak-forward-references 
                           *default-estimated-peak-forward-references*)
                          (external-format ':default)
                          (readtable *reading-saved/sent-objects-readtable*)
                          (read-eval nil))
  (with-open-file (stream (make-jnl-pathname pathname)
                   :direction ':input
                   :external-format external-format)
    (with-reading-saved/sent-objects-block 
        (stream :class-name-translations class-name-translations
                :coalesce-strings coalesce-strings
                :estimated-peak-forward-references 
                  estimated-peak-forward-references
                :readtable readtable
                :read-eval read-eval)
    (with-blackboard-repository-locked ()
      ;; Read everything:
      (let ((eof-marker '#:eof))
        (until (eq eof-marker (read stream nil eof-marker)))))
      ;; Return the pathname, saved/sent-time, and saved/sent-value:
      (values (pathname stream)
              *block-saved/sent-time* 
              *block-saved/sent-value*))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
