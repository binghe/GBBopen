;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/extensions/send-receive.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Feb 11 13:08:48 2011 *-*
;;;; *-* Machine: twister.local *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *               GBBopen Send/Receive & Journaling Entities
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
            *gbbopen-network-server-port*
            add-mirroring
            begin-queued-streaming
            beginning-queued-receive
            end-queued-streaming
            ending-queued-receive
            gbbopen-network-server-running-p
            handle-stream-connection-exiting
            handle-streamed-command-atom
            handle-streamed-command-form
            kill-gbbopen-network-server
            make-gbbopen-network-streamer
            make-streamer
            start-gbbopen-network-server
            stream-command-form
            stream-delete-instance
            stream-instance
            stream-instances
            stream-link
            stream-slot-update
            stream-unlink
	    with-mirroring-disabled
	    with-mirroring-enabled
            with-queued-streaming
            with-streamer)))

;;; ---------------------------------------------------------------------------

(defstruct (streamer
            (:copier nil)
            (:constructor %make-streamer))
  lock
  name
  package
  read-default-float-format 
  (recorded-class-descriptions-ht
   (make-hash-table :test 'eq))
  stream
  queue-stream)

;;; ---------------------------------------------------------------------------

(defmacro with-streamer ((var streamer) &body body)
  (with-once-only-bindings (streamer)
    `(with-standard-io-syntax 
       (let ((*recorded-class-descriptions-ht* 
              (streamer-recorded-class-descriptions-ht ,streamer)))
         (setf *package* (streamer-package ,streamer))
         (setf *read-default-float-format* 
               (streamer-read-default-float-format ,streamer))
         (with-lock-held ((streamer-lock ,streamer))
           (let* ((.streamer-queue-stream. (streamer-queue-stream ,streamer))
                  (,var (or .streamer-queue-stream. (streamer-stream ,streamer))))
             ,@body
             (unless .streamer-queue-stream.
               (force-output ,var))))))))

;;; ---------------------------------------------------------------------------

(defun make-streamer (stream &key (name "Streamer")
                                  (package ':cl-user)
                                  (read-default-float-format 'single-float))
  (let ((streamer
         (%make-streamer
          :name name
          :lock (make-lock :name (concatenate 'simple-string name " lock"))
          :package (if (packagep package)
                       package
                       (ensure-package package))
          :read-default-float-format read-default-float-format 
          :stream stream)))
    streamer))

;;; ---------------------------------------------------------------------------

(defun begin-queued-streaming (streamer &optional tag (errorp t))
  (let ((already-queuing? nil))
    (with-lock-held ((streamer-lock streamer))
      (force-output (streamer-stream streamer))
      (setf already-queuing? (streamer-queue-stream streamer))
      (unless already-queuing?
        (let ((queue-stream (make-string-output-stream)))
          (setf (streamer-queue-stream streamer) queue-stream)
          (with-standard-io-syntax 
            (let ((*recorded-class-descriptions-ht* 
                   (streamer-recorded-class-descriptions-ht streamer)))
              (setf *package* (streamer-package streamer))
              (setf *read-default-float-format* 
                    (streamer-read-default-float-format streamer))
              (princ "#G!(:BB " queue-stream)
              (print-object-for-saving/sending tag queue-stream)
              (princ ") " queue-stream))))))
    (when (and already-queuing? errorp)
      (error "Streamer ~s is already queuing." streamer))))

;;; ---------------------------------------------------------------------------

(defun end-queued-streaming (streamer &optional (errorp t))
  (let ((not-queuing? nil))
    (with-lock-held ((streamer-lock streamer))
      (let ((queue-stream (streamer-queue-stream streamer)))
        (cond
         (queue-stream
          (setf (streamer-queue-stream streamer) nil)
          (princ "#G!(:EB) " queue-stream)
          (let ((string (get-output-stream-string queue-stream))
                (stream (streamer-stream streamer)))
            (write-sequence string stream)
            (force-output stream)))
         (t (setf not-queuing? (not queue-stream))))))
    (when (and not-queuing? errorp)
      (error "Streamer ~s is not queuing." streamer))))
      
;;; ---------------------------------------------------------------------------

(defmacro with-queued-streaming ((streamer &optional tag (errorp t)) 
                                 &body body)
  (with-once-only-bindings (streamer tag errorp)
    `(progn (begin-queued-streaming ,streamer ,tag ,errorp)
            (unwind-protect (progn ,@body)
              (end-queued-streaming ,streamer ,errorp)))))

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
      ;; TODO: LINK SLOTS, MISSING SLOTS
      (setf (slot-value instance slot-name) new-value))))
        
;;; ---------------------------------------------------------------------------
;;;  Unit-instance link reader

(defmethod saved/sent-object-reader ((char (eql #\+)) stream)
  (destructuring-bind (class-name instance-name slot-name other-instances)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't)))
      ;; TODO: MISSING SLOTS
      (linkf (slot-value instance slot-name) other-instances))))
        
;;; ---------------------------------------------------------------------------
;;;  Unit-instance unlink reader

(defmethod saved/sent-object-reader ((char (eql #\-)) stream)
  (destructuring-bind (class-name instance-name slot-name other-instances)
      (read stream t nil 't)
    (setf class-name (possibly-translate-class-name class-name))
    (let ((instance (find-instance-by-name instance-name class-name 't)))
      ;; TODO: MISSING SLOTS
      (unlinkf (slot-value instance slot-name) other-instances))))
        
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
;;;   Network Streamin Server

(defvar *gbbopen-network-server-port* 1968)
(defparameter *gbbopen-network-format-version* 1)
(defvar *gbbopen-network-connection-server-thread* nil)
(defvar *break-on-receive-errors* nil)
(defvar *queued-receive-tag*)

;;; ---------------------------------------------------------------------------

(defun safe-read (connection)
  (with-error-handling (read connection nil ':eof)
    (format t "~&;; Read error occurred: ~a~%" (error-message))
    ':error))

;;; ---------------------------------------------------------------------------

(defun make-gbbopen-network-streamer
    (host &key (port *gbbopen-network-server-port*)
               passphrase
               ;; For the created streamer:
               (name "GBBopen Network Streamer")
               (package ':cl-user)
               (read-default-float-format 'single-float))
  ;; Convert package to package name, if needed:
  (when (packagep package)
    (setf package (package-name package)))
  ;; Try to connect to GBBopen Network Server:
  (let ((connection (open-connection host port)))
    (format connection "(:gbbopen ~s ~s ~s)"
            *gbbopen-network-format-version*
            passphrase
            name)
    (let ((*package* (ensure-package package))
          (*read-default-float-format* read-default-float-format))
      (write-saving/sending-block-info connection))
    ;; Why is this needed to prevent reading problems...?
    (princ " " connection)
    (force-output connection)
    ;; If connection is established, make and return the streamer:
    (when connection
      (make-streamer 
       connection
       :name name
       :package package
       :read-default-float-format read-default-float-format))))
          
;;; ---------------------------------------------------------------------------
;;;  Connection exiting methods

(defgeneric handle-stream-connection-exiting (connection exit-status))

;; Default handler method:
(defmethod handle-stream-connection-exiting (connection exit-status)
  (format t "~&;; Network stream connection ~s closing~@[: (~s)~]"
          connection exit-status))

;;; ---------------------------------------------------------------------------

(defun client-loop (connection)
  (let ((maximum-contiguous-errors 4)
        (contiguous-errors 0)
        *queued-receive-tag*
        form)
    (loop
      (setf form 
            (if *break-on-receive-errors*
                (read connection)
                (safe-read connection)))
      (case form
        (:eof (return))
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

(defun validate-passcode (passcode connection)
  (declare (ignore connection))
  ;; Good enough for now!
  (eq passcode nil))

;;; ---------------------------------------------------------------------------

(defun gbbopen-client-connection (connection)
  (let ((authentication-form (safe-read connection)))
    (when (and (consp authentication-form)
               (=& (length authentication-form) 4))
      (destructuring-bind (client version passcode name)
          authentication-form
        (declare (ignore name))
        (when (and (eq client ':gbbopen)
                   (eql version 1)
                   (validate-passcode passcode connection))
          (with-reading-saved/sent-objects-block (connection)
            (let ((exit-status ':error))
              (unwind-protect 
                  (setf exit-status (client-loop connection))
                (handle-stream-connection-exiting connection exit-status)
                (close connection)))))))))

;;; ---------------------------------------------------------------------------

(defun gbbopen-network-connection-server (connection) 
  (flet ((connect-it (connection)
           (unwind-protect (gbbopen-client-connection connection)
             (close connection))))
    (declare (dynamic-extent #'connect-it))
    (spawn-thread "Client GBBopen Connection" #'connect-it connection)))

;;; ---------------------------------------------------------------------------

(defun gbbopen-network-server-running-p ()
  (and *gbbopen-network-connection-server-thread*
       (thread-alive-p *gbbopen-network-connection-server-thread*)))

;;; ---------------------------------------------------------------------------

(defun start-gbbopen-network-server (&optional
                                     (port *gbbopen-network-server-port*))
  (setf *gbbopen-network-connection-server-thread*
        (start-connection-server 'gbbopen-network-connection-server
                                 port
                                 :name "GBBopen Network Connection Server"
                                 :reuse-address 't)))

;;; ---------------------------------------------------------------------------

(defun kill-gbbopen-network-server ()
  (when *gbbopen-network-connection-server-thread*
    (kill-thread *gbbopen-network-connection-server-thread*)
    ;; indicate success:
    't))

;;; ---------------------------------------------------------------------------
;;;  Queued block methods

(defgeneric beginning-queued-receive (tag))
(defgeneric ending-queued-receive (tag))

;; Default do-nothing methods:
(defmethod beginning-queued-receive (tag)
  tag)
(defmethod ending-queued-receive (tag)
  tag)
         
;;; ---------------------------------------------------------------------------
;;;  GBBopen streamer-command reader
         
(defmethod saved/sent-object-reader ((char (eql #\!)) stream)
  (let ((form (read stream 't nil 't)))
    (case (first form)
      (:bb (beginning-queued-receive (setf *queued-receive-tag* (second form))))
      (:eb (ending-queued-receive *queued-receive-tag*))
      (otherwise (printv form)))))

;;; ===========================================================================
;;;   Senders

(defun stream-instance (instance streamer)
  (with-streamer (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (print-object-for-saving/sending instance stream))))

;;; ---------------------------------------------------------------------------

(defun stream-instances (instances streamer)
  (with-streamer (stream streamer)
    (let ((*save/send-references-only* nil)) 
      (dolist (instance instances)
        (print-object-for-saving/sending instance stream)))))

;;; ---------------------------------------------------------------------------

(defun stream-delete-instance (instance streamer)
  (with-streamer (stream streamer)
    (format stream "#GX(~s "
            (if (typep instance 'deleted-unit-instance)
                (class-name (original-class-of instance))
                (type-of instance)))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-slot-update (instance slot/slot-name new-value streamer)
  (with-streamer (stream streamer)
    (format stream "#GS(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (if (symbolp slot/slot-name)
                              slot/slot-name
                              (slot-definition-name slot/slot-name)))
    (print-object-for-saving/sending new-value stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-link (instance slot/slot-name other-instances streamer)
  (with-streamer (stream streamer)
    (format stream "#G+(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (if (symbolp slot/slot-name)
                              slot/slot-name
                              (slot-definition-name slot/slot-name)))
    (print-object-for-saving/sending other-instances stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-unlink (instance slot/slot-name other-instances streamer)
  (with-streamer (stream streamer)
    (format stream "#G-(~s " (type-of instance))
    (print-object-for-saving/sending (instance-name-of instance) stream)
    (format stream " ~s " (if (symbolp slot/slot-name)
                              slot/slot-name
                              (slot-definition-name slot/slot-name)))
    (print-object-for-saving/sending other-instances stream)
    (princ ")" stream)))

;;; ---------------------------------------------------------------------------

(defun stream-command-form (form streamer)
  (with-streamer (stream streamer)
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
;;;   Mirroing setup

(defun add-mirroring (streamer unit-class-spec &optional (slots 't))
  ;; Instance creation:
  (add-event-function
   #'(lambda (event-name &key instance &allow-other-keys)
       (declare (ignore event-name))
       (when *%%mirroring-enabled%%*
         (stream-instance instance streamer)))
   'create-instance-event
   unit-class-spec)
  ;; Instance deletion:
  (add-event-function
   #'(lambda (event-name &key instance &allow-other-keys)
       (declare (ignore event-name))
       (when *%%mirroring-enabled%%*
         (stream-delete-instance instance streamer)))
   'instance-deleted-event
   unit-class-spec)
  ;; Add/remove-instance-to/from-space [NEEDED]:  
  ;; Slots:
  (cond
   ((eq slots 't)
    ;; Nonlink-slot-updates
    (add-event-function
     #'(lambda (event-name &key instance slot current-value initialization
                &allow-other-keys)
         (declare (ignore event-name))
         (when (and (not initialization) *%%mirroring-enabled%%*)
           (stream-slot-update instance slot current-value streamer)))
     'update-nonlink-slot-event
     unit-class-spec)
    ;; added-links
    (add-event-function
     #'(lambda (event-name &key instance slot added-instances initialization
                &allow-other-keys)
         (declare (ignore event-name))
         (when (and (not initialization) *%%mirroring-enabled%%*)
           (stream-link instance slot added-instances streamer)))
     'link-event
     unit-class-spec)
    ;; removed-links
    (add-event-function
     #'(lambda (event-name &key instance slot removed-instances initialization
                &allow-other-keys)
         (declare (ignore event-name))
         (when (and (not initialization) *%%mirroring-enabled%%*)
           (stream-unlink instance slot removed-instances streamer)))
     'unlink-event
     unit-class-spec))
   ;; STILL TO DO: specified slots/excluded slots
   (t (nyi))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
