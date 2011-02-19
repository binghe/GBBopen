;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/extensions/streaming.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sat Feb 19 17:17:40 2011 *-*
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
            add-mirroring
            beginning-queued-read
            define-streamer-node
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
            make-journal-streamer
            name-of
            network-stream-receiver
            network-stream-server-running-p
            network-streamer            ; class-name
            port-of
            start-network-stream-server
            stream-command-form
            stream-delete-instance
            stream-instance
            stream-instances
            stream-link
            stream-slot-update
            stream-unlink
            streamer                    ; class-name
            streamer-node               ; class-name
	    with-mirroring-disabled
	    with-mirroring-enabled
            with-queued-streaming)))

;;; ---------------------------------------------------------------------------

(defvar *default-network-stream-server-port* 1968)

;;; ===========================================================================
;;;   Streamer Node

(defvar *local-streamer-node* nil)
(defvar *streamer-nodes-ht* (make-hash-table :test 'equal))

;;; ---------------------------------------------------------------------------

(define-class streamer-node (standard-gbbopen-instance)
  (name
   localnodep
   host
   port
   passphrase
   package
   external-format
   read-default-float-format 
   authorized-nodes
   (streamer-class :initform 'network-streamer)
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
                                  (localnodep nil)
                                  (passphrase nil)
                                  (package ':cl-user)
                                  (external-format ':default)                                  
                                  (read-default-float-format 
                                   *read-default-float-format*)
                                  (authorized-nodes ':all))
  (let ((streamer-node
         (make-instance 'streamer-node
           :name name
           :localnodep localnodep
           :host host
           :port port
           :passphrase passphrase
           :package package
           :external-format external-format
           :read-default-float-format read-default-float-format 
           :authorized-nodes authorized-nodes)))
    (setf (gethash name *streamer-nodes-ht*) streamer-node)
    (if localnodep (setf *local-streamer-node* streamer-node))))

;;; ---------------------------------------------------------------------------

(defun find-streamer-node (name &optional errorp)
  (or (gethash name *streamer-nodes-ht*)
      (when errorp
        (error "No streamer node named  ~s" name))))

;;; ===========================================================================
;;;   Streamer queues

(define-class streamer-queue (standard-gbbopen-instance)
  (streamer-lock
   (streamer :initform nil)
   (queue-stream :initform nil)
   tag))

;;; ===========================================================================
;;;   Streamers

(define-class streamer (standard-gbbopen-instance)
  (streamer-lock
   (streamer-stream :initform nil)
   streamer-package
   external-format
   read-default-float-format 
   (recorded-class-descriptions-ht
    :initform (make-hash-table :test 'eq))))

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
;;;   Streamer entities

(defvar *%%streamer-queues%%* nil)      ; records thread-local streamer queues

;;; ---------------------------------------------------------------------------

(defun %do-with-streamer-stream (streamer body-form-fn)
  (let ((streamer-queue (cdr (assq streamer *%%streamer-queues%%*))))
    (with-standard-io-syntax 
      (setf *package* (streamer-package-of streamer))
      (setf *read-default-float-format* 
            (read-default-float-format-of streamer))
      (let ((*recorded-class-descriptions-ht* 
             (recorded-class-descriptions-ht-of streamer)))
        (if streamer-queue
            ;; queued streaming:
            (funcall body-form-fn (queue-stream-of streamer-queue))
            ;; regular streaming:
            (with-lock-held ((streamer-lock-of streamer))
              (let ((stream (streamer-stream-of streamer)))
                (funcall body-form-fn stream)
                (force-output stream))))))))
    
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

(defun %begin-queued-streaming (streamer tag)
  (with-lock-held ((streamer-lock-of streamer))
    (force-output (streamer-stream-of streamer))
    (let* ((queue-stream (make-string-output-stream))
           (streamer-queue
            (make-instance 'streamer-queue
              :streamer streamer
              :queue-stream queue-stream
              :tag tag)))
      (let ((*recorded-class-descriptions-ht* 
             (recorded-class-descriptions-ht-of streamer)))
        (with-standard-io-syntax 
          (setf *package* (streamer-package-of streamer))
          (setf *read-default-float-format* 
                (read-default-float-format-of streamer))
          (princ "#G!(:BB " queue-stream)
          (print-object-for-saving/sending tag queue-stream)
          (princ ") " queue-stream)))
      ;; Push the new streamer-queue for this streamer:
      (push-acons streamer streamer-queue *%%streamer-queues%%*)
      ;; Return the tag-string:
      (get-output-stream-string queue-stream))))

;;; ---------------------------------------------------------------------------

(defun %end-queued-streaming (streamer tag-string write-empty-queue-p)
  (let* ((streamer-queue (cdr (assq streamer *%%streamer-queues%%*)))
         (queue-stream (queue-stream-of streamer-queue))
         (stream (streamer-stream-of streamer)))
    (let ((string (get-output-stream-string queue-stream)))
      ;; Leave evidence that this queue has ended:
      (setf (queue-stream-of streamer-queue) ':ended)
      (cond
       ;; Empty queue:
       ((zerop& (length string))
        (when write-empty-queue-p
          (with-lock-held ((streamer-lock-of streamer))
            (write-sequence tag-string stream)
            (princ "#G!(:EB) " stream)
            (force-output stream))))
       ;; Non-empty queue:
       (t (with-lock-held ((streamer-lock-of streamer))
            (write-sequence tag-string stream)
            (write-sequence string stream)
            (princ "#G!(:EB) " stream)
            (force-output stream)))))
    ;; Leave more evidence that this queue has ended:
    (setf (streamer-of streamer-queue) ':ended)))
      
;;; ---------------------------------------------------------------------------

(defmacro with-queued-streaming ((streamer &optional tag write-empty-queue-p)
                                 &body body)
  (with-gensyms (tag-string)
    (with-once-only-bindings (streamer tag write-empty-queue-p)
      `(let* ((*%%streamer-queues%%* *%%streamer-queues%%*)
              (,tag-string 
               (%begin-queued-streaming ,streamer ,tag)))
         (unwind-protect (progn ,@body)
           (%end-queued-streaming
            ,streamer ,tag-string ,write-empty-queue-p))))))

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
;;;   Network Streaming Server

(defparameter *network-stream-format-version* 1)
(defvar *network-stream-connection-server-thread* nil)
(defvar *break-on-receive-errors* nil)
(defvar *queued-read-tag*)

;;; ---------------------------------------------------------------------------

(defun safe-read (connection)
  (with-error-handling (read connection nil ':eof)
    (format t "~&;; Read error occurred: ~a~%" (error-message))
    ':error))

;;; ---------------------------------------------------------------------------

(defun find-or-make-network-streamer (streamer-node &rest initargs)
  (unless *local-streamer-node*
    (error "No local streamer node has been defined."))
  ;; Lookup streamer node, if needed:
  (unless (typep streamer-node 'streamer-node)
    (setf streamer-node (find-streamer-node streamer-node 't)))
  (let ((streamer (streamer-of streamer-node)))
    (or 
      ;; A streamer already exists, return it:
     streamer
     ;; A new streamer is needed; try to connect to GBBopen Network Server:
     (let ((connection 
            ;; TODO: ** Extend open-connection to accept external-format **
            (open-connection (host-of streamer-node) (port-of streamer-node)))
           (package (ensure-package (package-of streamer-node)))
           (external-format (external-format-of streamer-node))
           (read-default-float-format 
            (read-default-float-format-of streamer-node)))
       (format connection "(:gbbopen ~s ~s ~s)"
               *network-stream-format-version*
               (passphrase-of streamer-node)
               (name-of *local-streamer-node*))
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
                      :streamer-lock (make-recursive-lock 
                                      :name (concatenate 'simple-string 
                                              (name-of streamer-node) 
                                              " lock"))
                      :streamer-package package
                      :external-format external-format
                      :read-default-float-format read-default-float-format 
                      :streamer-stream connection
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
  (let ((maximum-contiguous-errors 4)
        (contiguous-errors 0)
        *queued-read-tag*
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

(defun start-streaming-connection-endpoint (streamer-node connection 
                                            skip-block-info-reading)
  (with-reading-saved/sent-objects-block 
      (connection :%%skip-block-info-reading%% skip-block-info-reading)
    (let ((exit-status ':error)
          (network-streamer (streamer-of streamer-node)))
      (if network-streamer
          (unwind-protect 
              (setf exit-status
                    (network-stream-receiver network-streamer connection))
            (setf (streamer-of streamer-node) nil)
            (handle-stream-connection-exiting network-streamer exit-status))
          (error "Missing network-streamer at ~s" streamer-node)))
    (close connection)))

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
                         :streamer-lock (make-recursive-lock 
                                         :name (concatenate 'simple-string 
                                                 (name-of streamer-node) 
                                                 " lock"))
                         :streamer-package (package-of streamer-node)
                         :external-format (external-format-of streamer-node)
                         :read-default-float-format (read-default-float-format-of 
                                                     streamer-node)
                         :streamer-stream connection
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
             (close connection))))
    (spawn-thread "Client GBBopen Connection" #'connect-it connection)))

;;; ---------------------------------------------------------------------------

(defun network-stream-server-running-p ()
  (and *network-stream-connection-server-thread*
       (thread-alive-p *network-stream-connection-server-thread*)))

;;; ---------------------------------------------------------------------------

(defun start-network-stream-server ()
  (unless *local-streamer-node*
    (error "No local streamer node has been defined."))
  (setf *network-stream-connection-server-thread*
        (start-connection-server 'network-stream-connection-server
                                 (port-of *local-streamer-node*)
                                 :name "GBBopen Network Connection Server"
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

(defvar *mirroring-evfns-ht* (make-hash-table :test 'eq))

;;; ---------------------------------------------------------------------------

(defstruct (mirroring-evfns
            (:copier nil))
  create-instance
  deleted-instance
  nonlink-slot-update
  added-links
  removed-links
  add-to-space
  remove-from-space)

;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------

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
  (let ((stream (open (make-bb-pathname pathname)
                      ;; TODO: ** Deal with appended journaling **
                      :direction ':output
                      :if-exists if-exists
                      :external-format external-format))
        (*package* (ensure-package package))
        (*read-default-float-format* read-default-float-format))
    (write-saving/sending-block-info stream)
    ;; Why is this needed to prevent reading problems...?
    (princ " " stream)
    (force-output stream)
    ;; Make and return the streamer:
    (apply #'make-instance
           streamer-class
           :streamer-lock (make-recursive-lock 
                           :name (concatenate 'simple-string 
                                   (enough-namestring pathname)
                                   " lock"))
           :streamer-package *package*
           :external-format external-format
           :read-default-float-format read-default-float-format 
           :streamer-stream stream
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
