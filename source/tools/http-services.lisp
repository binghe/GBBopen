;;;; -*- Mode:Common-Lisp; Package:HTTP-SERVICES; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/http-services.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Aug 10 10:06:03 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       Basic HTTP and HTML Services
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2009, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; --------------------------------------------------------------------------
;;;
;;;  These basic service entities provide simple and easy-to-use HTTP and HTML
;;;  services when a full-blown Common Lisp HTTP server (such as AllegroServe or
;;;  Hunchentoot) is not required.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  08-05-09 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':http-services)
    (make-package ':http-services
                  :use '(:common-lisp :portable-threads :portable-sockets
                         :gbbopen-tools))))

(in-package :http-services)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(handle-http-get             ; not yet documented
            kill-http-server            ; not yet documented
            send-http-response-headers  ; not yet documented
            start-http-server           ; not yet documented
            write-crlf)))               ; not yet documented

 ;;; ---------------------------------------------------------------------------

(defvar *http-server-port* 8052)
(defvar *http-server-thread* nil)
(defvar *http-clients* nil)

;;; ---------------------------------------------------------------------------

(defun send-http-response-headers (connection status status-message
                                   &key (content-type "text/html")
                                        content-length
                                        last-modified
                                        (server
                                         (format nil "GBBopen/~a" 
                                                 (gbbopen-tools-implementation-version)))
                                        (charset "iso-8859-1"))
  (format connection "HTTP/1.1 ~d ~a" status status-message)
  (write-crlf connection)
  (format connection "Server: ~a" server)
  (write-crlf connection)
  (format connection "Date: ")
  (http-date-and-time nil :destination connection)
  (write-crlf connection)
  (format connection "Content-Type: ~a; charset=~a"
          content-type charset)
  (write-crlf connection)
  (when content-length
    (format connection "Content-Length: ~s"
            content-length)
    (write-crlf connection))
  (when last-modified
    (format connection "Last-Modified: ")
    (http-date-and-time last-modified :destination connection)
    (write-crlf connection))
  (write-crlf connection)
  (values))

;;; ---------------------------------------------------------------------------

(defun http-connection-thread (connection)
  (loop for line = (string-trim '(#\Return) (read-line connection nil nil))
      until (zerop (length line)) do
        #+debugging
        (printv line)
        (cond
         ;; GET request:
         ((and (>& (length line) 3) (string= "GET" line :end2 3)) 
          (funcall 'handle-http-get connection
                   (subseq line 4
                           (position #\Space line :start 5 :test #'eq))))))
  #-debugging
  (printv "Closing connection...")
  (close connection))

;;; ---------------------------------------------------------------------------

(defun http-connection-server (connection) 
  (let ((client-thread
         (spawn-thread "HTTP Connection" 'http-connection-thread connection)))
    (atomic-push client-thread *http-clients*)))

;;; ---------------------------------------------------------------------------

(defun start-http-server (&optional (port *http-server-port*))
  (setf *http-server-thread*
        (start-connection-server 'http-connection-server
                                 port
                                 :name "HTTP Connection Server"
                                 :reuse-address 't)))

;;; ---------------------------------------------------------------------------

(defun kill-http-server (&optional (kill-clients 't))
  (when *http-server-thread*
    (kill-thread *http-server-thread*))
  (when kill-clients
    (dolist (client *http-clients*)
      (kill-thread client))
    (setf *http-clients* nil)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

