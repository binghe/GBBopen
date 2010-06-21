;;;; -*- Mode:Common-Lisp; Package:HTTP-SERVICES; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/http-services.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Jun 21 13:31:01 2010 *-*
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
;;; Copyright (C) 2009-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
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
  (export '(*http-server*               ; not yet documented
            decode-uri-string           ; not yet documented
            encode-xml-string           ; not yet documented
            encode-xml-value            ; not yet documented
            handle-http-get             ; not yet documented
            kill-http-server            ; not yet documented
            make-http-server            ; not yet documented
            send-http-response-headers  ; not yet documented
            start-http-server           ; not yet documented
            write-crlf)))               ; not yet documented

;;; ---------------------------------------------------------------------------

(defstruct (http-server (:copier nil))
  port
  thread
  clients
  (name "HTTP Connection Server")
  (log-stream *standard-output*)
  (log-stream-lock (make-lock :name "HTTP log stream"))
  log-connections
  log-headers
  log-requests)
  
(defvar *http-server*)

;;; ---------------------------------------------------------------------------

(defun decode-uri-string (encoded-uri)
  (cond
   ;; Need to decode?
   ((position #\% encoded-uri)
    (macrolet ((char-hex-code (char)
                 ;; convert hex character to numeric equiv
		 `(let ((.char-code. (char-code ,char)))
		    (if (<=& .char-code. #.(char-code #\9))
                        (-& .char-code. #.(char-code #\0))
                        (+& 9 (logand .char-code. 7))))))
      (let* ((length (length encoded-uri))
             (uri (make-array `(,length)
                              :element-type 'character
                              :adjustable t
                              :fill-pointer 0))
             (pos 0))
        (while (<& pos length)
          (let ((char (schar encoded-uri pos)))
            (when (char= char #\%)
              (setf char
                    (code-char
                     (+& (ash (char-hex-code (schar encoded-uri (1+& pos))) 4)
                         (char-hex-code (schar encoded-uri (+& pos 2))))))
              (incf& pos 2))
            (vector-push char uri))
          (incf& pos)) 
        (coerce uri 'simple-string))))
   ;; Nothing to decode:
   (t encoded-uri)))

;;; ---------------------------------------------------------------------------

(defparameter *xml-special-characters*
    `((#\< . "lt;")
      (#\> . "gt;")
      (#\& . "amp;")
      (#\' . "apos;")
      (#\" . "quot;")))

;;; ---------------------------------------------------------------------------

(defun encode-xml-string (string)
  (flet ((special-char-p (char)
           (assoc char *xml-special-characters*)))
    (declare (dynamic-extent #'special-char-p))
    (let ((pos (position-if #'special-char-p string)))
      (if pos
          (let* ((length (length string))
                 (encoded-string (make-array `(,length)
                                             :element-type 'character
                                             :adjustable t
                                             :fill-pointer 0))
                 (pos 0))
            (while (<& pos length)
              (let* ((char (schar string pos))
                     (acons (special-char-p char)))
                (cond
                 (acons
                  (vector-push-extend #\& encoded-string)
                  (dosequence (char (cdr acons))
                     (vector-push-extend char encoded-string)))
                 (t (vector-push-extend char encoded-string))))
              (incf& pos))
            (coerce encoded-string 'simple-string))
          ;; Nothing to decode:
          string))))

;;; ---------------------------------------------------------------------------

(defun encode-xml-value (value)
  (encode-xml-string (format nil "~s" value)))

;;; ---------------------------------------------------------------------------

(defun write-to-http-log (server control-string &rest args)
  (let ((log-stream (http-server-log-stream server)))
    (when log-stream
      (with-lock-held ((http-server-log-stream-lock server))
        (format log-stream "~&;; ~a ~?~%"
                (message-log-date-and-time)
                control-string
                args)))))

;;; ---------------------------------------------------------------------------

(defun add-to-http-log (server control-string &rest args)
  (let ((log-stream (http-server-log-stream server)))
    (when log-stream
      (with-lock-held ((http-server-log-stream-lock server))
        (format log-stream "~&;;~19t~?~%"
                control-string
                args)))))

;;; ---------------------------------------------------------------------------

(defun send-http-response-headers (connection status status-message
                                   &key (content-type "text/html")
                                        content-length
                                        last-modified
                                        location
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
  (when location
    (format connection "Location: ~a"
            location)
    (write-crlf connection))
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

(defun read-http-line (connection)
  (let ((line (with-error-handling      ; in case the client closes down at
                                        ; the wrong time...
                  (with-timeout (20)
                    (read-line connection nil nil)))))
    (when line (string-right-trim '(#\Return) line))))

;;; ---------------------------------------------------------------------------

(defun read-remaining-http-request-headers (server connection)
  ;; Reads the HTTP request headers (after the inital request header),
  ;; returning user-agent and connection-type values, if found:
  (let ((user-agent nil)
        (connection-type nil))
  (loop
      for line = (read-http-line connection)
      until (zerop (length line)) do
        (when (http-server-log-headers server)
          (add-to-http-log server "~s" line))
        (cond
         ;; User-Agent:
         ((and (>& (length line) 12) (string= "User-Agent: " line :end2 12))
          (setf user-agent (subseq line 12)))
         ;; Connection-Type:
         ((and (>& (length line) 12) (string= "Connection: " line :end2 12))
          (setf connection-type (subseq line 12)))))
  (values user-agent connection-type)))

;;; ---------------------------------------------------------------------------

(defun close-http-connection (server connection)
  (when (open-stream-p connection)
    (when (http-server-log-connections server)
      (multiple-value-call 'write-to-http-log 
        server
        "Closing connection from ~a/~s"
        (remote-hostname-and-port connection)))
    (close connection)))

;;; ---------------------------------------------------------------------------

(defun http-connection-thread (server connection)
  (when (http-server-log-connections server)
    (multiple-value-call 'write-to-http-log 
      server
      "Connection from ~a/~s"
      (remote-hostname-and-port connection)))
  (loop
    (let ((line (read-http-line connection)))
      (when (and line (or (http-server-log-requests server)
                          (http-server-log-headers server)))
        (add-to-http-log server "~s" line))
      (cond
       ;; The line is dead:
       ((not line) (return))
       ;; GET request:
       ((and (>& (length line) 3) (string= "GET" line :end2 3))
        (let ((path (subseq line 4 (position #\Space line :start 5 :test #'eq))))
          (multiple-value-bind (user-agent connection-type)
              (read-remaining-http-request-headers server connection)
            (funcall 'handle-http-get connection path user-agent)
            (unless (string= connection-type "keep-alive")
              (return)))))
       ;; Unhandled requests:
       (t (printv "Unhandled HTTP request:" line)
          (multiple-value-bind (user-agent connection-type)
              (read-remaining-http-request-headers connection)
            (declare (ignore user-agent))
            (unless (string= connection-type "keep-alive")
              (return))))))
    (force-output connection))
  (close-http-connection server connection))

;;; ---------------------------------------------------------------------------

(defun http-connection-server (connection) 
  (spawn-thread "HTTP Connection" 'http-connection-thread
                ;; Grab the server from the global binding (typically bound in
                ;; START-HTTP-SERVER):
                *http-server*
                connection))

;;; ---------------------------------------------------------------------------

(defun start-http-server (&optional (*http-server* *http-server*))
  (let ((server *http-server*))
    (setf (http-server-thread server)
          (start-connection-server 'http-connection-server
                                   (http-server-port server)
                                   :name (http-server-name server)
                                   :reuse-address 't))))

;;; ---------------------------------------------------------------------------

(defun kill-http-server (&optional (server *http-server*)
                                   (kill-clients 't))
  (let ((server-thread (http-server-thread server)))
    (when server-thread
      (kill-thread server-thread)
      (setf (http-server-thread server) nil))
    (when kill-clients
      (dolist (client (http-server-clients server))
        (kill-thread client))
      (setf (http-server-clients server) nil))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

