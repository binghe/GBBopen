;;;; -*- Mode:Common-Lisp; Package:PORTABLE-SOCKETS-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/test/http-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jul 25 17:46:34 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                        Basic Socket/HTTP Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-11-05 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :portable-sockets-user) 
    (defpackage :portable-sockets-user
      (:use :common-lisp :portable-threads :portable-sockets))))

(in-package :portable-sockets-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*autorun-modules*)))

;;; ---------------------------------------------------------------------------

(defparameter *test-port* 9876)

;;; ---------------------------------------------------------------------------

(defun log-error (control-string &rest args)
  (declare (dynamic-extent args))
  (format t "~&;; ~73,,,'*<*~>~
             ~%;; *** ~?~
             ~&;; ~73,,,'*<*~>"
          control-string
          args))

;;; ---------------------------------------------------------------------------

(defun forced-format (&rest args)
  (declare (dynamic-extent args))
  (apply #'format t args)
  (force-output))

;;; ---------------------------------------------------------------------------

(defun http-test (host port)
  (with-open-connection (connection host port)
    (format connection "GET /robots.txt HTTP/1.1")
    (write-crlf connection)
    (format connection "Host: ~a:~a" host port)
    (write-crlf connection)
    (write-crlf connection)
    (force-output connection)
    (let (line)
      (loop :while (setq line (read-line connection nil)) :do
	(format t "~&;; ~a~%" line)))))

;;; ---------------------------------------------------------------------------

(defun tester (port &optional (host *localhost*))
  (format t "~&;; Opening connection~%")
  (with-open-connection (connection host port)
    (format t "~&;; Connected~%")
    (format connection "This is a test!~%")
    (finish-output connection)
    (format t "~&;; Finishing...~%")))

;;; ---------------------------------------------------------------------------

(defun connection-server-tester (connection)
  (let ((line (read-line connection nil)))
    (format t "~&;; New Connection: ~a~%" line)
    (close connection)))

;;; ---------------------------------------------------------------------------

(defun passive-socket-tests ()
  (forced-format "~&;; Performing passive-socket and connection tests...")
  (let ((passive-socket (make-passive-socket *test-port*))
        (client1 (open-connection *localhost* *test-port*))
        (client2 (open-connection *localhost* *test-port*)))
    (let ((server1 (accept-connection passive-socket))
          (server2 (accept-connection passive-socket))
          (form '(:a 1 :b "2" :c 3.0 :d 't)))
      (close-passive-socket passive-socket)
      ;; Print test form:
      (print form client1)
      ;; Print it again:
      (print form client1)
      (force-output client1)
      ;; Read test form:
      (let ((read-form (read server1)))
        (unless (equal form read-form)
          (log-error "Wrong form read: ~s" read-form)))
      ;; Read it again:
      (let ((read-form (read server1)))
        (unless (equal form read-form)
          (log-error "Wrong form read: ~s" read-form)))
      (close client1)
      (close server1)
      ;; Shutdown client2:
      (shutdown-socket-stream client2 ':output)
      (let ((read-form (read server2 nil ':eof)))
        (unless (eq read-form ':eof)
          (log-error "Didn't receive client shutdown")))
      (close server2)
      (let ((read-form (read client2 nil ':eof)))
        (unless (eq read-form ':eof)
          (log-error "Didn't receive server close")))
      (close client2)))
  (forced-format "~&;; Passive-socket and connection tests completed."))

;;; ---------------------------------------------------------------------------

(defun portable-sockets-test ()
  (http-test "GBBopen.org" 80)
  (passive-socket-tests))

;;; ---------------------------------------------------------------------------

(when *autorun-modules* (portable-sockets-test))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


