;;;; -*- Mode:Common-Lisp; Package:PORTABLE-SOCKETS-USER; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/test/http-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Mar 22 17:03:39 2006 *-*
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
;;; Copyright (C) 2005-2006, Dan Corkill <corkill@GBBopen.org>
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
  (import '(common-lisp-user::*autorun-gbbopen-modules*)))

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

(when *autorun-gbbopen-modules*
  (http-test "GBBopen.org" 80))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


