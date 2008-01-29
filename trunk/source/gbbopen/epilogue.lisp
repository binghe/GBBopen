;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/source/gbbopen/epilogue.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Tue Jan 29 10:19:37 2008 *-*
;;;; *-* Machine: whirlwind.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                GBBopen Miscellaneous Entities & Epilogue
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2004-2007, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-16-04 File Created.  (Corkill)
;;;  05-03-04 Added reset-gbbopen.  (Corkill)
;;;  11-07-07 Retain the root-space-instance when resetting GBBopen.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(load-blackboard-repository  ; not yet documented
            save-blackboard-repository  ; not yet documented
            reset-gbbopen)))

;;; ===========================================================================
;;;  Miscellaneous Entities

(defun reset-gbbopen (&key (disable-events t)
			   (retain-classes nil)
			   (retain-event-printing nil)
			   (retain-event-functions nil)
			   ;; Not documented:
			   (retain-space-instance-event-functions nil))
  ;;; Deletes all unit and space instances; resets instance counters to 1.
  (let ((*%%events-enabled%%* (not disable-events)))
    (map-extended-unit-classes 
     #'(lambda (unit-class plus-subclasses)
	 (declare (ignore plus-subclasses))
	 (unless (or 
                  ;; Retain the root-space-instance
                  (eq (class-name unit-class) 'root-space-instance)
                  (and retain-classes
                       (unit-class-in-specifier-p unit-class retain-classes)))
	   ;; We must practice safe delete-instance:
	   (let ((instances nil))
	     (map-instances-given-class 
	      #'(lambda (instance) (push instance instances)) unit-class)
	     (mapc #'delete-instance instances)
	     (reset-unit-class unit-class))))
     't)
    (unless retain-event-printing (disable-event-printing))
    ;; Keep around the path-event-functions specifications for new
    ;; space-instances if either :retain-event-functions (or the more
    ;; specific :retain-space-instance-event-functions) is specified:
    (unless (or retain-event-functions
		retain-space-instance-event-functions)
      (map-event-classes 
       #'(lambda (event-class plus-subclasses)
	   (declare (ignore plus-subclasses))
	   (setf (space-instance-event-class.path-event-functions event-class)
		 nil))
       (find-class 'space-instance-event)))
    ;; Remove all event functions (path-event-functions specifications
    ;; for new space instances are removed above, if appropriate):
    (unless retain-event-functions
      (remove-all-event-functions))))

;;; ===========================================================================
;;;  Save & restore repository

(defun make-bb-pathname (pathname) 
  ;; Adds type "bb", if not supplied ; then adds defaults from
  ;; *default-pathname-defaults*, as needed:
  (merge-pathnames 
   pathname
   (make-pathname :type "bb"
                  :defaults *default-pathname-defaults*)))

;;; ---------------------------------------------------------------------------

(defun save-blackboard-repository (pathname &key (package ':cl-user)
                                                 (read-default-float-format 
                                                  'single-float))
  (with-open-file (file (make-bb-pathname pathname)
                   :direction ':output
                   :if-exists ':supersede)
    (format file ";;;  GBBopen Blackboard Repository (saved ~a)~%"
            (internet-text-date-and-time))
    (with-saving/sending-block (file :package package
                                     :read-default-float-format 
                                     read-default-float-format)
      (let ((root-space-instance-children
             (space-instance-children *root-space-instance*)))
        (format file "~&;;;  Space instances:~%")
        (let ((*save/send-references-only* 't))
          (print-object-for-saving/sending root-space-instance-children file))
        (dolist (child root-space-instance-children)
          (traverse-space-instance-tree 
           #'(lambda (space-instance)
               (print-object-for-saving space-instance file))
           child)))
      (format file "~&;;;  Other unit instances:~%")
      (do-instances-of-class (instance t)
        ;; Skip  space instances:
        (unless (typep instance 'root-space-instance)
          (print-object-for-saving instance file))))
    (format file "~&;;;  End of File~%")
    (namestring file)))

;;; ---------------------------------------------------------------------------

(defun load-blackboard-repository (pathname &rest reset-gbbopen-args)
  (declare (dynamic-extent reset-gbbopen-args))
  (with-open-file (file  (make-bb-pathname pathname)
                   :direction ':input)
    (apply 'reset-gbbopen reset-gbbopen-args)
    (with-reading-object-block (file)
      (let ((root-children (read file))
            (*%%allow-setf-on-link%%* 't))
        (setf (slot-value *root-space-instance* 'children) root-children))
      ;; Now read everything else:
      (let ((eof-marker '#:eof))
        (until (eq eof-marker (read file nil eof-marker)))))
    (namestring file)))
  
;;; ===========================================================================
;;;  GBBopen is fully loaded

(pushnew :gbbopen *features*)
(pushnew *gbbopen-version-keyword* *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


