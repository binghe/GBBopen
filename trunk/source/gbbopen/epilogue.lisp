;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/epilogue.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr 27 13:56:51 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

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
;;; Copyright (C) 2004-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-16-04 File Created.  (Corkill)
;;;  05-03-04 Added reset-gbbopen.  (Corkill)
;;;  11-07-07 Retain the root-space-instance when resetting GBBopen.  (Corkill)
;;;  01-28-08 Added load-blackboard-repository and save-blackboard-repository.
;;;           (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*current-system-name*
            common-lisp-user::define-repl-command)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::*recorded-class-descriptions-ht*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(confirm-if-blackboard-repository-not-empty-p ; not yet documented
            delete-blackboard-repository
            empty-blackboard-repository-p ; not yet documented
            load-blackboard-repository
            save-blackboard-repository
            reset-gbbopen)))

;;; ---------------------------------------------------------------------------
;;;  Save-blackboard-repository format version

(defparameter *save-blackboard-repository-format-version* 2)

;;; ===========================================================================
;;;  Miscellaneous Entities

(defun delete-blackboard-repository (&key (all-classes nil)
                                          (disable-events t)
                                          (retain-classes nil))
  (when (and all-classes retain-classes)
    (warn "~s is being overridden by ~s ~s."
          ':all-classes ':retain-classes retain-classes))
  ;;; Deletes all unit and space instances; resets instance counters to 1.
  (let ((*%%events-enabled%%* (not disable-events)))
    (map-extended-unit-classes 
     #'(lambda (unit-class plus-subclasses)
	 (declare (ignore plus-subclasses))
	 (unless (or 
                  ;; Retain the root-space-instance
                  (eq (class-name unit-class) 'root-space-instance)
                  ;; Explicitly retained:
                  (and retain-classes
                       (unit-class-in-specifier-p
                        unit-class retain-classes))
                  ;; :all-classes specified or a retained unit class:
                  (not (or all-classes
                           (not (or (standard-unit-class.retain unit-class))))))
	   ;; We must practice safe delete-instance:
	   (let ((instances nil))
	     (map-instances-given-class 
	      #'(lambda (instance) (push instance instances)) unit-class)
	     (mapc #'delete-instance instances)
	     (reset-unit-class unit-class))))
     't)))
  
;;; ---------------------------------------------------------------------------

(defun reset-gbbopen (&key (disable-events t))
  (delete-blackboard-repository :all-classes 't
                                :disable-events disable-events)
  (disable-event-printing)
  (remove-all-event-functions))

;;; ===========================================================================
;;;  Save & restore repository

(defun make-bb-pathname (pathname) 
  ;; Adds type "bb", if not supplied ; then adds defaults from
  ;; (user-homedir-pathname), as needed:
  (merge-pathnames 
   pathname
   (make-pathname :type "bb"
                  :defaults (user-homedir-pathname))))

;;; ---------------------------------------------------------------------------

(defun save-blackboard-repository (pathname
                                   &key (after-loading-function nil)
                                        (package ':cl-user)
                                        (read-default-float-format
                                         'single-float)
                                        (external-format ':default))
  (with-open-file (file (make-bb-pathname pathname)
                   :direction ':output
                   :if-exists ':supersede
                   :external-format external-format)
    (format file ";;; GBBopen Blackboard Repository (saved ~a)~%"
            (internet-text-date-and-time))
    (with-saving/sending-block (file :package package
                                     :read-default-float-format 
                                     read-default-float-format)
      ;; Save repository-format version:
      (format file "~&;;; Saved repository format version:~%~s~%"
              *save-blackboard-repository-format-version*)
      ;; Save after-loading-function:
      (format file "~&;;; After-loading function:~%")
      (print-object-for-saving/sending after-loading-function file)
      ;; Save space instances:
      (let ((root-space-instance-children (children-of *root-space-instance*)))
        (format file "~&;;; Space instances:~%")
        ;; Save top-level (root children) space-instance references:
        (let ((*save/send-references-only* 't))
          (print-object-for-saving/sending root-space-instance-children file))
        ;; Now save the space-instances in the repository forest:
        (let ((*save/send-references-only* nil))
          (dolist (child root-space-instance-children)
            (traverse-space-instance-tree 
             #'(lambda (space-instance)
                 (print-object-for-saving/sending space-instance file))
             child))
          ;; Save non-space unit instances:
          (format file "~&;;; Other unit instances:~%")
          (do-instances-of-class (instance t)
            ;; Skip space instances (root-space-instance :plus-subclasses):
            (unless (typep instance 'root-space-instance)
              (print-object-for-saving/sending instance file)))
          ;; Save unit-class states:
          (format file "~&;;; Unit-class states:~%")
          (loop for class-name being the hash-keys
              of *recorded-class-descriptions-ht* do
                (let ((class (find-class class-name)))
                  (when (typep class 'standard-unit-class)
                    (print-unit-class-state-for-saving/sending
                     class stream)))))))
    (format file "~&;;; End of file~%")
    (pathname file)))

;;; ---------------------------------------------------------------------------

(defun empty-blackboard-repository-p ()
  ;; Returns t if there are no unit instances (other than the
  ;; root-space-instance) in the blackboard repository
  (map-unit-classes
   #'(lambda (class plus-subclasses-p)
       (declare (ignore plus-subclasses-p))
       (unless (eq (class-name class) 'root-space-instance)
         (when (plusp& (class-instances-count class))
           (return-from empty-blackboard-repository-p nil))))
   (find-class 'standard-unit-instance))
  ;; The repository is empty:
  't)

;;; ---------------------------------------------------------------------------

(defun confirm-if-blackboard-repository-not-empty-p ()
  ;; Returns true unless the blackboard-repository is not empty and the user
  ;; doesn't confirm continuing:
  (if (empty-blackboard-repository-p)
      't
      (nicer-y-or-n-p 
       "The blackboard repository is not empty.~%Continue anyway ~
        (the current contents will be deleted)? ")))

;;; ---------------------------------------------------------------------------

(defun load-blackboard-repository (pathname 
                                   &rest reset-gbbopen-args
                                   &key (class-name-translations nil)
                                        (confirm-if-not-empty 't)
                                        (external-format ':default)
                                        (ignore-after-loading-function nil)
                                        (readtable
                                         *reading-saved/sent-objects-readtable*)
                                        (read-eval nil))
  (declare (dynamic-extent reset-gbbopen-args))
  (when confirm-if-not-empty
    (unless (confirm-if-blackboard-repository-not-empty-p)
      (return-from load-blackboard-repository nil)))
  (with-open-file (file (make-bb-pathname pathname)
                   :direction ':input
                   :external-format external-format)
    (apply 'delete-blackboard-repository
           :all-classes 't
           (remove-properties reset-gbbopen-args 
                              '(:class-name-translations
                                :confirm-if-not-empty
                                :external-format
                                :ignore-after-loading-function
                                :readtable
                                :read-eval)))
    (with-reading-saved/sent-objects-block 
        (file :class-name-translations class-name-translations
              :readtable readtable
              :read-eval read-eval)
      (let ((format-version (read file)))
        (unless (eql format-version *save-blackboard-repository-format-version*)
          (error "Incompatible ~s format version ~a (the current version is ~a)"
                 'save-blackboard-repository
                 format-version
                 *save-blackboard-repository-format-version*)))
      (let ((after-loading-function (read file))
            ;; Read top-level (root children) space-instance references:
            (root-children (read file))
            (*%%allow-setf-on-link%%* 't))
        (setf (slot-value *root-space-instance* 'children) root-children)
        ;; Now read everything else:
        (let ((eof-marker '#:eof))
          (until (eq eof-marker (read file nil eof-marker))))
        ;; Return the pathname and values from executing the
        ;; after-loading-function:
        (apply #'values
               (pathname file)
               (when (and after-loading-function
                          (not ignore-after-loading-function))
                 (multiple-value-list (funcall after-loading-function))))))))
  
;;; ---------------------------------------------------------------------------
;;;  Add :dsbb REPL command (available if using GBBopen's initiate.lisp)

(when (fboundp 'define-repl-command)
  (let ((common-lisp-user::*current-system-name* ':gbbopen))
    (declare (special common-lisp-user::*current-system-name*))

    (define-repl-command (:dsbb :add-to-native-help) ()
      "Describe blackboard repository"
      (describe-blackboard-repository))

    (define-repl-command :fi (&rest args)
      "Find instance by name"
      (let ((instance (apply 'find-instance-by-name args)))
        (cond 
         (instance
          ;; Note: * is not set in SBCL's REPL:
          (setf * instance)
          (format t "~&Found ~s (assigned to ~s)~%" instance '*)
          (force-output)
          instance)
         (t (format t "~&No unit instance named ~s~@[ of class ~s~] was ~
                         found~%"
                    (first args)
                    (second args))
            (force-output)))))))
  
;;; ===========================================================================
;;;  GBBopen is fully loaded

(pushnew :gbbopen *features*)
(pushnew *gbbopen-version-keyword* *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


