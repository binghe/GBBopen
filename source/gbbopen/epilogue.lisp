;;;; -*- Mode:Common-Lisp; Package:GBBOPEN; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/gbbopen/epilogue.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Mar 11 19:43:02 2010 *-*
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
;;; Copyright (C) 2004-2010, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-16-04 File created.  (Corkill)
;;;  05-03-04 Added reset-gbbopen.  (Corkill)
;;;  01-28-08 Added load-blackboard-repository and save-blackboard-repository.
;;;           (Corkill)
;;;  05-02-08 Added :di and :dsi REPL commands.  (Corkill)
;;;  05-25-08 Added :dsis REPL command.  (Corkill)
;;;  04-25-09 Added :pic REPL command.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*current-system-name*
            common-lisp-user::define-repl-command)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(gbbopen-tools::*recorded-class-descriptions-ht*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(confirm-if-blackboard-repository-not-empty-p
            delete-blackboard-repository
            empty-blackboard-repository-p
            load-blackboard-repository
            save-blackboard-repository
            reset-gbbopen)))

;;; ---------------------------------------------------------------------------
;;;  Save-blackboard-repository format version

(defparameter *save-blackboard-repository-format-version* 5)

;;; ---------------------------------------------------------------------------
;;;  Result-passing variable (used by :fi REPL command), with a bit of
;;;  effort to make use of the = symbol where necessary...

#-(or lispworks sbcl)
(defvar = nil)

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (special =))
  (unless (boundp '=)
    (setf = nil)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext::without-package-locks
   (defvar = nil)))

;;; ===========================================================================
;;;  Miscellaneous Entities

(defun empty-blackboard-repository-p ()
  ;; Returns t if there are no unit instances in the blackboard repository
  (flet ((fn (class plus-subclasses-p)
           (declare (ignore plus-subclasses-p))
           (when (plusp& (class-instances-count class))
             (return-from empty-blackboard-repository-p nil))))
    (declare (dynamic-extent #'fn))
    (map-unit-classes 
     #'fn (load-time-value (find-class 'standard-unit-instance))))
  ;; The repository is empty:
  't)

;;; ---------------------------------------------------------------------------

(defun delete-blackboard-repository (&key (all-classes nil)
                                          (disable-events t)
                                          (retain-classes nil))
  (when (and all-classes retain-classes)
    (warn "~s is being overridden by ~s ~s."
          ':all-classes ':retain-classes retain-classes))
  ;;; Deletes all unit and space instances; resets instance counters to 1.
  (with-lock-held (*master-instance-lock*)
    (let ((*%%events-enabled%%* (not disable-events)))
      (flet ((fn (unit-class plus-subclasses)
               (declare (ignore plus-subclasses))
               (unless (or 
                         ;; Explicitly retained:
                         (and retain-classes
                              (unit-class-in-specifier-p
                               unit-class retain-classes))
                         ;; :all-classes specified or a retained unit class:
                         (not (or all-classes
                                  (not (or (standard-unit-class.retain unit-class))))))
                 ;; We must practice safe delete-instance:
                 (let ((instances nil))
                   (flet ((do-instance (instance) (push instance instances)))
                     (declare (dynamic-extent #'do-instance))
                     (map-instances-given-class #'do-instance unit-class))
                   (mapc #'delete-instance instances)
                   (reset-unit-class unit-class)))))
        (declare (dynamic-extent #'fn))
        (map-extended-unit-classes #'fn 't)))
    ;; Reset the global instance-name counter, if possible:
    (when (empty-blackboard-repository-p)
      (setf *global-instance-name-counter* 0)))
  ;; Return a polite success:
  't)
  
;;; ---------------------------------------------------------------------------

(defun reset-gbbopen (&key (disable-events t))
  (with-lock-held (*master-instance-lock*)
    (delete-blackboard-repository :all-classes 't
                                  :disable-events disable-events)
    (setf *top-level-space-instances* nil))
  (disable-event-printing)
  (remove-all-event-functions)
  ;; Return a polite success:
  't)

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

(defun make-unit-instance-count-alist ()
  (let ((result nil))
    (flet ((fn (unit-class plus-subclasses)
             (declare (ignore plus-subclasses))
             (let ((count (class-instances-count unit-class)))
               (when (plusp& count)
                 (push `(,(class-name unit-class) . ,count) result)))))
      (declare (dynamic-extent #'fn))
      (map-extended-unit-classes #'fn 't))
    result))

;;; ---------------------------------------------------------------------------

(defun save-blackboard-repository (pathname
                                   &key (package ':cl-user)
                                        (read-default-float-format
                                         'single-float)
                                        (external-format ':default)
                                        (value))
  (with-open-file (file (make-bb-pathname pathname)
                   :direction ':output
                   :if-exists ':supersede
                   :external-format external-format)
    (format file ";;; GBBopen Blackboard Repository (saved ~a)~%"
            (internet-text-date-and-time))
    (with-saving/sending-block (file :package package
                                     :read-default-float-format 
                                     read-default-float-format
                                     :value value)
      (with-lock-held (*master-instance-lock*)
        ;; Save repository-format version:
        (format file "~&;;; Saved repository format version:~%~s~%"
                *save-blackboard-repository-format-version*)
        ;; Save important values:
        (format file "~&;;; Important values:~%~s~%"
                (list *global-instance-name-counter* (make-unit-instance-count-alist)))
        ;; Save space instances:
        (let ((top-level-space-instances *top-level-space-instances*))
          (format file "~&;;; Space instances:~%")
          ;; Save top-level (root children) space-instance references:
          (let ((*save/send-references-only* 't))
            (print-object-for-saving/sending top-level-space-instances file))
          ;; Now save the space-instances in the repository forest:
          (let ((*save/send-references-only* nil))
            (dolist (child top-level-space-instances)
              (flet ((do-si (space-instance)
                       (print-object-for-saving/sending space-instance file)))
                (declare (dynamic-extent #'do-si))
                (traverse-space-instance-tree #'do-si child)))
            ;; Save non-space unit instances:
            (format file "~&;;; Other unit instances:~%")
            (do-instances-of-class (instance t)
              ;; Skip space instances (standard-space-instance :plus-subclasses):
              (unless (typep instance 'standard-space-instance)
                (print-object-for-saving/sending instance file)))
            ;; Save unit-class states:
            (format file "~&;;; Unit-class states:~%")
            (loop for class-name being the hash-keys
                of *recorded-class-descriptions-ht* do
                  (let ((class (find-class class-name)))
                    (when (typep class 'standard-unit-class)
                      (print-unit-class-state-for-saving/sending
                       class stream))))))))
    (format file "~&;;; End of file~%")
    (pathname file)))

;;; ---------------------------------------------------------------------------

(defun confirm-if-blackboard-repository-not-empty-p
    (&key describe-non-empty-repository
          (pending-action "deleted"))
  ;; Returns true unless the blackboard-repository is not empty and the user
  ;; doesn't confirm continuing:
  (cond
   ((empty-blackboard-repository-p) 't)
   (t (when describe-non-empty-repository
        (describe-blackboard-repository))
      (nicer-y-or-n-p 
       "The~@[ above~*~] blackboard repository is not empty.~%Continue anyway ~
        (the current contents will be ~a)? "
       describe-non-empty-repository
       pending-action))))

;;; ---------------------------------------------------------------------------

(defun load-blackboard-repository
    (pathname 
     &rest reset-gbbopen-args
     &key (class-name-translations nil)
          (coalesce-strings nil)
          (confirm-if-not-empty 't)
          (estimated-peak-forward-references 
            *default-estimated-peak-forward-references*)
          (external-format ':default)
          (readtable *reading-saved/sent-objects-readtable*)
          (read-eval nil))
  (declare (dynamic-extent reset-gbbopen-args))
  (when confirm-if-not-empty
    (unless (confirm-if-blackboard-repository-not-empty-p)
      (return-from load-blackboard-repository nil)))
  (with-open-file (file (make-bb-pathname pathname)
                   :direction ':input
                   :external-format external-format)
    (with-lock-held (*master-instance-lock*)
      (apply 'delete-blackboard-repository
             :all-classes 't
             (remove-properties reset-gbbopen-args 
                                '(:class-name-translations
                                  :coalesce-strings
                                  :confirm-if-not-empty
                                  :estimated-peak-forward-references
                                  :external-format
                                  :readtable
                                  :read-eval)))
      (with-reading-saved/sent-objects-block 
          (file :class-name-translations class-name-translations
                :coalesce-strings coalesce-strings
                :estimated-peak-forward-references 
                   estimated-peak-forward-references
                :readtable readtable
                :read-eval read-eval)
        (let ((format-version (read file)))
          (when (>& format-version *save-blackboard-repository-format-version*)
            (error "Incompatible ~s format version ~a ~
                    (the current version is ~a, generated by a newer version ~
                   of GBBopen)"
                   'save-blackboard-repository
                   format-version
                   *save-blackboard-repository-format-version*))
          (unless (>=& format-version 2)
            (error "Incompatible ~s format version ~a (the current version is ~a)"
                   'save-blackboard-repository
                   format-version
                   *save-blackboard-repository-format-version*))
          (when (>=& format-version 3)
            (destructuring-bind (global-instance-name-counter 
                                 &optional unit-instance-count-alist)
                ;; Read important values:
                (read file)
              (setf *global-instance-name-counter* global-instance-name-counter)
              ;; Resize instance hash-tables:
              (dolist (acons unit-instance-count-alist)
                (destructuring-bind (class-name . count)
                    acons
                  (let ((unit-class 
                         (find-unit-class 
                          (possibly-translate-class-name class-name))))
                    (when unit-class
                      (resize-instance-hash-table unit-class count)))))))
          ;; Skip after-loading-function, if an old format version:
          (when (<& format-version 4)
            (read file)))
        ;; Read top-level (root children) space-instance references:
        (let ((root-children (read file))
              (*%%allow-setf-on-link%%* 't))
          (setf *top-level-space-instances* root-children)
          ;; Now read everything else:
          (let ((eof-marker '#:eof))
            (until (eq eof-marker (read file nil eof-marker))))
          ;; Process all unit instances, in case any link-slot arity or sorting
          ;; has changed (FUTURE ENHANCEMENT: It would be nice to skip this
          ;; unless such has truly happened):
          (map-instances-of-class #'reconcile-direct-link-values 't))
        ;; Return the pathname, saved/sent-time, and saved/sent-value:
        (values (pathname file)
                *block-saved/sent-time* 
                *block-saved/sent-value*)))))

;;; ---------------------------------------------------------------------------
;;;  Add :di, :dsbb, :dsi, :dsis, :fi, and :pic REPL commands (available if
;;;  using GBBopen's initiate.lisp)

(defun do-di/dsi-repl-command (type find-fn describe-fn args)
      (let ((maybe-instance 
             ;; Handle evaluating REPLs:
             (if (typep (first args) type)
                 ;; Already evaluated:
                 (first args)
                 ;; Try evaluating:
                 (ignore-errors (eval (first args))))))
        (cond 
         ;; We're given a unit instance:
         ((typep maybe-instance type)
          (setf = maybe-instance)
          (funcall describe-fn maybe-instance))
         ;; Look it up
         (t (let ((instance (apply find-fn args)))
              (cond
               (instance 
                (setf = instance)
                (funcall describe-fn instance))
               (t (case type
                    (standard-unit-instance
                     (format t "~&No unit instance named ~s~@[ of class ~s~] ~
                             was found~%"
                             (first args)
                             (second args)))
                    (standard-space-instance
                     (format t "~&No space instance named ~s was found~%"
                             (first args))))
                  (force-output))))))))

;;; ---------------------------------------------------------------------------

(defun do-fi-repl-command (args)
  (let ((instance (apply 'find-instance-by-name args)))
    (cond 
     (instance
      (setf = instance)
      (format t "~&Found ~s (assigned to =)~%" instance)
      (force-output)
      instance)
     (t (format t "~&No unit instance named ~s~@[ of class ~s~] was found~%"
                (first args)
                (second args))
        (force-output)))))
  
;;; ===========================================================================
;;;  If DEFINE-REPL-COMMAND is available, define these GBBopen commands:

(when (fboundp 'define-repl-command)
  (eval `(let ((*current-system-name* ':gbbopen))
           (declare (special *current-system-name*))
           
           (define-repl-command :di (&rest args)
             "Describe instance"
             (do-di/dsi-repl-command 'standard-unit-instance 
               'find-instance-by-name
               'describe-instance args))
           
           (define-repl-command (:dsbb :add-to-native-help) ()
             "Describe blackboard repository"
             (describe-blackboard-repository))
           
           (define-repl-command :dsi (&rest args)
             "Describe space instance"
             (do-di/dsi-repl-command 'standard-space-instance 
               'find-space-instance-by-path
               'describe-space-instance args))
           
           (define-repl-command :dsis (&rest args)
             "Describe space instance storage"
             (do-di/dsi-repl-command 'standard-space-instance 
               'find-space-instance-by-path
               'describe-space-instance-storage args))
           
           (define-repl-command :fi (&rest args)
             "Find instance by name"
             (do-fi-repl-command args))

           (define-repl-command :pic (&rest args)
             "Print instances of class"
             (apply 'map-instances-of-class 'print (or args '(t)))))))


;;; ===========================================================================
;;;  GBBopen is fully loaded

(pushnew ':gbbopen *features*)
(pushnew *gbbopen-version-keyword* *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


