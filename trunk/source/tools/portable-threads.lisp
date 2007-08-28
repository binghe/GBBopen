;;;; -*- Mode:Common-Lisp; Package:PORTABLE-THREADS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/portable-threads.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Mon Aug 27 22:15:37 2007 *-*
;;;; *-* Machine: ruby.corkills.org *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *             Portable Threads (Multiprocessing) Interface
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2003-2007, Dan Corkill <corkill@GBBopen.org> 
;;;
;;; Developed and supported by the GBBopen Project (http://GBBopen.org) and
;;; donated to the CL Gardeners portable threads initiative
;;; (http://wiki.alu.org/Portable_Threads).  (Licenced under the Apache 2.0
;;; license, see http://GBBopen.org/downloads/LICENSE for license details.)
;;;
;;; Bug reports, suggestions, enhancements, and extensions should be sent to
;;; corkill@GBBopen.org.
;;;
;;; On-line documentation for these portable thread interface entities is
;;; available at http://gbbopen.org/hyperdoc/index.html
;;;
;;; This file can be used stand-alone on the supported CLs (no additional
;;; libraries are requried).
;;;
;;; Porting Notice:
;;;
;;;   The semantics of these interface entities must be maintained when
;;;   porting to new CL implementations/versions
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-21-03 File Created.  (Corkill)
;;;  03-20-04 Added process-yield, kill, hibernate/awaken.  (Corkill)
;;;  03-21-04 Added atomic operations.  (Corkill)
;;;  06-11-05 Clean up best attempts for non-threaded CLs.  (Corkill)
;;;  10-21-05 Added polling functions for non-threaded CLs.  (Corkill)
;;;  12-17-05 Added process-wait-with-timeout.  (Corkill)
;;;  12-22-05 Removed without-interrupts support (incompatible with
;;;           preemptive scheduling models).  (Corkill)
;;;  12-27-05 Added process-name.  (Corkill)
;;;  01-02-06 Separated from GBBopen, moved polling-functions into separate
;;;           polling-functions.lisp file and module.  (Corkill)
;;;  01-12-06 Added as-atomic-operation support, but only as a mechanism
;;;           for implementing very brief atomic operations.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
;;;  07-28-07 V2.0 naming changes, full condition variable support.  (Corkill)
;;;  08-20-07 V2.1: Added scheduled functions, thread-alive-p, 
;;;           encode-time-of-day. (Corkill)
;;;  08-27-07 V2.2: Added periodic functions.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :portable-threads)
    (defpackage :portable-threads
      (:use :common-lisp))))

(in-package :portable-threads)

;; Support for threads in Corman Common Lisp is under development and is 
;; incomplete, so we consider it threadless, for now:
#+cormanlisp-is-not-ready
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'threads))

;;; ---------------------------------------------------------------------------
;;; Add a single feature to identify sufficiently new Digitool MCL
;;; implementations (at present, both Digitool MCL and OpenMCL include
;;; the feature mcl):

#+(and digitool ccl-5.1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :digitool-mcl *features*))

;;; ---------------------------------------------------------------------------
;;;  Warn if sb-thread support is missing on SBCL/linux

#+(and sbcl linux (not sb-thread))
(warn "Thread support on ~a is not present.~@
       (Add the ~s feature in SBCL's customize-target-features.lisp ~
        and rebuild)"
      (lisp-implementation-type)
      :sb-thread)

;;; ---------------------------------------------------------------------------
;;;  Warn if threads support is missing in ECL

#+(and ecl (not threads)) 
(warn "Thread support on ~a is not present.~@
       (Use configure option --enable-threads and remake to provide threads ~
        support.)"
      (lisp-implementation-version))

;;; ---------------------------------------------------------------------------
;;; Import the CL-implementation's threading symbols, as needed:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   #+allegro 
   '(mp:close-gate
     mp:gate-open-p
     mp:make-gate
     mp:open-gate
     mp:process-wait
     mp:process-wait-with-timeout
     sys:with-timeout)
   #+clisp
   '()
   #+(and cmu mp)
   '(mp:atomic-decf
     mp:atomic-incf
     mp:atomic-pop
     mp:atomic-push
     mp:process-wait
     mp:process-wait-with-timeout
     mp::recursive-lock
     mp:with-timeout)
   #+(and cmu (not mp))
   '()
   #+cormanlisp
   '()
   #+digitool-mcl
   '(ccl:process-wait
     ccl:process-wait-with-timeout)
   ;; EL's mp:make-lock doesn't support owner, so we must build our own
   ;; from it
   #+(and ecl threads)
   '()                                  
   #+(and ecl (not threads))
   '()
   #+gcl
   '()
   #+lispworks
   '(mp:make-lock
     mp:process-wait
     mp:process-wait-with-timeout)
   #+openmcl
   '(ccl:process-wait
     ccl:process-wait-with-timeout)
   #+(and sbcl sb-thread)
   '(sb-thread:thread-alive-p
     sb-thread::thread-name)
   #+(and sbcl (not sb-thread))
   '()
   #+scl
   '(mp:atomic-decf
     mp:atomic-incf
     mp:atomic-pop
     mp:atomic-push
     thread:make-cond-var
     thread:cond-var-wait
     thread:cond-var-timedwait
     thread:cond-var-signal
     thread:cond-var-broadcast)))

;;; ---------------------------------------------------------------------------
;;;   Name changes in V1.0 (support for old names will be removed soon):
;;;
;;;    all-processes              all-threads
;;;    awaken-process             awaken-thread
;;;    current-process            current-thread
;;;    hibernate-process          hibernate-thread
;;;    kill-process               kill-thread
;;;    make-process-lock          make-recursive-lock
;;;    process-name               thread-name
;;;    processp                   threadp
;;;    process-wait               thread-wait
;;;    process-wait-with-timeout  thread-wait-with-timeout
;;;    process-whostate           thread-whostate
;;;    process-yield              thread-yield
;;;    run-in-process             run-in-thread
;;;    spawn-process              spawn-thread
;;;    symbol-value-in-process    symbol-value-in-thread
;;;    with-process-lock          with-lock-held
;;;    <new>                      make-lock
;;;    <new>                      condition-variable (object)
;;;    <new>                      make-condition-variable
;;;    <new>                      condition-variable-broadcast
;;;    <new>                      condition-variable-signal
;;;    <new>                      condition-variable-wait
;;;    <new>                      condition-variable-wait-with-timeout
;;;    <new>                      thread-holds-lock-p

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*non-threaded-polling-function-hook* ; not documented
            *periodic-function-verbose* ; new (document soon)
            *schedule-function-verbose*
	    all-processes               ; renamed (to be removed soon)
            all-scheduled-functions
            all-threads
	    as-atomic-operation
	    atomic-decf
	    atomic-delete
	    atomic-flush
	    atomic-incf
	    atomic-pop
	    atomic-push
	    atomic-pushnew
	    awaken-process		; renamed (to be removed soon)
	    awaken-thread
	    close-gate                  ; deprecated, to be removed
	    condition-variable
	    condition-variable-broadcast
            condition-variable-signal
            condition-variable-wait
	    condition-variable-wait-with-timeout
	    current-process             ; renamed (to be removed soon)
            current-thread
            encode-time-of-day
	    gate-open-p                 ; deprecated, to be removed
	    hibernate-process		; renamed (to be removed soon)
	    hibernate-thread
            kill-periodic-function      ; new (document soon)
	    kill-process                ; renamed (to be removed soon)
            kill-thread
	    make-condition-variable
	    make-gate                   ; deprecated, to be removed
            make-lock
	    make-process-lock           ; renamed (to be removed soon)
            make-recursive-lock
            make-scheduled-function
	    multiprocessing-not-available ; renamed (remove soon)
	    open-gate                   ; deprecated, to be removed
	    process-name                ; renamed (to be removed soon)
            portable-threads-implementation-version ; not documented
	    processp                    ; renamed (to be removed soon)
	    process-wait                ; deprecated, to be removed
	    process-wait-with-timeout   ; deprecated, to be removed
	    process-whostate            ; renamed (to be removed soon)
	    process-yield               ; renamed (to be removed soon)
            restart-scheduled-function-scheduler
	    run-in-process              ; renamed (to be removed soon)
            run-in-thread
            schedule-function
            schedule-function-relative
            scheduled-function          ; structure (not documented)
            scheduled-function-name
            scheduled-function-repeat-interval
            spawn-periodic-function     ; new (document soon)
	    spawn-process               ; renamed (to be removed soon)
            spawn-thread
	    symbol-value-in-process     ; renamed (to be removed soon)
            symbol-value-in-thread
            threadp
            threads-not-available       ; not documented
            thread-alive-p
	    thread-condition-variables-not-available ; not documented
            thread-holds-lock-p
            thread-name
            thread-whostate
            thread-yield
            unschedule-function
            with-lock-held
	    with-process-lock           ; renamed (to be removed soon)
            with-timeout)))

;;; ---------------------------------------------------------------------------
;;;  Warn if the Idle Process is not running on CMUCL

#+(and cmu mp)
(unless (member-if #'(lambda (process)
                       (string= "Idle Loop" (mp:process-name process)))
                   (mp:all-processes))
  (warn "You must start CMUCL's idle-loop process by calling~
         ~%~3t~s~
         ~%for ~s and other thread operations to function properly."
        '(mp::startup-idle-and-top-level-loops)
        'with-timeout))

;;; ---------------------------------------------------------------------------

#+(or clisp
      cormanlisp
      (and cmu (not mp)) 
      (and ecl (not threads))
      gcl
      (and sbcl (not sb-thread)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :threads-not-available *features*)
  ;; Remove the following soon!
  (pushnew :multiprocessing-not-available *features*))

;;; ---------------------------------------------------------------------------

#+threads-not-available
(defun threads-not-available (operation)
  (warn "Threads are not available in ~s running on ~s; ~s was used."
	(lisp-implementation-type) 
	(machine-type)
	operation))

;; Temporary backward name compatability:
#+threads-not-available
(defun multiprocessing-not-available (operation)
  (threads-not-available operation))

#+threads-not-available
(defun thread-condition-variables-not-available (operation)
  (warn "Thread condition variables are not available in ~s running on ~s; ~
        ~s was used."
	(lisp-implementation-type) 
	(machine-type)
	operation))

#+threads-not-available
(defun not-a-thread (thread)
  (error "~s is not a thread object" thread))

;;; ===========================================================================

(defun portable-threads-implementation-version ()
  "2.2")

;;; Added to *features* at the end of this file:
(defparameter *portable-threads-version-keyword* :portable-threads-2.2)

;;; ---------------------------------------------------------------------------

(defun print-portable-threads-herald ()
  (format t "~%;;; ~72,,,'-<-~>
;;;  Portable Threads Interface ~a~@
;;;
;;;    Developed and supported by the GBBopen Project (http:/GBBopen.org/)
;;;    (See http://GBBopen.org/downloads/LICENSE for license details.)
;;; ~72,,,'-<-~>~2%"
          (portable-threads-implementation-version)))
  
(eval-when (:load-toplevel)
  (print-portable-threads-herald))

;;; ===========================================================================
;;;  Thread-wait hook for non-threaded CLs (for example, GBBopen's
;;;  polling functions)

#+threads-not-available
(defvar *non-threaded-polling-function-hook* nil)

;;; ===========================================================================
;;;  With-timeout

#-(or allegro (and cmu mp))
(defmacro with-timeout ((seconds &body timeout-body) &body timed-body)
  #+lispworks
  (let ((timer-sym (gensym)))
    ;; Note that Lispworks runs the timer function in the process that is
    ;; running when the timeout occurs, so we have to use some cruft to get
    ;; back to the with-timeout process:
    `(catch 'with-timeout
       (let ((,timer-sym (mp:make-timer
                          #'(lambda (process)
                              (mp:process-interrupt 
                               process
                               #'(lambda ()
                                   (throw 'with-timeout
                                     (progn ,@timeout-body)))))
                          mp:*current-process*)))
         (mp:schedule-timer-relative ,timer-sym ,seconds)
         (unwind-protect (progn ,@timed-body)
           (mp:unschedule-timer ,timer-sym)))))
  #+(or digitool-mcl
        openmcl)
  (let ((timer-process-sym (gensym)))
    ;; No timers in OpenMCL, so we use sleep in a separate "timer" process:
    `(catch 'with-timeout
       (let ((,timer-process-sym
              (ccl:process-run-function 
                  "With-timeout timer"
                #'(lambda (process seconds)
                    (sleep seconds)
                    (ccl:process-interrupt
                     process
                     #'(lambda ()
                         (ignore-errors
                          (throw 'with-timeout
                            (progn ,@timeout-body))))))
                ccl:*current-process*
                ,seconds)))
         (ccl:process-allow-schedule)
         (unwind-protect (progn ,@timed-body)
           (ccl:process-kill ,timer-process-sym)
           (ccl:process-allow-schedule)))))
  #+sbcl
  (let ((timer-sym (gensym)))
    `(block with-timeout
       (let ((,timer-sym (sb-ext:make-timer
                          #'(lambda () (return-from with-timeout
                                         (progn ,@timeout-body))))))
         (sb-ext:schedule-timer ,timer-sym ,seconds)
         (unwind-protect (progn ,@timed-body)
           (sb-ext:unschedule-timer ,timer-sym)))))
  #+threads-not-available
  (declare (ignore seconds timeout-body timed-body))
  #+threads-not-available
  (progn
    (threads-not-available 'with-timeout)
    '(threads-not-available 'with-timeout)))    

;;; ===========================================================================
;;;   Locks

#+allegro
(defstruct (recursive-lock
            (:include mp:process-lock)
            (:copier nil)))

;; ECL's mp:lock is a built-in class, and although ECL allows us to subclass it,
;; ECL's mp:with-lock performs an exact (non-subclass) type match.  Also ECL
;; doesn't support lock owner inquiry, so we must build our own:
#+(and ecl threads) 
(progn
  (defclass nonrecursive-lock ()
    ((%lock :initform (mp:make-lock)
            :accessor %lock-of)
     (name :initarg :name :initform nil)
     (owner :initform nil :accessor lock-owner)))
  (defclass recursive-lock (nonrecursive-lock) 
    ()))

#+lispworks
(defstruct (recursive-lock
            (:include mp:lock)
            (:copier nil)))

;; OpenMCL only has a recursive lock object:
#+(or digitool-mcl
      openmcl)
(progn
  (defstruct (lock
              (:copier nil)
              (:constructor %make-lock))                
    (ccl-lock))
  (defstruct (recursive-lock 
              (:include lock)
              (:copier nil)
              (:constructor %make-recursive-lock))))

#+(and sbcl sb-thread)
(defstruct (recursive-lock 
            (:include sb-thread:mutex))
  (:copier nil))

#+threads-not-available
(progn
  (defstruct (lock (:copier nil))
    (count 0 :type fixnum)
    (name "Lock" :type string))
  (defstruct (recursive-lock
              (:include lock)
              (:copier nil))))

;;; ---------------------------------------------------------------------------
;;; It would have been great if various CL's lock and condition-variable
;;; objects were CLOS classes.  Without multiple inheritance, we have to hack
;;; a delegated lock-extraction dispatch level into with-lock-held:

(defgeneric %%get-lock%% (obj))

(defmethod %%get-lock%% (obj)
  ;; Regular lock & recursive-lock objects
  obj)

;;; ---------------------------------------------------------------------------

(defun wrong-lock-type-error (lock needed-lock-type operator)
  (error "A ~a lock is needed by ~s, a ~s was supplied"
         needed-lock-type
         operator
         (type-of lock)))
         
;;; ---------------------------------------------------------------------------

#+(or allegro
      digitool-mcl
      lispworks
      openmcl)
(defun recursive-lock-attempt-error (lock)
  (error "A recursive attempt was made to hold lock ~s"
         lock))
         
#+threads-not-available
(defun non-threaded-lock-deadlock-error (lock)
  (error "Attempt to grab the locked lock ~s on a non-threaded Common Lisp"
         lock))
  
;;; ---------------------------------------------------------------------------
;;;   Make-lock

#-(or lispworks                         ; simply imported
      threads-not-available
      cormanlisp)                       ; CLL 3.0 can't handle this one
(defun make-lock (&key name)
  #+allegro
  (mp:make-process-lock :name name)
  #+(and cmu mp)
  (mp:make-lock name :kind ':error-check)
  #+(and ecl threads)
  (make-instance 'nonrecursive-lock :name name)
  #+(or digitool-mcl
        openmcl)
  (%make-lock :ccl-lock (ccl:make-lock name))
  #+(and sbcl sb-thread)
  (sb-thread:make-mutex :name name)
  #+scl
  (mp:make-lock name :type :recursive))

;;; ---------------------------------------------------------------------------
;;;   Make-recursive-lock

#-(or allegro 
      lispworks
      (and sbcl sb-thread)
      threads-not-available)
(defun make-recursive-lock (&key name)
  #+(and cmu mp)
  (mp:make-lock name)
  #+(and ecl threads)
  (make-instance 'recursive-lock :name name)
  #+(or digitool-mcl
        openmcl)
  (%make-recursive-lock :ccl-lock (ccl:make-lock name))
  #+scl
  (mp:make-lock name :type :recursive))
  
;; Temporary backward name compatability:
(defun make-process-lock (&rest args)
  (declare (dynamic-extent args))
  (apply #'make-recursive-lock args))
#-full-safety
(define-compiler-macro make-process-lock (&rest args)
  `(make-recursive-lock ,@args))

;;; ---------------------------------------------------------------------------
;;;   With-Lock-held

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-lock-held ((lock &key (whostate "With Lock Held"))
                            &body body)
    #+(or (and ecl threads)
	  (and sbcl sb-thread)
          threads-not-available)
    (declare (ignore whostate))
    (let ((lock-sym (gensym)))
      `(let ((,lock-sym (%%get-lock%% ,lock)))
         #+allegro
         (progn
           ;; Allegro's mp:with-process-lock doesn't evaluate its :norecursive
           ;; option, so we roll our own recursive check:
           (when (and (not (recursive-lock-p ,lock-sym))
                      (eq system:*current-process*
                          (mp:process-lock-locker ,lock-sym)))
             (recursive-lock-attempt-error ,lock-sym))                     
           (mp:with-process-lock 
               (,lock-sym :norecursive nil
                          :whostate ,whostate)
             ,@body))
         #+(and cmu mp)
         (mp:with-lock-held (,lock-sym ,whostate) ,@body) 
         #+(and ecl threads)
         (mp:with-lock ((%lock-of ,lock-sym)) ,@body)
         #+lispworks
         (progn
           (when (and (not (recursive-lock-p ,lock-sym))
                      (eq mp:*current-process* 
                          (mp:lock-owner ,lock-sym)))
             (recursive-lock-attempt-error ,lock-sym))
           (mp:with-lock (,lock-sym ,whostate) 
             ,@body))
         #+(or digitool-mcl
               openmcl)
         (let ((.ccl-lock. (and (lock-p ,lock-sym)
                                (lock-ccl-lock (the lock ,lock-sym)))))
           (when (and (not (recursive-lock-p ,lock-sym))
                      (eq ccl:*current-process*
                          (ccl::%%lock-owner .ccl-lock.)))
             (recursive-lock-attempt-error ,lock-sym))
           (ccl:with-lock-grabbed (.ccl-lock. ,whostate)
             ,@body))
         ;; sb-thread:with-recursive-lock is heavy handed, we roll our own
         ;; with sb-thread:with-mutex (non-recursive) instead:
         #+(and sbcl sb-thread)
         (flet ((body-fn () ,@body))
           (if (and (recursive-lock-p ,lock-sym)
                    (eq (sb-thread:mutex-value ,lock-sym)
                        sb-thread::*current-thread*))
               (body-fn)
               (sb-thread:with-mutex (,lock-sym) (body-fn))))
         #+scl
         `(mp:with-lock-held (,lock-sym ,whostate) ,@body) 
         ;; Note that polling functions complicate non-threaded CL locking;
         ;; the following does not deal with polling functions:
         #+threads-not-available
         (cond
          ;; The lock is available:
          ((or (recursive-lock-p ,lock-sym)
               (zerop (the fixnum (lock-count ,lock-sym))))
           (unwind-protect (progn (incf (the fixnum (lock-count ,lock-sym)))
                                  ,@body)
             (decf (the fixnum (lock-count ,lock-sym)))))
          ;; Deadlocked:
          (t (non-threaded-lock-deadlock-error ,lock-sym)))))))

;; Temporary backward name compatability:
(defmacro with-process-lock (&rest args)
  `(with-lock-held ,@args))

;;; ---------------------------------------------------------------------------

(defun thread-holds-lock-p (lock)
  (let ((lock (%%get-lock%% lock)))
    #+allegro
    (eq (mp:process-lock-locker lock) system:*current-process*)
    #+(and cmu mp)
    (eq (mp::lock-process lock) mp:*current-process*)
    #+(and ecl threads)
    nil                                 ; need to find lock owner
    #+lispworks
    (eq (mp:lock-owner lock) mp:*current-process*)
    #+(or digitool-mcl 
          openmcl)
    (eq (ccl::%%lock-owner (lock-ccl-lock lock)) ccl:*current-process*)
    #+(and sbcl sb-thread)
    (eq (sb-thread:mutex-value lock) sb-thread:*current-thread*)
    ;; Note that polling functions complicate non-threaded CL locking;
    ;; the following does not deal with polling functions:
    #+threads-not-available
    (plusp (the fixnum (lock-count lock)))))

#-full-safety
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-compiler-macro thread-holds-lock-p (lock)
    (let ((lock-sym (gensym)))
      `(let ((,lock-sym (%%get-lock%% ,lock)))
         #+allegro
         (eq (mp:process-lock-locker ,lock-sym)
             system:*current-process*)
         #+(and cmu mp)
         (eq (mp::lock-process ,lock-sym)
             mp:*current-process*)
         #+(and ecl threads)
         nil                            ; need to find lock owner
         #+lispworks
         (eq (mp:lock-owner ,lock-sym)
             mp:*current-process*)
         #+(or digitool-mcl 
               openmcl)
         (eq (ccl::%%lock-owner (lock-ccl-lock ,lock-sym))
             ccl:*current-process*)
         #+(and sbcl sb-thread)
         (eq (sb-thread:mutex-value ,lock-sym)
             sb-thread:*current-thread*)
         #+threads-not-available
         (plusp (the fixnum (lock-count ,lock-sym)))))))

;;; ===========================================================================
;;;   As-atomic-operation 
;;;
;;;  (used to implement atomic operations; for very brief operations only)
;;;
;;; We use native without-scheduling on Allegro, CMUCL/mp, and SCL, and we use
;;; native without-interrupts on Digitool MCL and Lispworks.  
;;;
;;; OpenMCL's without-interrupts doesn't control thread scheduling, so we have
;;; to use a lock.
;;;
;;; The documentation of ECL/threads's without-interrupts is unspecific about
;;; it disabling all thread scheduling, so we don't use it.

#-(or allegro
      (and cmu mp)
      digitool-mcl
      lispworks
      scl)
(defvar *atomic-operation-lock* (make-lock :name "Atomic operation"))

#-(or allegro
      (and cmu mp)
      digitool-mcl
      lispworks
      scl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro as-atomic-operation (&body body)
    `(with-lock-held (*atomic-operation-lock*)
       ,@body)))

#+(or allegro
      (and cmu mp)
      digitool-mcl 
      lispworks
      scl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro as-atomic-operation (&body body)
    `(#+allegro mp:without-scheduling
      #+(and cmu mp) mp:without-scheduling
      #+digitool-mcl ccl:without-interrupts
      #+lispworks mp:without-preemption
      #+scl mp:without-scheduling
      ,@body)))

;;; ===========================================================================
;;;   Atomic Operations (defined here unless imported from the CL
;;;   implementation)

#-(or (and cmu mp) scl)
(defmacro atomic-push (value place)
  `(as-atomic-operation (push ,value ,place)))

#-scl
(defmacro atomic-pushnew (value place &rest args)
  `(as-atomic-operation (pushnew ,value ,place ,@args)))

#+scl
(defmacro atomic-pushnew (value place &rest args)
  (let ((list (gensym)))
    (ext:once-only ((value value))
      `(kernel:with-atomic-modification (,list ,place)
	 (if (member ,value ,list ,@args)
	     ,list
	     (cons ,value ,list))))))

#-(or (and cmu mp) scl)
(defmacro atomic-pop (place)
  `(as-atomic-operation (pop ,place)))

#-(or (and cmu mp) scl)
(defmacro atomic-incf (place &optional (delta 1))
  `(as-atomic-operation (incf ,place ,delta)))

#-(or (and cmu mp) scl)
(defmacro atomic-decf (place &optional (delta 1))
  `(as-atomic-operation (decf ,place ,delta)))

(defmacro atomic-delete (item place &rest args &environment env)
  #-scl
  (if (symbolp place)
      `(as-atomic-operation
	 (setf ,place (delete ,item ,place ,@args)))
      (multiple-value-bind (vars vals store-vars writer-form reader-form)
	  (get-setf-expansion place env)
	(let ((item-var (gensym)))
	  `(as-atomic-operation
	     (let* ((,item-var ,item)
		    ,@(mapcar #'list vars vals)
		    (,(first store-vars)
		     (delete ,item-var ,reader-form ,@args)))
	       ,writer-form)))))
  #+scl
  (let ((list (gensym)))
    (ext:once-only ((item item))
      `(kernel:with-atomic-modification (,list ,place)
         ;; Question for dtc: Why is remove used rather than delete?
	 (remove ,item ,list ,@args)))))

(defmacro atomic-flush (place)
  ;;; Set place to nil, returning the original value:
  #-scl
  `(as-atomic-operation (prog1 ,place (setf ,place nil)))
  #+scl
  `(loop
     (let ((value ,place))
       (when (eq (kernel:setf-conditional ,place value nil) value)
         (return value)))))

;;; ===========================================================================
;;;   Current-Thread (returns nil on CLs without threads)

(defun current-thread ()
  #+allegro
  mp:*current-process*
  #+(and cmu mp)
  mp:*current-process*
  #+(and ecl threads)
  mp:*current-process*
  #+digitool-mcl
  ccl:*current-process*
  #+lispworks
  mp:*current-process*
  #+openmcl
  ccl:*current-process*
  #+(and sbcl sb-thread)
  sb-thread:*current-thread*
  #+scl
  (mp:current-process)
  #+threads-not-available
  nil)

#-full-safety
(define-compiler-macro current-thread ()
  #+allegro
  'mp:*current-process*
  #+(and cmu mp)
  'mp:*current-process*
  #+digitool-mcl
  'ccl:*current-process*
  #+lispworks
  'mp:*current-process*
  #+openmcl
  'ccl:*current-process*
  #+(and sbcl sb-thread)
  'sb-thread:*current-thread*
  #+scl
  '(mp:current-process)
  #+threads-not-available
  nil)
 
;; Temporary backward name compatability:
(defun current-process ()
  (current-thread))
#-full-safety
(define-compiler-macro current-process ()
  '(current-thread))
 
;;; ===========================================================================
;;;   All-Threads (returns nil on CLs without threads)

(defun all-threads ()
  #+allegro
  mp:*all-processes*
  #+(and cmu mp)
  (mp:all-processes)
  #+digitool-mcl
  ccl:*all-processes*
  #+(and ecl threads)
  (mp:all-processes)
  #+lispworks
  (mp:list-all-processes)
  #+openmcl
  (ccl:all-processes)
  #+(and sbcl sb-thread)
  sb-thread::*all-threads*
  #+scl
  (mp:all-processes)
  #+threads-not-available
  nil)
  
#-full-safety
(define-compiler-macro all-threads ()
  #+allegro
  'mp:*all-processes*
  #+(and cmu mp)
  '(mp:all-processes)
  #+digitool-mcl
  'ccl:*all-processes*
  #+(and ecl threads)
  '(mp:all-processes)
  #+lispworks
  '(mp:list-all-processes)
  #+openmcl
  '(ccl:all-processes)
  #+(and sbcl sb-thread)
  'sb-thread::*all-threads*
  #+scl
  '(mp:all-processes)
  #+threads-not-available
  nil)

;; Temporary backward name compatability:
(defun all-processes ()
  (all-threads))
#-full-safety
(define-compiler-macro all-processes ()
  '(all-threads))
 
;;; ---------------------------------------------------------------------------
;;;   Threadp

(defun threadp (obj)
  #+allegro
  (mp:process-p obj)
  #+(and cmu mp)
  (mp:processp obj)
  #+digitool-mcl
  (ccl::processp obj)
  #+(and ecl threads)
  (typep obj 'mp:process)
  #+lispworks
  (mp:process-p obj)
  #+openmcl
  (ccl::processp obj)
  #+(and sbcl sb-thread)
  (sb-thread::thread-p obj)
  #+scl
  (mp:processp obj)
  #+threads-not-available
  (declare (ignore obj))
  #+threads-not-available
  nil)

#-full-safety
(define-compiler-macro threadp (obj)
  #+allegro
  `(mp:process-p ,obj)
  #+(and cmu mp)
  `(mp:processp ,obj)
  #+digitool-mcl
  `(ccl::processp ,obj)
  #+(and ecl threads)
  `(typep ,obj 'mp:process)
  #+lispworks
  `(mp:process-p ,obj)
  #+openmcl
  `(ccl::processp ,obj)
  #+(and sbcl sb-thread)
  `(sb-thread::thread-p ,obj)
  #+scl
  `(mp:processp ,obj)
  #+threads-not-available
  (declare (ignore obj))
  #+threads-not-available
  nil)

;; Temporary backward name compatability:
(defun processp (obj)
  (threadp obj))
#-full-safety
(define-compiler-macro processp (obj)
  `(threadp ,obj))
 
;;; ---------------------------------------------------------------------------
;;;   Thread-alive-p

#-(and sbcl sb-thread)
(defun thread-alive-p (obj)
  #+allegro
  (mp:process-alive-p obj)
  #+(and cmu mp)
  (mp:process-alive-p obj)
  #+digitool-mcl
  (ccl::process-active-p obj)
  #+(and ecl threads)
  (mp:process-active-p obj)
  #+lispworks
  (mp:process-alive-p obj)
  #+openmcl
  (ccl::process-active-p obj)
  #+scl
  (mp:process-alive-p obj)
  #+threads-not-available
  (declare (ignore obj))
  #+threads-not-available
  nil)

#-(or full-safety threads-not-available (and sbcl sb-thread))
(define-compiler-macro thread-alive-p (obj)
  #+allegro
  `(mp:process-alive-p ,obj)
  #+(and cmu mp)
  `(mp:process-alive-p ,obj)
  #+digitool-mcl
  `(ccl::process-active-p ,obj)
  #+(and ecl threads)
  `(mp:process-active-p ,obj)
  #+lispworks
  `(mp:process-alive-p ,obj)
  #+openmcl
  `(ccl::process-active-p ,obj)
  #+scl
  `(mp:process-alive-p ,obj)
  #+threads-not-available
  (declare (ignore obj))
  #+threads-not-available
  nil)

;;; ---------------------------------------------------------------------------
;;;   Thread-name

#-(and sbcl sb-thread)
(defun thread-name (thread)
  #+allegro
  (mp:process-name thread)
  #+(and cmu mp)
  (mp:process-name thread)
  #+digitool-mcl
  (ccl:process-name thread)
  #+(and ecl threads)
  (mp:process-name thread)
  #+lispworks
  (mp:process-name thread)
  #+openmcl
  (ccl:process-name thread)
  #+scl
  (mp:process-name thread)
  #+threads-not-available
  (not-a-thread thread))

#-(or full-safety threads-not-available (and sbcl sb-thread))
(define-compiler-macro thread-name (thread)
  #+allegro
  `(mp:process-name ,thread)
  #+(and cmu mp)
  `(mp:process-name ,thread)
  #+digitool-mcl
  `(ccl:process-name ,thread)
  #+(and ecl threads)
  `(mp:process-name ,thread)
  #+lispworks
  `(mp:process-name ,thread)
  #+openmcl
  `(ccl:process-name ,thread)
  #+scl
  `(mp:process-name ,thread))

;; Temporary backward name compatability:
(defun process-name (thread)
  (thread-name thread))
#-full-safety
(define-compiler-macro process-name (thread)
  `(thread-name ,thread))

;;; ---------------------------------------------------------------------------

#-(and sbcl sb-thread)
(defun (setf thread-name) (name thread)
  #+allegro
  (setf (mp:process-name thread) name)
  #+(and cmu mp)
  (setf (mp:process-name thread) name)
  #+digitool-mcl
  (setf (ccl:process-name thread) name)
  #+(and ecl threads)
  (setf (mp:process-name thread) name)
  #+lispworks
  (setf (mp:process-name thread) name)
  #+openmcl
  (setf (ccl:process-name thread) name)
  #+scl
  (setf (mp:process-name thread) name)
  #+threads-not-available
  (declare (ignore name))
  #+threads-not-available
  (not-a-thread thread))

;; Temporary backward name compatability:
(defun (setf process-name) (name thread)
  (setf (thread-name thread) name))

;;; ---------------------------------------------------------------------------
;;;   Thread-whostate (values and capabilities vary among CLs)

(defun thread-whostate (thread)
  #+allegro
  (mp:process-whostate thread)
  #+(and cmu mp)
  (mp:process-whostate thread)
  #+digitool-mcl
  (ccl:process-whostate thread)
  ;; We fake a basic whostate for ECL/threads:
  #+(and ecl threads)
  (if (mp:process-active-p thread) "Active" "Inactive")
  #+lispworks
  (mp:process-whostate thread)  
  #+openmcl
  (ccl:process-whostate thread)     
  ;; We fake a basic whostate for SBCL/sb-threads:
  #+(and sbcl sb-thread)
  (if (sb-thread:thread-alive-p thread) "Alive" "Dead")
  #+scl
  (mp:process-whostate thread)    
  #+threads-not-available
  (not-a-thread thread))

#+threads-not-available
(define-compiler-macro thread-whostate (thread)
  #+allegro
  `(mp:process-whostate ,thread)
  #+(and cmu mp)
  `(mp:process-whostate ,thread)
  #+digitool-mcl
  `(ccl:process-whostate ,thread)
  ;; We fake a basic whostate for ECL/threads:
  #+(and ecl threads)
  `(if (mp:process-active-p ,thread) "Active" "Inactive")
  #+lispworks
  `(mp:process-whostate ,thread)  
  #+openmcl
  `(ccl:process-whostate ,thread)     
  ;; We fake a basic whostate for SBCL/sb-threads:
  #+(and sbcl sb-thread)
  `(if (sb-thread:thread-alive-p ,thread) "Alive" "Dead")
  #+scl
  `(mp:process-whostate ,thread)
  #+threads-not-available
  `(not-a-thread ,thread))

;; Temporary backward name compatability:
(defun process-whostate (thread)
  (thread-whostate thread))
#-full-safety
(define-compiler-macro process-whostate (thread)
  `(thread-whostate ,thread))

;;; ===========================================================================
;;;   Spawn-Thread

(defun spawn-thread (name function &rest args)
  #-(or (and cmu mp) cormanlisp (and sbcl sb-thread))
  (declare (dynamic-extent args))
  #+allegro
  (apply #'mp:process-run-function name function args)
  #+(and cmu mp)
  (mp:make-process #'(lambda () (apply function args)) :name name)
  #+digitool-mcl
  (apply #'ccl:process-run-function name function args)
  #+(and ecl threads)
  (apply #'mp:process-run-function name function args)
  #+lispworks
  (apply #'mp:process-run-function name nil function args)
  #+openmcl
  (apply #'ccl:process-run-function name function args)
  #+(and sbcl sb-thread)
  (sb-thread:make-thread #'(lambda () (apply function args))
			 :name name)
  #+scl
  (mp:make-process #'(lambda () (apply function args)) :name name)
  #+threads-not-available
  (declare (ignore name function args))
  #+threads-not-available
  (threads-not-available 'spawn-process))

;; Temporary backward name compatability:
(defun spawn-process (&rest args)
  (declare (dynamic-extent args))
  (apply #'spawn-thread args))

;;; ---------------------------------------------------------------------------
;;;   Kill-Thread

(defun kill-thread (thread)
  #+allegro
  (mp:process-kill thread)
  #+(and cmu mp)
  (mp:destroy-process thread)
  #+digitool-mcl
  (ccl:process-kill thread)
  #+(and ecl threads)
  (mp:process-kill thread)
  #+lispworks
  (mp:process-kill thread)
  #+openmcl
  (ccl:process-kill thread)
  #+(and sbcl sb-thread)
  (sb-thread:terminate-thread thread)
  #+scl
  (mp:destroy-process thread)
  #+threads-not-available
  (declare (ignore thread))
  #+threads-not-available
  (threads-not-available 'kill-thread))

#-full-safety
(define-compiler-macro kill-thread (thread)
  #+allegro
  `(mp:process-kill ,thread)
  #+(and cmu mp)
  `(mp:destroy-process ,thread)
  #+digitool-mcl
  `(ccl:process-kill ,thread)
  #+(and ecl threads)
  `(mp:process-kill ,thread)
  #+lispworks
  `(mp:process-kill ,thread)
  #+openmcl
  `(ccl:process-kill ,thread)
  #+(and sbcl sb-thread)
  `(sb-thread:terminate-thread ,thread)
  #+scl
  `(mp:destroy-process ,thread)
  #+threads-not-available
  (declare (ignore thread))
  #+threads-not-available
  '(threads-not-available 'kill-thread))

;; Temporary backward name compatability:
(defun kill-process (thread)
  #+threads-not-available
  (declare (ignore thread))
  (kill-thread thread))
#-full-safety
(define-compiler-macro kill-process (thread)
  `(kill-thread ,thread))

;;; ===========================================================================
;;;   Thread-yield (runs *non-threaded-polling-function-hook* functions on
;;;                non-threaded CLs)

(defun thread-yield ()
  #+allegro
  (mp:process-allow-schedule)
  #+(and cmu mp)
  (mp:process-yield)
  #+digitool-mcl
  (ccl:process-allow-schedule)
  #+(and ecl threads)
  ;; Yield is not available, so we sleep 0:
  (sleep 0)
  #+lispworks
  (mp:process-allow-scheduling)
  #+openmcl
  (ccl:process-allow-schedule)
  #+(and sbcl sb-thread)
  ;; Yield is not available, so we sleep 0.05:  
  (sleep 0.05)
  #+scl
  (mp:process-yield)
  #+threads-not-available
  (mapc #'funcall *non-threaded-polling-function-hook*))

#-full-safety
(define-compiler-macro thread-yield ()
  #+allegro  
  '(mp:process-allow-schedule)
  #+(and cmu mp)
  '(mp:process-yield)
  #+digitool-mcl
  '(ccl:process-allow-schedule)
  #+(and ecl threads)
  ;; Yield is not available, so we sleep 0:
  '(sleep 0)
  #+lispworks
  '(mp:process-allow-scheduling)
  #+openmcl
  '(ccl:process-allow-schedule)
  #+(and sbcl sb-thread)
  ;; Yield is not available, so we sleep 0.05:  
  (sleep 0.05)
  #+scl
  '(mp:process-yield)
  #+threads-not-available
  '(mapc #'funcall *non-threaded-polling-function-hook*))

;; Temporary backward name compatability:
(defun process-yield ()
  (thread-yield))
#-full-safety
(define-compiler-macro proces-yield ()
  '(thread-yield))
 
;;; ---------------------------------------------------------------------------
;;;  Run-in-thread

(defun run-in-thread (thread function &rest args)
  #-threads-not-available
  (declare (dynamic-extent args))
  #+allegro
  (apply #'multiprocessing:process-interrupt thread function args)
  #+(and cmu mp)
  (multiprocessing:process-interrupt thread 
                                     #'(lambda () (apply function args)))
  #+(and ecl threads)
  (mp:interrupt-process thread #'(lambda () (apply function args)))
  #+digitool-mcl
  (apply #'ccl:process-interrupt thread function args)
  #+lispworks
  (progn
    (apply #'mp:process-interrupt thread function args)
    ;; Help Lispworks be more aggressive in running function promptly:
    (mp:process-allow-scheduling))
  #+openmcl
  (apply #'ccl:process-interrupt thread function args)
  #+(and sbcl sb-thread)
  (sb-thread:interrupt-thread thread #'(lambda () (apply function args)))
  #+scl
  (multiprocessing:process-interrupt thread
				     #'(lambda () (apply function args)))
  #+threads-not-available
  (declare (ignore thread function args))
  #+threads-not-available
  (threads-not-available 'run-in-thread))
  
;; Temporary backward name compatability:
(defun run-in-process (&rest args)
  (declare (dynamic-extent args))
  (apply #'run-in-thread args))
#-full-safety
(define-compiler-macro run-in-process (&rest args)
  `(run-in-thread ,@args))
 
;;; ---------------------------------------------------------------------------
;;;   Symbol-value-in-thread

(defun symbol-value-in-thread (symbol thread)
  ;; Returns two values:
  ;;  1. the symbol value (or nil if it is unbound)
  ;;  2. true if the symbol is bound; nil otherwise
  ;; The global symbol value is returned if no thread-local value is
  ;; bound.
  #+allegro
  (multiple-value-bind (value boundp)
      (mp:symeval-in-process symbol thread)
    (if boundp
	(values value (eq boundp 't))
	(if (boundp symbol)
	    (values (symbol-value symbol) 't)
	    (values nil nil))))
  #+(and cmu mp)
  (let ((result nil))
    (mp:process-interrupt
     thread
     #'(lambda ()
         (setf result (if (boundp symbol)
                          `(,(symbol-value symbol) t)
                          '(nil nil)))))
    ;; Wait for result:
    (loop until result do (mp:process-yield))
    (apply #'values result))
  #+digitool-mcl
  (handler-case
      (let ((value (ccl:symbol-value-in-process symbol thread)))
	(if (eq value (ccl::%unbound-marker))
	    (values nil nil)
	    (values value 't)))
    (error (condition)
      (declare (ignore condition))
      (values nil nil)))
  #+(and ecl threads)
  (let ((result nil))
    (mp:interrupt-process
     thread
     #'(lambda () (setf result (if (boundp symbol)
                                   `(,(symbol-value symbol) t)
                                   '(nil nil)))))
    ;; Wait for result:
    (loop until result do (sleep 0))
    (apply #'values result))
  #+lispworks
  (mp:read-special-in-process thread symbol)
  #+openmcl
  (let ((value (ccl:symbol-value-in-process symbol thread)))
    (if (eq value (ccl::%unbound-marker))
	(values nil nil)
	(values value 't)))
  #+(and sbcl sb-thread)
  ;; We can't figure out how to use (sb-thread::symbol-value-in-thread
  ;; symbol thread-sap), so:
  (let ((result nil))
    (sb-thread:interrupt-thread 
     thread
     #'(lambda () (setf result
                        (if (boundp symbol)
                            `(,(symbol-value symbol) t)
                            '(nil nil)))))
    ;; Wait for result:
    (loop until result do (sleep 0.05))
    (apply #'values result))
  #+scl
  (handler-case 
      (values (kernel:thread-symbol-dynamic-value thread symbol) t)
    (unbound-variable (condition)
      (declare (ignore condition))
      (handler-case
	  (values (kernel:symbol-global-value symbol) t)
	(unbound-variable () (values nil nil)))))
  #+threads-not-available
  (declare (ignore thread))
  #+threads-not-available
  (if (boundp symbol)
      (values (symbol-value symbol) t)
      (values nil nil)))

;; Temporary backward name compatability:
(defun symbol-value-in-process (symbol thread)
  (symbol-value-in-thread symbol thread))
#-full-safety
(define-compiler-macro symbol-value-in-process (symbol thread)
  `(symbol-value-in-thread ,symbol ,thread))

;;; ===========================================================================
;;;   Hibernate/Awaken Thread
;;;
;;;  Hibernating threads need to be able to perform run-in-thread and
;;;  symbol-value-in-thread operations.  We also want to allow:
;;;      (with-timeout (n) (hibernate-thread))
;;;
;;;  Using process arrest-reasons is too powerful for these operations,
;;;  instead we use sleeping which works like a charm!

#-threads-not-available
(defun throwable-sleep-forever ()
  ;; Used in place of process-arrest-reasons in CL's that don't have
  ;; them or that don't mix with-timeout and arrested processes:
  (catch 'throwable-sleep-forever
    (sleep most-positive-fixnum)))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun awaken-throwable-sleeper (thread)
  (flet ((awake-fn ()
           (ignore-errors
            (throw 'throwable-sleep-forever nil))))
    (run-in-thread  thread #'awake-fn)))

;;; ---------------------------------------------------------------------------

(defun hibernate-thread ()
  #-threads-not-available
  (throwable-sleep-forever)
  #+threads-not-available
  (threads-not-available 'hibernate-thread))

;; Temporary backward name compatability:
(defun hibernate-process ()
  (hibernate-thread))
#-full-safety
(define-compiler-macro hibernate-process ()
  '(hibernate-thread))

;;; ---------------------------------------------------------------------------

(defun awaken-thread (thread)
  #-threads-not-available
  (awaken-throwable-sleeper thread)
  #+threads-not-available
  (declare (ignore thread))
  #+threads-not-available
  (threads-not-available 'awaken-thread))
  
;; Temporary backward name compatability:
(defun awaken-process (process)
  (awaken-thread process))
#-full-safety
(define-compiler-macro awaken-process (process)
  `(awaken-thread ,process))

;;; ===========================================================================
;;;  Allegro-style gates are deprecated and will be removed soon.
;;;  Use condition variables instead. 

#-allegro
(locally (declare (optimize (speed 3) (safety 0) 
			    (debug 0) (compilation-speed 0)))
  
  (defun make-gate (open)
    (let ((gate (make-array '(1) :element-type '(signed-byte 32))))
      (setf (aref gate 0)
            (if open 1 0))
      gate))
  
  (defun open-gate (gate)
    (declare (type (simple-array (signed-byte 32) (*)) gate))
    (setf (aref gate 0) 1))
  
  (defun close-gate (gate)
    (declare (type (simple-array (signed-byte 32) (*)) gate))
    (setf (aref gate 0) 0))
  
  (defun gate-open-p (gate)
    (declare (type (simple-array (signed-byte 32) (*)) gate))
    (not (zerop (the fixnum (aref gate 0))))))

;;; ===========================================================================
;;;   Condition variables

#+allegro
(defclass condition-variable ()
  ((lock :initarg :lock
         :initform (mp:make-process-lock :name "CV Lock")
         :reader condition-variable-lock)
   (queue :initform nil
          :accessor condition-variable-queue)))

#+(and cmu mp)
(defclass condition-variable ()
  ((lock :initarg :lock
         :initform (mp:make-lock "CV Lock" :kind ':error-check)
         :reader condition-variable-lock)
   (queue :initform nil
          :accessor condition-variable-queue)))

#+(and ecl threads)
(defclass condition-variable ()
  ((lock :initarg :lock
         :initform (make-lock :name "CV Lock")
         :reader condition-variable-lock)
   (queue :initform nil
          :accessor condition-variable-queue)))

#+lispworks
(defclass condition-variable ()
  ((lock :initarg :lock
         :initform (mp:make-lock :name "CV Lock")
         :reader condition-variable-lock)
   (queue :initform nil
          :accessor condition-variable-queue)))

#+(or digitool-mcl
      openmcl)
(defclass condition-variable ()
  ((lock :initarg :lock
         :initform (make-lock :name "CV Lock")
         :reader condition-variable-lock)
   (semaphore :initform (ccl:make-semaphore)
              :reader condition-variable-semaphore)
   (queue :initform nil
          :accessor condition-variable-queue)))

#+(and sbcl sb-thread)
(defclass condition-variable ()
  ((lock :initarg :lock
         :initform (sb-thread:make-mutex :name "CV Lock")
         :reader condition-variable-lock)
   (cv :initform (sb-thread:make-waitqueue)
       :reader condition-variable-cv)))

#+threads-not-available
(defclass condition-variable ()
  ((lock :initarg :lock
         :initform (make-lock :name "CV Lock")
         :reader condition-variable-lock)))
  
(defmethod %%get-lock%% ((obj condition-variable))
  (condition-variable-lock obj))

;;; ---------------------------------------------------------------------------
;;; Syntactic sugar: make-condition-variable

(defun make-condition-variable (&rest initargs 
                                &key (class 'condition-variable)
                                &allow-other-keys)
  (declare (dynamic-extent initargs))
  (flet ((remove-property (plist indicator)
           (do* ((ptr plist (cddr ptr))
                 (ind (car ptr) (car ptr))
                 (result nil))
               ;; Only when nothing was found:
               ((null ptr) plist)
             (cond ((atom (cdr ptr))
                    (error "~s is a malformed property list." plist))
                   ((eq ind indicator)
                    (return (nreconc result (cddr ptr)))))
             (setq result (list* (second ptr) ind result)))))
    (apply #'make-instance class (remove-property initargs ':class))))

;;; ---------------------------------------------------------------------------

(defun condition-variable-lock-needed-error (condition-variable operation)
  (error "The condition-variable lock is required by ~s: ~s"
         operation
         condition-variable))

;;; ---------------------------------------------------------------------------

(defun condition-variable-wait (condition-variable)
  #-threads-not-available
  (let ((lock (condition-variable-lock condition-variable)))
    (unless (thread-holds-lock-p lock)
      (condition-variable-lock-needed-error
       condition-variable 'condition-variable-wait))
    #+allegro
    (progn
      (push system:*current-process* 
            (condition-variable-queue condition-variable))
      (mp::process-unlock lock)
      (throwable-sleep-forever)
      (mp::process-lock lock))
    #+(and cmu mp)
    (progn
      (push mp:*current-process* (condition-variable-queue condition-variable))
      (setf (mp::lock-process lock) nil)
      (throwable-sleep-forever)
      (mp::lock-wait lock nil))
    #+lispworks
    (progn
      (push mp:*current-process* (condition-variable-queue condition-variable))
      (mp:process-unlock lock)
      (throwable-sleep-forever)
      (mp:process-allow-scheduling)
      (mp:process-lock lock))
    #+(or digitool-mcl
          openmcl)
    (let ((ccl-lock (lock-ccl-lock lock)))
      (unwind-protect
          (progn
            (push ccl:*current-process* 
                  (condition-variable-queue condition-variable))
            (ccl:release-lock ccl-lock)
            (ccl:wait-on-semaphore 
             (condition-variable-semaphore condition-variable)))
        (ccl:grab-lock ccl-lock)))
    #+(and sbcl sb-thread)
    (sb-thread:condition-wait (condition-variable-cv condition-variable) lock))
  #+threads-not-available
  (declare (ignore condition-variable))
  #+threads-not-available
  (thread-condition-variables-not-available 'condition-variable-wait))

;;; ---------------------------------------------------------------------------

(defun condition-variable-wait-with-timeout (condition-variable seconds)
  #-threads-not-available
  (let ((lock (condition-variable-lock condition-variable)))
    (unless (thread-holds-lock-p lock)
      (condition-variable-lock-needed-error
       condition-variable 'condition-variable-wait-with-timeout))
    #+allegro
    (progn
      (push system:*current-process* 
            (condition-variable-queue condition-variable))
      (mp::process-unlock lock)
      (with-timeout 
          (seconds 
           (system:without-scheduling
             (setf (condition-variable-queue condition-variable)
                   (remove system:*current-process*
                           (condition-variable-queue condition-variable)))))
        (throwable-sleep-forever))
      (mp::process-lock lock))
    #+(and cmu mp)
    (progn
      (push mp:*current-process*
            (condition-variable-queue condition-variable))
      (setf (mp::lock-process lock) nil)
      (with-timeout 
          (seconds 
           (mp:without-scheduling
             (setf (condition-variable-queue condition-variable)
                   (remove mp:*current-process*
                           (condition-variable-queue condition-variable)))))
        (throwable-sleep-forever))
      (mp::lock-wait lock nil))
    #+lispworks
    (progn
      (push mp:*current-process* (condition-variable-queue condition-variable))
      (mp:process-unlock lock)
      (with-timeout 
          (seconds 
           (mp:without-preemption
             (setf (condition-variable-queue condition-variable)
                   (remove mp:*current-process*
                           (condition-variable-queue condition-variable)))))
        (throwable-sleep-forever))
      (mp:process-lock lock))
    #+(or digitool-mcl
          openmcl)
    (let ((ccl-lock (lock-ccl-lock lock)))
      (unwind-protect
          (progn
            (push ccl:*current-process* 
                  (condition-variable-queue condition-variable))
            (ccl:release-lock ccl-lock)
            (ccl:timed-wait-on-semaphore 
             (condition-variable-semaphore condition-variable) seconds))
        (ccl:grab-lock ccl-lock)
        (setf (condition-variable-queue condition-variable)
              (remove ccl:*current-process*
                      (condition-variable-queue condition-variable)))))
    #+(and sbcl sb-thread)
    (sb-ext:with-timeout seconds
      (handler-case (sb-thread:condition-wait 
                     (condition-variable-cv condition-variable) lock)
        (sb-ext:timeout () nil))))
  #+threads-not-available
  (declare (ignore condition-variable seconds))
  #+threads-not-available
  (thread-condition-variables-not-available
   'condition-variable-wait-with-timeout))

;;; ---------------------------------------------------------------------------

(defun condition-variable-signal (condition-variable)
  (unless (thread-holds-lock-p (condition-variable-lock condition-variable))
    (condition-variable-lock-needed-error
     condition-variable 'condition-variable-signal))
  #+(or allegro
        (and cmu mp)
        lispworks)
  (let ((thread (pop (condition-variable-queue condition-variable))))
    (when thread 
      (awaken-throwable-sleeper thread)))
  #+(or digitool-mcl
        openmcl)
  (ccl:signal-semaphore (condition-variable-semaphore condition-variable))
  #+(and sbcl sb-thread)
  (sb-thread:condition-notify (condition-variable-cv condition-variable)))
  
;;; ---------------------------------------------------------------------------

(defun condition-variable-broadcast (condition-variable)
  (unless (thread-holds-lock-p (condition-variable-lock condition-variable))
    (condition-variable-lock-needed-error
     condition-variable 'condition-variable-broadcast))
  #+(or allegro
        (and cmu mp)
        lispworks)
  (let ((queue (condition-variable-queue condition-variable)))
    (setf (condition-variable-queue condition-variable) nil)
    (dolist (thread queue)
      (awaken-throwable-sleeper thread)))
  #+(or digitool-mcl
        openmcl)        
  (let ((queue-length (length (condition-variable-queue condition-variable)))
        (semaphore (condition-variable-semaphore condition-variable)))
    (dotimes (i queue-length)
      (ccl:signal-semaphore semaphore)))
  #+(and sbcl sb-thread)
  (sb-thread:condition-broadcast (condition-variable-cv condition-variable)))

;;; ===========================================================================
;;;   Process-Wait (our best approximation on several non-threaded CLs,
;;;                 ECL with threads, and SBCL with sb-thread)

#+(or threads-not-available (and ecl threads) (and sbcl sb-thread))
(defun process-wait (whostate function &rest args)
  (declare (ignore whostate))
  (loop until (apply function args)
      do (process-yield)))

#+scl
(defun process-wait (whostate function &rest args)
  (mp:process-wait whostate #'(lambda () (apply function args))))

;;; ---------------------------------------------------------------------------
;;;   Process-Wait-With-Timeout (our best approximation on several
;;;   non-threaded CLs and SBCL with sb-thread)

#+(or threads-not-available (and ecl threads) (and sbcl sb-thread))
(defun process-wait-with-timeout (whostate seconds function &rest args)
  (declare (ignore whostate))
  (let ((end-time (+ (get-internal-real-time) 
		     (* internal-time-units-per-second seconds))))
    (loop until (or (apply function args)
		    (> (get-internal-real-time) end-time)) 
	do (process-yield))))

#+scl
(defun process-wait-with-timeout (whostate seconds function &rest args)
  (mp:process-wait-with-timeout whostate seconds
   #'(lambda () (apply function args))))

;;; ===========================================================================
;;;  Scheduled Functions (built entirely on top of Portable Threads substrate)

(defstruct (scheduled-function
            (:constructor %make-scheduled-function (function name))
            (:copier nil))
  name
  function
  invocation-time
  repeat-interval
  verbose)

(defmethod print-object ((obj scheduled-function) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~s [" (scheduled-function-name obj))
        (pretty-invocation-time (scheduled-function-invocation-time obj)
                                stream)
        (format stream "]"))))

;;; ---------------------------------------------------------------------------

(defvar *month-name-vector* 
    (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun"
	    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;;; ---------------------------------------------------------------------------

(defun pretty-invocation-time (ut stream)
  (if ut
      (multiple-value-bind (isecond iminute ihour idate imonth iyear)
          (decode-universal-time ut)
        (multiple-value-bind (second minute hour date month year)
            (decode-universal-time (get-universal-time))
          (declare (ignore second minute hour))
          (cond 
           ;; today?
           ((and (= date idate)
                 (= month imonth)
                 (= year iyear))
            (format stream "~2,'0d:~2,'0d:~2,'0d"
                    ihour
                    iminute
                    isecond))
           ;; someday:
           (t (let ((imonth-name (svref *month-name-vector* (1- imonth))))
                (format stream "~a ~d, ~d ~2,'0d:~2,'0d:~2,'0d"
                        imonth-name
                        idate
                        iyear
                        ihour
                        iminute
                        isecond))))))
      (format stream "unscheduled")))
  
;;; ---------------------------------------------------------------------------

(defvar *scheduled-functions* nil)
(defvar *schedule-function-verbose* nil)
#-threads-not-available
(defvar *scheduled-functions-cv* (make-condition-variable))
(defvar *scheduled-function-scheduler-thread* nil)

;;; ---------------------------------------------------------------------------

(defun all-scheduled-functions ()
  ;;; Returns the (unprotected) list of scheduled scheduled-functions.
  *scheduled-functions*)

;;; ---------------------------------------------------------------------------

(defun make-scheduled-function (function &key                                      
                                         (name (and (symbolp function)
                                                    function)))
  #-threads-not-available
  (%make-scheduled-function function name)
  #+threads-not-available
  (declare (ignore name))
  #+threads-not-available
  (threads-not-available 'make-scheduled-function))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun invoke-scheduled-function (scheduled-function)
  (with-simple-restart (continue "Resume scheduled-function scheduling")
    (funcall (scheduled-function-function scheduled-function)
             scheduled-function)))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun scheduled-function-scheduler ()
  ;;; The scheduled-function scheduler (run by the
  ;;; scheduled-function-scheduler thread).
  (let ((scheduled-function-to-run nil))
    (loop
      (with-lock-held (*scheduled-functions-cv*)
        (cond 
         ;; nothing to schedule, wait until signaled:
         ((null *scheduled-functions*)
          (condition-variable-wait *scheduled-functions-cv*))
         ;; something to schedule:
         (t (let ((invocation-time (scheduled-function-invocation-time
                                    (first *scheduled-functions*)))
                  (now (get-universal-time)))
              (cond 
               ;; wait until the next scheduled function (or until signaled):
               ((> invocation-time now)
                (condition-variable-wait-with-timeout 
                 *scheduled-functions-cv*
                 (- invocation-time now))
                ;; recheck that it's time to run the first scheduled function
                ;; before running the scheduled-function (in case we have been
                ;; awakened due to a schedule change rather than due to the
                ;; scheduled time of the next scheduled-function):
                (let ((invocation-time (scheduled-function-invocation-time
                                        (first *scheduled-functions*)))
                      (now (get-universal-time)))
                  (when (<= invocation-time now)
                    (setf scheduled-function-to-run
                          (pop *scheduled-functions*)))))
               ;; no need to wait:
               (t (setf scheduled-function-to-run
                        (pop *scheduled-functions*))))))))
      ;; funcall the scheduled function (outside of the CV lock):
      (when scheduled-function-to-run
        (unwind-protect (invoke-scheduled-function scheduled-function-to-run)
          (let ((repeat-interval (scheduled-function-repeat-interval
                                  scheduled-function-to-run)))
            (cond 
             ;; reschedule, if a repeat interval was specified:
             (repeat-interval
              ;; The following keeps invocations closest to intended, but
              ;; leads to rapidly repeating "catch up" invocations if the
              ;; scheduler has been blocked or terminated/restarted:
              #+this-keeps-times-in-alignment
              (incf (scheduled-function-invocation-time
                     scheduled-function-to-run)
                    repeat-interval)
              ;; This version avoids "catch up" invocations, but can
              ;; drift over time:
              #-this-keeps-times-in-alignment
              (setf (scheduled-function-invocation-time
                     scheduled-function-to-run)
                    (+ (get-universal-time) repeat-interval))
              (when (or *schedule-function-verbose*
                        (scheduled-function-verbose scheduled-function-to-run))
                (format *trace-output* 
                        "~&;; Scheduling ~s at repeat-interval ~s...~%"
                        scheduled-function-to-run
                        repeat-interval)
                (force-output *trace-output*))
              (with-lock-held (*scheduled-functions-cv*)
                (insert-scheduled-function scheduled-function-to-run nil)))
             ;; otherwise, clear the invocation time:
             (t (setf (scheduled-function-invocation-time 
                       scheduled-function-to-run)
                      nil))))
          (setf scheduled-function-to-run nil))))))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun awaken-scheduled-function-scheduler ()
  ;;; Awaken the scheduled-function-scheduler thread due to a change.
  ;;; If the thread isn't alive, start (or restart) it up.
  (if (and (threadp *scheduled-function-scheduler-thread*)
           (thread-alive-p *scheduled-function-scheduler-thread*))
      (condition-variable-signal *scheduled-functions-cv*)
      (setf *scheduled-function-scheduler-thread*
            (spawn-thread "Scheduled-Function Scheduler"
                          'scheduled-function-scheduler))))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun insert-scheduled-function (scheduled-function verbose)
  ;;; Do the work of inserting a scheduled function into the list of
  ;;; *scheduled-functions*.  The *scheduled-function-cv* lock must be held
  ;;; when calling this function.
  (cond
   ;; empty list:
   ((null *scheduled-functions*)
    (when verbose
      (format *trace-output* 
              "~&;; Scheduling ~s as the next scheduled-function...~%"
              scheduled-function)
      (force-output *trace-output*))
    (setf *scheduled-functions* (list scheduled-function))
    ;; schedule it:
    (awaken-scheduled-function-scheduler))
   ;; find position in list:
   (t (let ((invocation-time
             (scheduled-function-invocation-time scheduled-function)))
        (cond
         ;; front insertion:
         ((< invocation-time (scheduled-function-invocation-time 
                              (car *scheduled-functions*)))
          (when verbose
            (format *trace-output* 
                    "~&;; Scheduling ~s as the next scheduled-function...~%"
                    scheduled-function)
            (force-output *trace-output*))
          (setf *scheduled-functions*
                (cons scheduled-function *scheduled-functions*))
          ;; schedule it:
          (awaken-scheduled-function-scheduler))
         ;; splice into the list:
         (t (when verbose
              (format *trace-output* 
                      "~&;; Adding ~s as a scheduled-function...~%"
                      scheduled-function)
              (force-output *trace-output*))
            (do ((sublist *scheduled-functions* (cdr sublist)))
                ((null (cdr sublist))
                 (setf (cdr sublist) (list scheduled-function)))
              (when (< invocation-time 
                       (scheduled-function-invocation-time (cadr sublist)))
                (setf (cdr sublist) (cons scheduled-function (cdr sublist)))
                (return)))))))))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun delete-scheduled-function (name-or-scheduled-function verbose)
  ;;; Do the work of deleting a scheduled function from the list of
  ;;; *scheduled-functions*.  The *scheduled-function-cv* lock must be held
  ;;; when calling this function.
  (let ((the-deleted-scheduled-function nil))
    (flet ((on-deletion (scheduled-function)
             (when verbose
               (format *trace-output* "~&;; Unscheduling ~s...~%"
                       scheduled-function)
               (force-output *trace-output*))
             ;; Clear the invocation and repeat-interval values:
             (setf (scheduled-function-invocation-time scheduled-function)
                   nil)
             (setf (scheduled-function-repeat-interval scheduled-function) 
                   nil)
             ;; Record the deleted function (which also returns true):
             (setf the-deleted-scheduled-function scheduled-function)))
      (setf *scheduled-functions* 
            (delete-if
             (if (scheduled-function-p name-or-scheduled-function)
                 #'(lambda (scheduled-function)
                     (when (eq scheduled-function name-or-scheduled-function)
                       (on-deletion scheduled-function)))
               #'(lambda (scheduled-function)
                   (when (equal (scheduled-function-name scheduled-function)
                                name-or-scheduled-function)
                       (on-deletion scheduled-function))))
             *scheduled-functions*)))
    ;; return the deleted scheduled-function (or nil, if unsuccessful):
    the-deleted-scheduled-function))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun schedule-function-internal (name-or-scheduled-function invocation-time
                                   repeat-interval verbose)
  (with-lock-held (*scheduled-functions-cv*)
    (let* ((next-scheduled-function (first *scheduled-functions*))
           (unscheduled-scheduled-function 
            (delete-scheduled-function name-or-scheduled-function verbose))
           (scheduled-function 
            (or unscheduled-scheduled-function name-or-scheduled-function)))
      (cond
       ((scheduled-function-p scheduled-function)
        (setf (scheduled-function-invocation-time scheduled-function)
              invocation-time)
        (setf (scheduled-function-repeat-interval scheduled-function)
              repeat-interval)
        (setf (scheduled-function-verbose scheduled-function) verbose)
        (insert-scheduled-function scheduled-function verbose)
        ;; awaken scheduler if this scheduled-function was the next to be
        ;; run and now it is not the next to be run:
        (when (and (eq next-scheduled-function scheduled-function)
                   (not (eq (first *scheduled-functions*) scheduled-function)))
          (awaken-scheduled-function-scheduler)))                   
       ;; unable to find the requested scheduled-function:
       (t (warn "Unable to find scheduled-function ~s."
                name-or-scheduled-function))))))

;;; ---------------------------------------------------------------------------

(defun schedule-function (name-or-scheduled-function invocation-time
                          &key repeat-interval
                               (verbose *schedule-function-verbose*))
  #-threads-not-available
  (progn
    (check-type invocation-time integer)
    (check-type repeat-interval (or null integer))
    (schedule-function-internal name-or-scheduled-function invocation-time
                                repeat-interval verbose)
    (values))
  #+threads-not-available
  (declare (ignore name-or-scheduled-function invocation-time repeat-interval
                   verbose))
  #+threads-not-available
  (threads-not-available 'schedule-function))

;;; ---------------------------------------------------------------------------

(defun schedule-function-relative (name-or-scheduled-function interval
                                   &key repeat-interval 
                                        (verbose *schedule-function-verbose*))
  ;;; Syntactic sugar that simply adds `interval' to the current time before
  ;;; scheduling the scheduled-function.
  #-threads-not-available
  (progn
    (check-type interval integer)
    (check-type repeat-interval (or null integer))
    (schedule-function-internal name-or-scheduled-function 
                                (+ (get-universal-time) interval)
                                repeat-interval verbose)
    (values))
  #+threads-not-available
  (declare (ignore name-or-scheduled-function interval repeat-interval
                   verbose))
  #+threads-not-available
  (threads-not-available 'schedule-function-relative))

;;; ---------------------------------------------------------------------------

(defun unschedule-function (name-or-scheduled-function 
                            &key (verbose *schedule-function-verbose*))
  #-threads-not-available
  (with-lock-held (*scheduled-functions-cv*)
    (let ((next-scheduled-function (first *scheduled-functions*)))
      (cond
       ;; unscheduled successfully: 
       ((delete-scheduled-function name-or-scheduled-function verbose)
        ;; awaken the scheduler if the next scheduled-function was the one
        ;; that was unscheduled:
        (unless (eq next-scheduled-function (first *scheduled-functions*))
          (awaken-scheduled-function-scheduler))
        ;; return t if unscheduled:
        't)
       ;; unable to find it:
       (t (warn "Scheduled-function ~s was not scheduled; no action taken."
                name-or-scheduled-function)))))
  #+threads-not-available
  (declare (ignore name-or-scheduled-function verbose))
  #+threads-not-available
  (threads-not-available 'unschedule-function))

;;; ---------------------------------------------------------------------------

(defun restart-scheduled-function-scheduler ()
  #-threads-not-available
  (if (and (threadp *scheduled-function-scheduler-thread*)
           (thread-alive-p *scheduled-function-scheduler-thread*))
      (format t "~%;; The scheduled-function scheduler is already running.~%")
      (with-lock-held (*scheduled-functions-cv*)
        (awaken-scheduled-function-scheduler)))
  #+threads-not-available
  (threads-not-available 'restart-scheduled-function-scheduler))

;;; ---------------------------------------------------------------------------
;;;  Handy utility to encode (hour minute second) time of day into a universal
;;;  time.  If that time has already passed, the next day is assumed.

(defun encode-time-of-day (hour minute second
                           &optional (universal-time (get-universal-time)))
  ;; get the decoded current time of day:
  (multiple-value-bind (current-second current-minute current-hour
                        date month year)
      (decode-universal-time universal-time)
    ;; substitute the supplied hour, minute, and second values:
    (let ((tentative-result
           (encode-universal-time second minute hour date month year)))
      (flet ((seconds-into-day (hour minute second)
               (the fixnum (+ (the fixnum (* (the fixnum hour) 3600))
                              (the fixnum (* (the fixnum minute) 60))
                              (the fixnum second)))))
        ;; if the time of day has already passed for today, assume
        ;; tomorrow is intended:
        (if (> (seconds-into-day current-hour current-minute current-second)
               (seconds-into-day hour minute second))
            (+ tentative-result #.(* 60 60 24))
            tentative-result)))))

;;; ===========================================================================
;;;  Periodic Functions (also built entirely on top of Portable Threads)

(defvar *periodic-function-verbose* nil)

;;; ---------------------------------------------------------------------------

(defun spawn-periodic-function (function interval 
                                &key (count nil)
                                     (name (and (symbolp function)
                                                function))
                                     (verbose *periodic-function-verbose*))
  #-threads-not-available
  (flet ((fn ()
           (let ((*periodic-function-verbose* verbose)
                 (*periodic-function-name* name))
             (declare (special *periodic-function-name*))
             (catch 'kill-periodic-function
               (loop
                 (when (and count (minusp (decf count)))
                   (return))
                 (sleep interval)
                 (with-simple-restart (continue "Resume periodic-function")
                   (funcall function))))
             (when *periodic-function-verbose*
               (format *trace-output* 
                       "~&;; Exiting periodic-function thread~@[ ~s~]~%"
                       name)
               (force-output *trace-output*)))))
    (when verbose
      (format *trace-output* 
              "~&;; Spawning periodic-function thread for~@[ ~s~]...~%"
              name)
      (force-output *trace-output*))
    (spawn-thread (format nil "Periodic Function~@[ ~a~]" name)
                  #'fn))
  #+threads-not-available
  (declare (ignore interval count name verbose))
  #+threads-not-available
  (threads-not-available 'spawn-periodic-function))

;;; ---------------------------------------------------------------------------

(defun kill-periodic-function ()
  #-threads-not-available
  (locally (declare (special *periodic-function-name*))
    (handler-case 
        (progn
          (when *periodic-function-verbose*
            (format *trace-output* "~&;; Killing periodic-function~@[ ~s~]...~%"
                    (and (boundp '*periodic-function-name*)
                         *periodic-function-name*))
            (force-output *trace-output*))
          (throw 'kill-periodic-function nil))
      (control-error ()
        (error "~s must be called within a periodic function"
               'kill-periodic-function))))
  #+threads-not-available
  (threads-not-available 'kill-periodic-function))

;;; ===========================================================================
;;;  Portable threads interface is fully loaded:

(pushnew :portable-threads *features*)
(pushnew *portable-threads-version-keyword* *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
