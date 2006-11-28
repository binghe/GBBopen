;;;; -*- Mode:Common-Lisp; Package:PORTABLE-THREADS; Syntax:common-lisp -*-
;;;; *-* File: /home/gbbopen/current/source/tools/portable-threads.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Nov 15 03:43:32 2006 *-*
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
;;; Copyright (C) 2003-2006, Dan Corkill <corkill@GBBopen.org> 
;;;
;;; Originally developed in conjunction with the GBBopen Project
;;; (http://GBBopen.org) and donated to the CL Gardeners portable threads
;;; initiative (http://wiki.alu.org/Portable_Threads).  (Released under the
;;; Apache 2.0 license, see http://GBBopen.org/downloads/LICENSE for license
;;; details.)
;;;
;;; Bug reports, suggestions, enhancements, and extensions should be sent to
;;; corkill@GBBopen.org.
;;;
;;; On-line documentation for these multiprocessing-interface entities is
;;; available at http://gbbopen.org/hyperdoc/index.html
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
;;;  06-11-05 Clean up best attempts for non-multiprocessing CLs.  (Corkill)
;;;  10-21-05 Added polling functions for non-multiprocessing CLs.  (Corkill)
;;;  12-17-05 Added process-wait-with-timeout.  (Corkill)
;;;  12-22-05 Removed without-interrupts support (incompatible with
;;;           preemptive scheduling models).  (Corkill)
;;;  12-27-05 Added process-name.  (Corkill)
;;;  01-02-06 Separated from GBBopen, moved polling-functions into separate
;;;           polling-functions.lisp file and module.  (Corkill)
;;;  01-12-06 Added as-atomic-operation support, but only as a mechanism
;;;           for implementing very brief atomic operations.  (Corkill)
;;;  05-08-06 Added support for the Scieneer CL. (dtc)
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
;;; Import multiprocessing symbols, as needed:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   #+allegro 
   '(mp:close-gate
     mp:gate-open-p
     mp:make-gate
     mp:open-gate
     mp:process-name
     mp:process-wait
     mp:process-wait-with-timeout
     mp:process-whostate)
   #+clisp
   '()
   #+(and cmu mp)
   '(mp:all-processes
     mp:atomic-decf
     mp:atomic-incf
     mp:atomic-pop
     mp:atomic-push
     mp:process-name
     mp:process-wait
     mp:process-wait-with-timeout
     mp:process-whostate
     mp:processp)
   #+(and cmu (not mp))
   '()
   #+cormanlisp
   '()
   #+digitool-mcl
   '(ccl:process-name
     ccl:process-wait
     ccl:process-wait-with-timeout
     ccl:process-whostate
     ccl::processp)
   #+(and ecl threads)
   '(mp:all-processes
     mp:process-name)
   #+(and ecl (not threads))
   '()
   #+gcl
   '()
   #+lispworks
   '(mp:process-name
     mp:process-wait
     mp:process-wait-with-timeout
     mp:process-whostate)
   #+openmcl
   '(ccl:all-processes
     ccl:make-semaphore
     ccl:process-name
     ccl:process-wait
     ccl:process-wait-with-timeout
     ccl:process-whostate
     ccl::processp
     ccl:signal-semaphore)
   #+(and sbcl sb-threads)
   '(sb-threads:make-semaphore
     sb-threads:signal-semaphore)
   #+(and sbcl (not sb-threads))
   '()
   #+scl
   '(mp:all-processes
     mp:atomic-decf
     mp:atomic-incf
     mp:atomic-pop
     mp:atomic-push
     mp:process-name
     mp:process-whostate
     mp:processp
     thread:make-cond-var
     thread:cond-var-wait
     thread:cond-var-timedwait
     thread:cond-var-signal
     thread:cond-var-broadcast)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*non-threaded-process-wait-hook* ; not documented
	    all-processes
	    as-atomic-operation
	    atomic-decf
	    atomic-delete
	    atomic-flush
	    atomic-incf
	    atomic-pop
	    atomic-push
	    atomic-pushnew
	    awaken-process		; deprecated, may be removed
	    close-gate
	    cond-var-wait
	    cond-var-timedwait
	    cond-var-signal
	    cond-var-broadcast
	    current-process
	    gate-open-p
	    hibernate-process		; deprecated, may be removed
	    kill-process
	    make-cond-var
	    make-gate
	    make-process-lock
	    make-semaphore		; not yet documented
	    multiprocessing-not-available ; not documented
	    open-gate
	    process-name
	    processp
	    process-wait
	    process-wait-with-timeout
	    process-whostate
	    process-yield
	    run-in-process
	    signal-semaphore		; not yet documented
	    spawn-process
	    symbol-value-in-process
	    thread-condition-variables-not-available ; not documented
	    thread-scheduling-not-available ; not documented
	    wait-on-semaphore		; not yet documented
	    wait-on-semaphore-with-timeout ; not yet documented
	    with-process-lock)))

;; Export cond-var entities (possible future interface entities):
#+scl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-cond-var
	    cond-var-wait
	    cond-var-timedwait
	    cond-var-signal
	    cond-var-broadcast)))

;;; ---------------------------------------------------------------------------

#+(or clisp
      cormanlisp
      (and cmu (not mp)) 
      (and ecl (not threads))
      gcl
      (and sbcl (not sb-thread)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :multiprocessing-not-available *features*))

#+(or (and ecl threads)
      (and sbcl sb-thread)
      scl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :thread-scheduling-not-available *features*))

#-scl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :thread-condition-variables-not-available *features*))

;;; ---------------------------------------------------------------------------

#+multiprocessing-not-available
(defun multiprocessing-not-available (operation)
  (warn "Multiprocessing is not available in ~s running on ~s; ~s was used."
	(lisp-implementation-type) 
	(machine-type)
	operation))

#+thread-scheduling-not-available
(defun thread-scheduling-not-available (operation)
  (warn "Thread scheduling is not available in ~s running on ~s; ~s was used."
	(lisp-implementation-type) 
	(machine-type)
	operation))

#+thread-condition-variables-not-available
(defun thread-condition-variables-not-available (operation)
  (warn "Thread condition variables are not available in ~s running on ~s; ~
        ~s was used."
	(lisp-implementation-type) 
	(machine-type)
	operation))

;;; ---------------------------------------------------------------------------
;;;  Process-Wait hook for non-multiprocessing CLs (for example, GBBopen's 
;;;  polling functions)

#+multiprocessing-not-available
(defvar *non-threaded-process-wait-hook* nil)

;;; ===========================================================================
;;;   Make-Process-Lock

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-process-lock (&key name)
    #+allegro
    (mp:make-process-lock :name name)
    #+(and cmu mp)
    (mp:make-lock name)
    #+digitool-mcl
    (ccl:make-lock name)
    #+(and ecl threads)
    (mp:make-lock :name name)    
    #+lispworks
    (mp:make-lock :name name)
    #+openmcl
    (ccl:make-lock name)
    #+(and sbcl sb-thread)
    (sb-thread:make-mutex :name name)
    #+scl
    (mp:make-lock name :type :recursive)
    #+multiprocessing-not-available
    (cons 'pseudo-lock name))
  
  #-(or full-safety cormanlisp)		; CLL 3.0 can't handle this one
  (define-compiler-macro make-process-lock (&key (name nil name-supplied-p))
    #+(or scl multiprocessing-not-available)
    (declare (ignore name-supplied-p))
    #+allegro
    `(mp:make-process-lock ,@(when name-supplied-p (list ':name name)))
    #+(and cmu mp)
    `(mp:make-lock ,@(when name-supplied-p (list name)))
    #+digitool-mcl
    `(ccl:make-lock ,@(when name-supplied-p (list name)))
    #+(and ecl threads)
    `(mp:make-lock ,@(when name-supplied-p (list ':name name)))
    #+lispworks
    `(mp:make-lock ,@(when name-supplied-p (list ':name name)))
    #+openmcl
    `(ccl:make-lock ,@(when name-supplied-p (list name)))
    #+(and sbcl sb-thread)
    `(sb-thread:make-mutex ,@(when name-supplied-p (list ':name name)))
    #+scl
    `(mp:make-lock ,name :type :recursive)
    #+multiprocessing-not-available
    `(cons 'pseudo-lock ',name)))
  
;;; ===========================================================================
;;;   With-Process-Lock (supports recursive locking)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-process-lock ((lock &key (whostate "With Process Lock"))
			       &body body)
    #+(or multiprocessing-not-available 
	  (and ecl threads)
	  openmcl 
	  (and sbcl sb-thread))
    (declare (ignore whostate))
    #+allegro
    `(mp:with-process-lock (,lock :whostate ,whostate) ,@body)
    #+(and cmu mp)
    `(mp:with-lock-held (,lock ,whostate) ,@body) 
    #+digitool-mcl
    `(ccl:with-lock-grabbed (,lock :whostate ,whostate) ,@body)
    #+(and ecl threads)
    `(mp:with-lock (,lock) ,@body)
    #+lispworks
    `(mp:with-lock (,lock ,whostate) ,@body)
    #+openmcl
    `(ccl:with-lock-grabbed (,lock) ,@body)
    ;; sb-thread:with-mutex does not allow recursive locking, 
    ;; with-recursive-lock does:
    #+(and sbcl sb-thread)
    `(sb-thread:with-recursive-lock (,lock) ,@body)
    #+scl
    `(mp:with-lock-held (,lock ,whostate) ,@body) 
    #+multiprocessing-not-available
    `(progn ,lock ,@body)))

;;; ===========================================================================
;;;   As-Atomic-Operation (used to implement Atomic Operations); for very brief
;;;   operations only
;;;
;;; We use native without-scheduling on Allegro, CMUCL/mp, and SCL, and we use
;;; native without-interrupts on Digitool MCL and Lispworks.  
;;;
;;; OpenMCL's without-interrupts doesn't control thread scheduling, so we have
;;; to use a process-lock.
;;;
;;; The documentation of ECL/threads's without-interrupts is unspecific about
;;; it disabling all thread scheduling, so we don't use it.

#-(or allegro (and cmu mp) lispworks scl)
(defvar *atomic-operation-lock* (make-process-lock :name "Atomic operation"))

#-(or allegro (and cmu mp) digitool-mcl lispworks scl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro as-atomic-operation (&body body)
    `(with-process-lock (*atomic-operation-lock*)
       ,@body)))

#+(or allegro (and cmu mp) digitool-mcl lispworks scl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro as-atomic-operation (&body body)
    `(#+allegro mp:without-scheduling
      #+(and cmu mp) mp:without-scheduling
      #+digitool-mcl ccl:without-interrupts
      #+lispworks mp:without-interrupts
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

#-scl
(defmacro atomic-delete (item place &rest args &environment env)
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
	       ,writer-form))))))

#+scl
(defmacro atomic-delete (item place &rest args)
  (let ((list (gensym)))
    (ext:once-only ((item item))
      `(kernel:with-atomic-modification (,list ,place)
	 (remove ,item ,list ,@args)))))

#-scl
(defmacro atomic-flush (place)
  ;;; Set place to nil, returning the original value:
  `(as-atomic-operation (prog1 ,place (setf ,place nil))))

#+scl
(defmacro atomic-flush (place)
  ;;; Set place to nil, returning the original value:
  `(loop
    (let ((value ,place))
      (when (eq (kernel:setf-conditional ,place value nil) value)
	(return value)))))

;;; ===========================================================================
;;;   Spawn-Process

(defun spawn-process (name function &rest args)
  #+multiprocessing-not-available
  (declare (ignore name function args))
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
  #+multiprocessing-not-available
  (multiprocessing-not-available 'spawn-process))

;;; ===========================================================================
;;;   Kill-Process

(defun kill-process (process)
  #+multiprocessing-not-available
  (declare (ignore process))
  #+allegro
  (mp:process-kill process)
  #+(and cmu mp)
  (mp:destroy-process process)
  #+digitool-mcl
  (ccl:process-kill process)
  #+(and ecl threads)
  (mp:process-kill process)
  #+lispworks
  (mp:process-kill process)
  #+openmcl
  (ccl:process-kill process)
  #+(and sbcl sb-thread)
  (sb-thread:terminate-thread process)
  #+scl
  (mp:destroy-process process)
  #+multiprocessing-not-available
  (multiprocessing-not-available 'kill-process))

;;; ===========================================================================
;;;   Current-Process (returns nil on CLs without multiprocessing)

(defun current-process ()
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
  #+multiprocessing-not-available
  nil)

#-full-safety
(define-compiler-macro current-process ()
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
  #+multiprocessing-not-available
  nil)
 
;;; ===========================================================================
;;;   All-Processes (returns nil on CLs without multiprocessing)

#-(or (and cmu mp) (and ecl threads) openmcl scl)
(defun all-processes ()
  #+allegro
  mp:*all-processes*
  #+digitool-mcl
  ccl:*all-processes*
  #+lispworks
  (mp:list-all-processes)
  #+(and sbcl sb-thread)
  sb-thread::*all-threads*
  #+multiprocessing-not-available
  nil)

#-(or full-safety (and cmu mp) (and ecl threads) openmcl scl)
(define-compiler-macro all-processes ()
  #+allegro
  'mp:*all-processes*
  #+digitool-mcl
  'ccl:*all-processes*
  #+lispworks
  '(mp:list-all-processes)
  #+(and sbcl sb-thread)
  'sb-thread::*all-threads*
  #+multiprocessing-not-available
  nil)
 
;;; ===========================================================================
;;;   Processp

#-(or (and cmu mp) digitool-mcl openmcl scl)
(defun processp (obj)
  #+multiprocessing-not-available
  (declare (ignore obj))
  #+allegro
  (mp:process-p obj)
  #+lispworks
  (mp:process-p obj)
  #+(and ecl threads)
  (typep obj 'mp:process)
  #+(and sbcl sb-thread)
  (sb-thread::thread-p obj)
  #+multiprocessing-not-available
  nil)

#-(or full-safety (and cmu mp) digitool-mcl openmcl scl)
(define-compiler-macro processp (obj)
  #+multiprocessing-not-available
  (declare (ignore obj))
  #+allegro
  `(mp:process-p ,obj)
  #+(and ecl threads)
  `(typep ,obj 'mp:process)
  #+lispworks
  `(mp:process-p ,obj)
  #+(and sbcl sb-thread)
  `(sb-thread::thread-p ,obj)
  #+multiprocessing-not-available
  nil)
 
;;; ===========================================================================
;;;   Process-name (mostly inherited from the CL versions)

#+multiprocessing-not-available
(defun not-a-process (process)
  (error "~s is not a process object" process))

#+(or multiprocessing-not-available (and sbcl sb-thread))
(defun process-name (process)
  #+(and sbcl sb-thread)
  (sb-thread::thread-name process)
  #+multiprocessing-not-available
  (not-a-process process))

#+(and sbcl sb-thread)
(defun (setf process-name) (name process)
  (setf (sb-thread::thread-name process) name))

#+(or multiprocessing-not-available (and sbcl sb-thread))
(define-compiler-macro process-name (process)
  #+(and sbcl sb-thread)
  `(sb-thread::thread-name ,process)
  #+multiprocessing-not-available
  `(not-a-process ,process))

;;; ===========================================================================
;;;   Process-whostate (inherited from CLs with whostate support); values and 
;;;   capabilities vary among CLs

#+(or multiprocessing-not-available (and ecl threads) (and sbcl sb-thread))
(defun process-whostate (process)
  ;; We fake a basic whostate for ECL/threads and SBCL/sb-thread:
  #+(and ecl threads)
  (if (mp:process-active-p process) "Active" "Inactive")
  #+(and sbcl sb-thread)
  (if (sb-thread:thread-alive-p process) "Alive" "Dead")
  #+multiprocessing-not-available
  (not-a-process process))

#+multiprocessing-not-available
(define-compiler-macro process-whostate (process)
  ;; No compiler macro for ECL/threads or SBCL/sb-thread, we use the full
  ;; function version (above)
  #+multiprocessing-not-available
  `(not-a-process ,process))

;;; ===========================================================================
;;;   Process-Yield (runs *non-threaded-process-wait-hook* functions on
;;;                  non-multiprocessing CLs)

(defun process-yield ()
  #+allegro
  (mp:process-allow-schedule)
  #+(and cmu mp)
  (mp:process-yield)
  #+digitool-mcl
  (ccl:process-allow-schedule)
  #+(and ecl threads)
  ;; Yield is not yet available, so we sleep 0:
  (sleep 0)
  #+lispworks
  (mp:process-allow-scheduling)
  #+openmcl
  (ccl:process-allow-schedule)
  #+(and sbcl sb-thread)
  (sb-thread:release-foreground)
  #+scl
  (mp:process-yield)
  #+multiprocessing-not-available
  (mapc #'funcall *non-threaded-process-wait-hook*))

#-full-safety
(define-compiler-macro process-yield ()
  #+allegro  
  '(mp:process-allow-schedule)
  #+(and cmu mp)
  '(mp:process-yield)
  #+digitool-mcl
  '(ccl:process-allow-schedule)
  #+(and ecl threads)
  ;; Yield is not yet available, so we sleep 0:
  '(sleep 0)
  #+lispworks
  '(mp:process-allow-scheduling)
  #+openmcl
  '(ccl:process-allow-schedule)
  #+(and sbcl sb-thread)
  '(sb-thread:release-foreground)
  #+scl
  '(mp:process-yield)
  #+multiprocessing-not-available
  '(mapc #'funcall *non-threaded-process-wait-hook*))

;;; ---------------------------------------------------------------------------
;;;   Process-Wait (our best approximation on several non-multiprocessing CLs,
;;;                 ECL with threads, and SBCL with sb-thread)

#+(or multiprocessing-not-available (and ecl threads) (and sbcl sb-thread))
(defun process-wait (whostate function &rest args)
  (declare (ignore whostate))
  (loop until (apply function args)
      do (process-yield)))

#+scl
(defun process-wait (whostate function &rest args)
  (mp:process-wait whostate #'(lambda () (apply function args))))

;;; ---------------------------------------------------------------------------
;;;   Process-Wait-With-Timeout (our best approximation on several
;;;   non-multiprocessing CLs and SBCL with sb-thread)

#+(or multiprocessing-not-available (and ecl threads) (and sbcl sb-thread))
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
;;;   Hibernate/Awaken (not available with ECL/threads or SBCL/sb-threads)
;;;
;;; Note that using run-in-process and symbol-value-in-process currently block
;;; on some CLs if process is hibernating.
;;;
;;; A process in OpenMCL cannot hibernate itself.
;;;
;;; Due to these issues, hibernate-process & awaken-process may soon
;;; be removed from the portable-threads interface.

#+lispworks
(defvar *hibernate/awaken-lock* (make-process-lock :name "Hibernate/Awaken"))

(defun hibernate-process (&optional (process (current-process)))
  #+(or multiprocessing-not-available thread-scheduling-not-available)
  (declare (ignore process))
  #+allegro
  (mp:process-add-arrest-reason process ':hibernate)
  #+(and cmu mp)
  (mp:process-add-arrest-reason process ':hibernate)
  #+digitool-mcl
  (ccl::process-enable-arrest-reason process :hibernate)
  #+lispworks
  (progn
    (with-process-lock (*hibernate/awaken-lock*)
      (pushnew ':hibernate (mp:process-arrest-reasons process) 
	       :test #'eq))
    (process-yield))
  #+openmcl
  (if (eq process ccl:*current-process*)
      ;; OpenMCL doesn't really support suspending oneself:
      (error "~s on the current-process is not supported in OpenMCL"
	     'hibernate-process)
      (ccl:process-suspend process))
  #+thread-scheduling-not-available
  (thread-scheduling-not-available 'hibernate-process)
  #+multiprocessing-not-available
  (multiprocessing-not-available 'hibernate-process))

;;; ---------------------------------------------------------------------------

(defun awaken-process (process)
  #+(or multiprocessing-not-available thread-scheduling-not-available)
  (declare (ignore process))
  #+allegro
  (mp:process-revoke-arrest-reason process ':hibernate)
  #+(and cmu mp)
  (mp:process-revoke-arrest-reason process ':hibernate)
  #+digitool-mcl
  (ccl::process-disable-arrest-reason process :hibernate)
  #+lispworks
  (with-process-lock (*hibernate/awaken-lock*)
    (setf (mp:process-arrest-reasons process)
	  (delete ':hibernate (mp:process-arrest-reasons process)
		  :test #'eq)))
  #+openmcl
  (ccl:process-resume process)
  #+thread-scheduling-not-available
  (thread-scheduling-not-available 'awaken-process)
  #+multiprocessing-not-available
  (multiprocessing-not-available 'awaken-process))
  
;;; ===========================================================================
;;;  Run in process

(defun run-in-process (process function &rest args)
  (declare (dynamic-extent args))
  #+multiprocessing-not-available
  (declare (ignore process function args))
  #+allegro
  (apply #'multiprocessing:process-interrupt process function args)
  #+(and cmu mp)
  (multiprocessing:process-interrupt process
				     #'(lambda () (apply function args)))
  #+(and ecl threads)
  (mp:interrupt-process process #'(lambda () (apply function args)))
  #+digitool-mcl
  (apply #'ccl:process-interrupt process function args)
  #+lispworks
  (apply #'mp:process-interrupt process function args)
  #+openmcl
  (apply #'ccl:process-interrupt process function args)
  #+(and sbcl sb-thread)
  (sb-thread:interrupt-thread process #'(lambda () (apply function args)))
  #+scl
  (multiprocessing:process-interrupt process
				     #'(lambda () (apply function args)))
  #+multiprocessing-not-available
  (multiprocessing-not-available 'run-in-process))
  
;;; ===========================================================================
;;;   Symbol-value-in-process

(defun symbol-value-in-process (symbol process)
  ;; Returns two values:
  ;;  1. the symbol value (or nil if it is unbound)
  ;;  2. true if the symbol is bound; nil otherwise
  ;; The global symbol value is returned if no process-local value is
  ;; bound.
  #+multiprocessing-not-available
  (declare (ignore symbol process))
  #+allegro
  (multiple-value-bind (value boundp)
      (mp:symeval-in-process symbol process)
    (if boundp
	(values value (eq boundp 't))
	(if (boundp symbol)
	    (values (symbol-value symbol) 't)
	    (values nil nil))))
  #+(and cmu mp)
  (let ((result nil))
    (multiprocessing:process-interrupt
     process 
     #'(lambda () 
	 (setq result
	   (if (boundp symbol)
	       `(,(symbol-value symbol) t)
	       '(nil nil)))))
    (process-wait "symbol-value-in-process" 
		  #'(lambda () result))
    (apply #'values result))
  #+digitool-mcl
  (handler-case
      (let ((value (ccl:symbol-value-in-process symbol process)))
	(if (eq value (ccl::%unbound-marker))
	    (values nil nil)
	    (values value 't)))
    (error (condition)
      (declare (ignore condition))
      (values nil nil)))
  #+(and ecl threads)
  (let ((result nil))
    (mp:interrupt-process
     process 
     #'(lambda () 
	 (setq result
	   (if (boundp symbol)
	       `(,(symbol-value symbol) t)
	       '(nil nil)))))
    (process-wait "symbol-value-in-process" 
		  #'(lambda () result))
    (apply #'values result))
  #+lispworks
  (mp:read-special-in-process process symbol)
  #+openmcl
  (let ((value (ccl:symbol-value-in-process symbol process)))
    (if (eq value (ccl::%unbound-marker))
	(values nil nil)
	(values value 't)))
  #+(and sbcl sb-thread)
  ;; We can't figure out how to use (sb-thread::symbol-value-in-thread
  ;; symbol thread-sap), so:
  (let ((result nil))
    (sb-thread:interrupt-thread 
     process 
     #'(lambda () 
	 (setq result
	   (if (boundp symbol)
	       `(,(symbol-value symbol) t)
	       '(nil nil)))))
    (process-wait "symbol-value-in-process" 
		  #'(lambda () result))
    (apply #'values result))
  #+scl
  (handler-case
      (values (kernel:thread-symbol-dynamic-value process symbol) t)
    (unbound-variable (condition)
      (declare (ignore condition))
      (handler-case
	  (values (kernel:symbol-global-value symbol) t)
	(unbound-variable (condition)
	  (declare (ignore condition))
	  (values nil nil)))))
  #+multiprocessing-not-available
  (multiprocessing-not-available 'symbol-value-in-process))

;;; ===========================================================================
;;;   Semaphores (a work in progress; not yet complete!)

(defun wait-on-semaphore (whostate semaphore)
  (declare (ignore whostate))
  #-openmcl
  (declare (ignore semaphore))
  #+openmcl (ccl:wait-on-semaphore semaphore)
  #+multiprocessing-not-available
  't)

;;; ---------------------------------------------------------------------------

(defun wait-on-semaphore-with-timeout (whostate semaphore seconds)
  (declare (ignore whostate))
  #-openmcl
  (declare (ignore semaphore seconds))
  #+openmcl (ccl:timed-wait-on-semaphore semaphore seconds)
  #+multiprocessing-not-available
  't)

;;; ===========================================================================
;;;   Gates (Allegro provides them already...); we provide a simple
;;;   implementations for other CLs (implementation-specific optimizations
;;;   could be made on some CLs).  
;;;
;;;   Gates are really just a hack to work around limitations of 'process-wait.
;;;   It is recommended that locks and condition variables be used for
;;;   synchronization, and that use of 'process-wait be avoided making the
;;;   gates work around obsolete. (dtc)
;;;

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
;;;
;;; Stubs for implementations not supporting condition variables.
;;; It is likely support could be added for SBCL.

#+thread-condition-variables-not-available
(progn

  (defun make-cond-var (&optional name)
    (declare (ignore name))
    (thread-condition-variables-not-available 'make-cond-var)
    nil)
  
  (defun cond-var-wait (cond-var lock &optional (whostate "Waiting"))
    (declare (ignore cond-var lock whostate))
    (thread-condition-variables-not-available 'cond-var-wait)
    (sleep 0.01)
    (values))
  
  (defun cond-var-timedwait (cond-var lock timeout
			     &optional (whostate "Waiting"))
    (declare (ignore cond-var lock timeout whostate))
    (thread-condition-variables-not-available 'cond-var-timedwait)
    (sleep 0.01)
    't)

  (defun cond-var-signal (cond-var)
    (declare (ignore cond-var))
    (thread-condition-variables-not-available 'cond-var-signal)
    nil)
  
  (defun cond-var-broadcast (cond-var)
    (declare (ignore cond-var))
    (thread-condition-variables-not-available 'cond-var-broadcast)
    nil))

;;; ===========================================================================
;;;  Portable Threads are loaded:

(pushnew :portable-threads *features*)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


