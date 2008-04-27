;;;; -*- Mode:Common-Lisp; Package:PORTABLE-THREADS-USER; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/portable-threads-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Sun Apr 27 14:03:07 2008 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                     Simple Portable Threads Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2005-2008, Dan Corkill <corkill@GBBopen.org>
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  08-21-05 File Created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package ':portable-threads-user) 
    (make-package ':portable-threads-user
                  :use '(:common-lisp :portable-threads))))

(in-package :portable-threads-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(common-lisp-user::*autorun-modules*)))

;; (Re-)define *autorun-modules* here, in case we are using this file
;; stand-alone...
(defvar *autorun-modules* nil)

;;; ---------------------------------------------------------------------------
;;;  Bindings used in thread tests:

(defparameter *w* nil)
(defparameter *x* 1)
(defparameter *y* 2)
(defparameter *z* 3)

;;; ---------------------------------------------------------------------------

(defstruct not-a-lock)

;;; ---------------------------------------------------------------------------

(defclass state-cv (condition-variable)
  ((state :initarg :state
          :initform nil
          :accessor state-of)))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro time-it (&body body)
    (let ((start-time-sym (gensym)))
      `(let ((,start-time-sym (get-internal-run-time)))
         ,@body
         (let ((run-time (/ (float (- (get-internal-run-time) ,start-time-sym))
                            (float internal-time-units-per-second))))
           (forced-format " ~s seconds" run-time)
           ;; return the run-time
           run-time)))))

;;; ---------------------------------------------------------------------------

(defun log-error (control-string &rest args)
  (declare (dynamic-extent args))
  (format t "~&;; ~73,,,'*<*~>~
             ~%;; *** ~?~
             ~&;; ~73,,,'*<*~>"
          control-string
          args))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro check-error-checking (form condition message)
    `(let ((form-completed nil))
       (handler-case (progn ,form
                            (setf form-completed 't))
         (,condition () nil))
       (when form-completed (log-error ,message)))))

;;; ---------------------------------------------------------------------------

(defun sleep-nearly-forever ()
  (sleep *nearly-forever-seconds*))

(defun sleepy-time ()
  ;; We sleep long enough for thread startup/cleanup/scheduling to occur:
  (sleep 0.5))

;;; ---------------------------------------------------------------------------

(defun forced-format (&rest args)
  (declare (dynamic-extent args))
  (apply #'format t args)
  (force-output))

;;; ---------------------------------------------------------------------------

(defun basic-lock-tests ()
  (forced-format "~&;; Performing basic lock tests...")
  (let ((nonrecursive-lock (make-lock :name "Nonrecursive"))
        (recursive-lock (make-recursive-lock :name "Recursive"))
        (cv (make-condition-variable))
        (not-a-lock (make-not-a-lock))
        (iterations (min 1000000 most-positive-fixnum))
        (start-real-time (get-internal-real-time)))
    (declare (fixnum iterations))
    ;; Try each type of locking:
    (when (thread-holds-lock-p nonrecursive-lock)
      (log-error 
       "Incorrect thread-holds-lock-p value on free nonrecursive lock"))
    (with-lock-held (nonrecursive-lock
                     :whostate "Waiting on nonrecursive lock")
      (unless (thread-holds-lock-p nonrecursive-lock)
        (log-error "Incorrect thread-holds-lock-p value on held lock")))
    (when (thread-holds-lock-p recursive-lock)
      (log-error "Incorrect thread-holds-lock-p value on free recursive lock"))
    (with-lock-held (recursive-lock 
                     :whostate "Waiting on recursive lock")
      (unless (thread-holds-lock-p recursive-lock)
        (log-error "Incorrect thread-holds-lock-p value on recursive lock")))
    (when (thread-holds-lock-p cv)
      (log-error 
       "Incorrect thread-holds-lock-p value on condition-variable lock"))
    (with-lock-held (cv :whostate "Waiting on condition-variable lock")
      (unless (thread-holds-lock-p cv)
        (log-error 
         "Incorrect thread-holds-lock-p value on condition-variable lock")))
    ;; Lock timing tests:
    (forced-format "~&;;   Timing ~s nonrecursive-lock acquisitions..."
                   iterations)
    (time-it 
     (dotimes (i iterations)
       (declare (fixnum i))
       (with-lock-held (nonrecursive-lock
                        :whostate "Waiting on nonrecursive lock")
         nil)))
    (forced-format "~&;;   Timing ~s recursive-lock acquisitions..." 
                   iterations)
    (time-it 
     (dotimes (i iterations)
       (declare (fixnum i))
       (with-lock-held (recursive-lock
                        :whostate "Waiting on recursive lock")
         nil)))
    (forced-format "~&;;   Timing ~s condition-variable lock acquisitions..."
                   iterations)
    (time-it 
     (dotimes (i iterations)
       (declare (fixnum i))
       (with-lock-held (cv :whostate "Waiting on Condition Variable")
         nil)))
    (forced-format "~&;;   Checking with a non-lock object...")
    ;; Incorrect lock type:
    (check-error-checking
     (with-lock-held (not-a-lock) nil)
     error
     "With-lock-held did not fail when given a non lock")
    ;; Check recursive locking:
    (forced-format 
     "~&;;   Testing recursive locking with a recursive lock...~%")
    (let ((counter 0))
      (with-lock-held (recursive-lock :whostate "Level 1")
        (incf counter)
        (with-lock-held (recursive-lock :whostate "Level 2")
          (incf counter)
          (with-lock-held (recursive-lock :whostate "Level 3")
            (incf counter))))
      (unless (= counter 3)
	(log-error "Incorrect recursive-lock counter value (should be 3): ~s" 
                   counter)))
    (forced-format
     "~&;;   Checking recursive locking with a non-recursive lock...~%")
    (check-error-checking
     (with-lock-held (nonrecursive-lock :whostate "Level 1")
       (with-lock-held (nonrecursive-lock :whostate "Level 2")
         (with-lock-held (nonrecursive-lock :whostate "Level 3")
           nil)))
     error
     "With-lock-held did not fail when used recursively")
    (forced-format "~&;;   Testing with-lock-held returned values...~%")
    (let ((returned-values
           (multiple-value-list (with-lock-held (nonrecursive-lock) 
                                  (values 1 2)))))
      (unless (equal returned-values '(1 2))
        (error "Incorrect ~s returned values: ~s"
               'with-lock-held
               returned-values)))
    (forced-format
     "~&;; Basic lock tests completed (~,2f seconds real time).~%"
     (/ (float (- (get-internal-real-time) start-real-time))
        (float internal-time-units-per-second)))))
  
;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun basic-thread-tests ()  
  (forced-format "~&;; Performing basic thread tests...")
  (let ((start-real-time (get-internal-real-time)))
    (unless (threadp (current-thread))
      (log-error "(current-thread) is not a thread"))
    (unless (member (current-thread) (all-threads))
      (log-error "(current-thread) is not a member of (all-threads)"))
    (let ((thread (spawn-thread "Trivial thread" #'sleep-nearly-forever)))
      (unless (threadp thread)
        (log-error "Spawned thread is not a thread"))
      (unless (member thread (all-threads))
        (log-error "Spawned thread is not a member of (all-threads)"))
      (kill-thread thread)
      ;; Allow sufficient time for the kill to be completed:
      (sleepy-time)
      (when (member thread (all-threads))
        (log-error "Killed thread is still a member of (all-threads)")))
    ;; Check that sleep is not "busy waiting...":  
    (sleep 0)                           ; one untimed call to set things up...
    (forced-format 
     "~&;;   Timing (sleep 0), run time should be zero seconds...")
    (let ((run-time (time-it (sleep 0))))
      (when (plusp run-time)
        (warn "(sleep 0) consumed ~s seconds of processing time." run-time)))
    (forced-format
     "~&;;   Timing (sleep 10), run time should also be zero seconds...")
    (let ((run-time (time-it (sleep 10))))
      (when (plusp run-time)
        (warn "(sleep 10) consumed ~s seconds of processing time." run-time)))
    ;; Check to be sure that (sleep 0) is not optimized away by this CL:
    (let ((iterations 
           #+digitool-mcl
           100                          ; MCL has a VERY high overhead
           #-digitool-mcl
           (min 100000 most-positive-fixnum)))
      (forced-format "~&;;   Timing ~s (sleep 0)s..." iterations)
      (let ((run-time (time-it (dotimes (i iterations)
                                 (declare (fixnum i))
                                 (sleep 0)))))
        (when (zerop run-time)
          (warn "~s (sleep 0)s took ~s seconds" iterations run-time)))
      (forced-format "~&;;   Timing ~s throwable (sleep 0)s..." iterations)
      (let ((run-time (time-it (dotimes (i iterations)
                                 (declare (fixnum i))
                                 (catch 'throwable-sleep-nearly-forever
                                   (sleep 0))))))
        (when (zerop run-time)
          (warn "~s throwable (sleep 0)s took ~s seconds" 
                iterations run-time))))
    (forced-format 
     "~&;; Basic thread tests completed  (~,2f seconds real time).~%"
     (/ (float (- (get-internal-real-time) start-real-time))
        (float internal-time-units-per-second)))))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun thread-timing-tests ()  
  (forced-format "~&;; Performing thread timing tests...")
  ;; Do spawn-thread timing:
  (let ((iterations 
         #+allegro 1500                 ; Allegro is limited to < 2K or so
         ;; Spawning in CCL is slow (10K works, but we don't want to wait)
         #+clozure 1000
         ;; Spawning in MCL is slow (10K works, but we don't want to wait)
         #+digitool-mcl 500
         #+ecl 3000                     ; ECL is limited to < 3K or so
         #+lispworks 250                ; Lispworks is limited to < 300 or so
         #-(or allegro 
               clozure
               digitool-mcl
               ecl
               lispworks)
         10000)
        (thread-count (length (all-threads))))
    (declare (fixnum iterations))
    (forced-format "~&;;   Timing ~s spawn-threads..." iterations)
    (time-it (dotimes (i iterations)
               (declare (fixnum i))
               (spawn-thread "Do nothing" #'(lambda ()))))
    (sleepy-time)
    (unless (= thread-count (length (all-threads)))
      (log-error "A do-nothing thread is still a member of (all-threads)")))
  ;; Do spawn and die timing:
  (let ((iterations 
         ;; Spawning in CCL is slow (10K works, but we don't want to wait)
         #+clozure
         1000
         ;; Spawning in MCL is slow (10K works, but we don't want to wait)
         #+digitool-mcl 
         500
         #-(or clozure
               digitool-mcl)
         10000)
        (thread-count (length (all-threads)))
        (cv (make-condition-variable))
        (start-real-time (get-internal-real-time)))
    (declare (fixnum iterations))
    (forced-format "~&;;   Timing ~s spawn and die threads..." iterations)
    (labels ((spawn-and-die-fn (count)
               (thread-yield)
               (cond ((zerop count)
                      (with-lock-held (cv)
                        (condition-variable-signal cv)))
                     (t 
                      ;; Lispworks is slow to clean up dead threads, and so it
                      ;; can fail to allocate stack space for new threads that
                      ;; are spawned so quickly.  So, we give Lispworks some
                      ;; extra time every 200 threads to catch up...
                      #+lispworks
                        (when (zerop (mod count 200))
                          (sleep 0.005))
                        (spawn-thread "Spawn and die"
                                      #'spawn-and-die-fn
                                      (decf count))))))
      (time-it (progn
                 (spawn-thread "Spawn and die"
                               #'spawn-and-die-fn
                               iterations)
                 (with-lock-held (cv)
                   (condition-variable-wait cv)))))
    (sleepy-time)
    (unless (= thread-count (length (all-threads)))
      (log-error "A spawn-and-die thread is still a member of (all-threads)"))
    (forced-format
     "~&;; Thread timing tests completed (~,2f seconds real time).~%"
     (/ (float (- (get-internal-real-time) start-real-time))
        (float internal-time-units-per-second)))))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun with-timeout-tests ()  
  (forced-format "~&;; Performing with-timeout tests...")
  (let ((values (multiple-value-list 
                 (with-timeout (1 (values 3 4))
                   (values 1 2)))))
    (unless (equal '(1 2) values)
      (log-error "With-timeout did not return the correct timed-body values ~
                  (1 2): ~s"
                 values)))
  (let ((values (multiple-value-list 
                 (with-timeout (0.1 (values 3 4))
                   (sleep 1)
                   (values 1 2)))))
    (unless (equal '(3 4) values)
      (log-error "With-timeout did not return the correct timeout-body values ~
                  (3 4): ~s"
                 values)))
  (forced-format "~&;; With-timeout tests completed~%"))

;;; ---------------------------------------------------------------------------

#+threads-not-available
(defun basic-nonthreaded-thread-tests ()  
  (forced-format "~&;; Performing basic nonthreaded thread tests...")
  (let ((current-thread (current-thread)))
    (unless (eq current-thread ':threads-not-available)
      (log-error "(current-thread) is ~s, not ~s" 
                 current-thread
                 ':threads-not-available)))
  (when (all-threads)
    (log-error "(all-threads) is not nil"))
  (forced-format "~&;;   Checking spawn-thread...")
  (check-error-checking 
     (spawn-thread "Trivial thread" #'sleep-nearly-forever)
   warning
   "(spawn-thread) did not generate a warning")
  (forced-format "~&;;   Checking kill-thread...")
  (check-error-checking 
     (kill-thread nil)
   warning
   "(kill-thread) did not generate a warning")
  (forced-format "~&;; Basic nonthreaded thread tests completed~%"))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun nonrecursive-lock-contention-tests ()
  (forced-format "~&;; Performing nonrecursive lock contention tests...")
  (let ((nonrecursive-lock (make-lock :name "Nonrecursive lock"))
        (counter 0))
    (spawn-thread
     "Lock contender"
     #'(lambda (lock)
         (with-lock-held (lock :whostate "Held by contender")
           (unless (oddp (incf counter))
             (log-error "Unexpected ~s value: ~s" 
                        'counter
                        counter))
           (sleepy-time))
         (sleepy-time)
         (with-lock-held (lock :whostate "Held again by contender")
           (unless (oddp (incf counter))
             (log-error "Unexpected ~s value: ~s" 
                        'counter
                        counter))
           (sleepy-time)))
     nonrecursive-lock)
    ;; Let the lock contender thread get started:
    (sleepy-time)
    (with-lock-held (nonrecursive-lock
                     :whostate "Held by main thread")
      (unless (evenp (incf counter))
        (log-error "Unexpected ~s value: ~s" 
                   'counter
                   counter))
      (sleepy-time))
    (sleepy-time)
    (with-lock-held (nonrecursive-lock
                     :whostate "Held by main thread")
      (unless (evenp (incf counter))
        (log-error "Unexpected ~s value: ~s" 
                   'counter
                   counter))))
  (forced-format "~&;; Nonrecursive lock contention tests completed~%"))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun recursive-lock-contention-tests ()
  (forced-format "~&;; Performing recursive lock contention tests...")
  (let ((recursive-lock (make-recursive-lock :name "Recursive lock"))
        (counter 0))
    (spawn-thread
     "Lock contender"
     #'(lambda (lock)
         (with-lock-held (lock :whostate "Held by contender")
           (unless (oddp (incf counter))
             (log-error "Unexpected ~s value: ~s" 
                        'counter
                        counter))
           (sleepy-time))
         (sleepy-time)
         (with-lock-held (lock :whostate "Held again by contender")
           (unless (oddp (incf counter))
             (log-error "Unexpected ~s value: ~s" 
                        'counter
                        counter))
           (sleepy-time)))
     recursive-lock)
    ;; Let the lock contender thread get started:
    (sleepy-time)
    (with-lock-held (recursive-lock :whostate "Held by main thread")
      (unless (evenp (incf counter))
        (log-error "Unexpected ~s value: ~s" 
                   'counter
                   counter))
      (sleepy-time))
    (sleepy-time)
    (with-lock-held (recursive-lock :whostate "Held by main thread")
      (unless (evenp (incf counter))
        (log-error "Unexpected ~s value: ~s" 
                   'counter
                   counter))))
  (forced-format "~&;; Recursive lock contention tests completed~%"))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun condition-variables-tests ()
  ;;;
  ;;; Condition-variable wait test with signal:
  (forced-format "~&;; Performing condition-variable wait & signal tests...")
  (let ((cv (make-condition-variable :class 'state-cv)))
    (spawn-thread
     "Condition Variable Waiter"
     #'(lambda (cv)
         (forced-format "~&;;    Also waiting on CV...~%")
         (with-lock-held (cv)
           (loop until (eq (state-of cv) ':signaled)
               do (condition-variable-wait cv))
           (setf (state-of cv) nil))
         (forced-format "~&;;    Also continuing on CV...~%"))
     cv)
    (spawn-thread 
     "Condition Variable Signaler"
     #'(lambda (cv)
         (sleepy-time)
         (forced-format "~&;;    Signaling CV...~%")
         (with-lock-held (cv)
           (setf (state-of cv) ':signaled)
           (condition-variable-signal cv))
         (sleepy-time)
         (forced-format "~&;;    Re-signaling CV...~%")
         (with-lock-held (cv)
           (setf (state-of cv) ':signaled)
           (condition-variable-signal cv)))
     cv)
    (forced-format "~&;;    Waiting on CV...~%")
    (with-lock-held (cv)
      (loop until (eq (state-of cv) ':signaled)
          do (condition-variable-wait cv))
      (setf (state-of cv) nil))
    (forced-format "~&;;    Continuing on CV...~%"))
  (forced-format "~&;; Condition-variable wait & signal tests completed~%")
  ;;;
  ;;; Condition-variable wait test with broadcast:
  (forced-format 
   "~&;; Performing condition-variable wait & broadcast tests...")
  (let ((cv (make-condition-variable :class 'state-cv)))
    (spawn-thread
     "Broadcast Condition Variable Waiter"
     #'(lambda (cv)
         (forced-format "~&;;    Also waiting on broadcast CV...~%")
         (with-lock-held (cv)
           (loop until (eq (state-of cv) ':broadcasted)
               do (condition-variable-wait cv)))
         (forced-format "~&;;    Continuing on broadcast CV...~%"))
     cv)
    (spawn-thread 
     "Broadcast Condition Variable Signaler"
     #'(lambda (cv)
         (sleepy-time)
         (forced-format "~&;;    Signaling CV to all...~%")
         (with-lock-held (cv)
           (setf (state-of cv) ':broadcasted)
           (condition-variable-broadcast cv)))
     cv)
    (forced-format "~&;;    Waiting on broadcast CV...~%")
    (with-lock-held (cv)
      (loop until (eq (state-of cv) ':broadcasted)
          do (condition-variable-wait cv)))
    (forced-format "~&;;    Also continuing on broadcast CV...~%"))
  (forced-format 
   "~&;; Condition-variable wait & broadcast tests completed~%")
  ;;;
  ;;; Non-timeout test of condition-variable-wait-with-timeout:
  (forced-format
   "~&;; Performing condition-variable-wait-with-timeout (non-timeout) tests...")
  (let ((cv (make-condition-variable)))
    (spawn-thread
     "Timeout-Waiting Condition Variable Waiter"
     #'(lambda (cv)
         (forced-format "~&;;    Also waiting-with-timeout on CV...~%")
         (with-lock-held (cv)
           (unless (condition-variable-wait-with-timeout
                    cv *nearly-forever-seconds*)
             (log-error "~s returned nil on non-timeout"
                        'condition-variable-wait-with-timeout)))
         (forced-format
          "~&;;    Also continuing on waiting-with-timeout CV...~%"))
     cv)
    (spawn-thread 
     "Timeout-Waiting condition Variable Signaler"
     #'(lambda (cv)
         (sleepy-time)
         (forced-format "~&;;    Signaling waiting-with-timeout CV...~%")
         (with-lock-held (cv)
           (condition-variable-signal cv))
         (sleepy-time)
         (forced-format "~&;;    Re-signaling waiting-with-timeout CV...~%")
         (with-lock-held (cv)
           (condition-variable-signal cv)))
     cv)
    (forced-format "~&;;    Waiting-with-timeout on CV...~%")
    (with-lock-held (cv)
      (condition-variable-wait-with-timeout cv *nearly-forever-seconds*))
    (forced-format "~&;;    Continuing on waiting-with-timout CV...~%"))
  (forced-format 
   "~&;; Condition-variable wait-with-timeout (non-timeout) tests completed~%")
  ;;;
  ;;; Timeout test of condition-variable-wait-with-timeout:
  (forced-format 
   "~&;; Performing condition-variable wait-with-timeout (timeout) tests...")
  (let ((cv (make-condition-variable)))
    (forced-format "~&;;    Waiting-with-timeout (forever) on CV...~%")
    (with-lock-held (cv)
      (when (condition-variable-wait-with-timeout cv 1)
        (log-error "~s did not return nil on timeout"
                   'condition-variable-wait-with-timeout)))      
    (forced-format "~&;;    Continuing without CV...~%"))
  (forced-format
   "~&;; Condition-variable wait-with-timeout (timeout) tests completed~%"))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun condition-variable-timing-tests ()
  (forced-format "~&;; Performing condition-variable timing tests...")
  (flet ((test (signal-fn signal-fn-label)
           (let ((cv (make-condition-variable
                      :class 'state-cv
                      :state 0))
                 (iterations 5000)
                 (start-real-time (get-internal-real-time)))
             (forced-format
              "~&;;   Timing ~s condition-variable wait & ~as..."
              (* 2 iterations)
              signal-fn-label)
             (spawn-thread 
              "Condition Variable Incrementer"
              #'(lambda (cv iterations signal-fn)
                  (dotimes (i iterations)
                    (with-lock-held (cv)
                      (loop while (plusp (state-of cv)) 
                          do (unless
                                 (condition-variable-wait-with-timeout cv 2)
                               (warn "Incrementer wait timeout (iteration ~s)"
                                     i)))
                      (incf (state-of cv))
                      (funcall signal-fn cv))))
              cv 
              iterations
              signal-fn)
             (time-it
              (dotimes (i iterations)
                (with-lock-held (cv)
                  (loop until (plusp (state-of cv)) 
                      do (unless (condition-variable-wait-with-timeout cv 2)
                           (warn "Decrementer wait timeout (iteration ~s)"
                                 i)))
                  (decf (state-of cv))
                  (funcall signal-fn cv))))
             (forced-format
              "~&;;   Condition-variable wait & ~a timing test completed~
               ~%;;      (~,2f seconds real time).~%"
              signal-fn-label
              (/ (float (- (get-internal-real-time) start-real-time))
                 (float internal-time-units-per-second))))))
    ;; Wait & signal timing:
    (test #'condition-variable-signal "signal")
    ;; Wait & broadcast timing:
    (test #'condition-variable-broadcast "broadcast"))
  (forced-format "~&;; Condition-variable timing tests completed~%"))

;;; ---------------------------------------------------------------------------

(defun run-in-thread-tests ()
  (forced-format "~&;; Performing run-in-thread tests...")
  (setf *x* 0)
  (let* ((cv (make-condition-variable))
         (thread (spawn-thread
                  "Service Thread"
                  #'(lambda (cv)
                      (forced-format "~&;;   Sleeping...~%")
                      (catch 'awaken (sleep-nearly-forever))
                      (forced-format "~&;;   Awakened...~%")
                      (with-lock-held (cv)
                        (incf *x*)
                        (condition-variable-signal cv)))  
                  cv)))
    (sleepy-time)
    (run-in-thread thread #'(lambda () 
                              (incf *x*)
                              (throw 'awaken nil)))
    (with-lock-held (cv)
      (condition-variable-wait cv)
      (forced-format "~&;;   *X* = ~s~%" *x*)))
  (forced-format "~&;; Run-in-Thread tests completed~%"))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun hibernate/awaken-thread-tests ()
  (forced-format "~&;; Performing hibernate/awaken thread tests...")
  (let* ((cv (make-condition-variable :class 'state-cv
                                      :state nil))
         (thread 
          (spawn-thread
           "Hibernating Thread"
           #'(lambda (cv)
               (let ((*special* 2007))
                 (declare (special *special*))
                 (with-lock-held (cv)
                   (setf (state-of cv) ':ready-to-hibernate)
                   (condition-variable-signal cv))
                 (forced-format "~&;;     Hibernating...~%")
                 (hibernate-thread)
                 (forced-format "~&;;     Awakened~%")
                 (with-lock-held (cv)
                   (setf (state-of cv) ':awake)
                   (condition-variable-signal cv))
                 (forced-format "~&;;     Waiting on proceed message...~%")
                 (with-lock-held (cv)
                   (unless (eq (state-of cv) ':proceed)
                     (condition-variable-wait cv)
                     (unless (eq (state-of cv) ':proceed)
                       (log-error "Unexpected CV state: ~s (~s expected)."
                                  (state-of cv)
                                  ':proceed))))
                 (forced-format
                  "~&;;     Proceed signal received, signaling rehibernate.~%")
                 (with-lock-held (cv)
                   (setf (state-of cv) ':ready-to-rehibernate)
                   (condition-variable-signal cv))
                 (forced-format "~&;;     Rehibernating...~%")
                 (hibernate-thread)
                 (forced-format "~&;;     Re-awakened~%")
                 (with-lock-held (cv)
                   (setf (state-of cv) ':reawake)
                   (condition-variable-signal cv))))
           cv)))
    (with-lock-held (cv)
      (unless (eq (state-of cv) ':ready-to-hibernate)
        (condition-variable-wait cv)
        (unless (eq (state-of cv) ':ready-to-hibernate)
          (log-error "Unexpected CV state: ~s (~s expected)."
                     (state-of cv)
                     ':ready-to-hibernate))))
    (forced-format "~&;;   Hibernate ready signal received.~%")
    ;; Allow process to hibernate:
    (sleepy-time)
    (forced-format "~&;;   Awakening thread...~%")
    (awaken-thread thread)
    (with-lock-held (cv)
      (unless (eq (state-of cv) ':awake)
        (condition-variable-wait cv)
        (unless (eq (state-of cv) ':awake)
          (log-error "Unexpected CV state: ~s (~s expected)."
                     (state-of cv)
                     ':awake))))
    (forced-format "~&;;   Awake signal received, signaling proceed.~%") 
    (with-lock-held (cv)
      (setf (state-of cv) ':proceed)
      (condition-variable-signal cv))
    (forced-format "~&;;   Waiting for rehibernate signal...~%") 
    (with-lock-held (cv)
      (unless (eq (state-of cv) ':ready-to-rehibernate)
        (condition-variable-wait cv)
        (unless (eq (state-of cv) ':ready-to-rehibernate)
          (log-error "Unexpected CV state: ~s (~s expected)."
                     (state-of cv)
                     ':ready-to-rehibernate))))
    (forced-format "~&;;   Rehibernate ready signal received.~%")
    ;; Allow process to hibernate:
    (sleepy-time)
    (forced-format
     "~&;;   Trying symbol-value-in-thread on hibernating thread...~%")
    (unless (= (symbol-value-in-thread '*special* thread) 2007)
      (log-error "Symbol-value-in-thread failed on hibernating thread"))
    (forced-format "~&;;   Re-awakening thread...~%")
    (awaken-thread thread)
    (with-lock-held (cv)
      (unless (eq (state-of cv) ':reawake)
        (condition-variable-wait cv)
        (unless (eq (state-of cv) ':reawake)
          (log-error "Unexpected CV state: ~s (~s expected)."
                     (state-of cv)
                     ':reawake)))))
  (forced-format "~&;;   Trying with-timeout on a hibernating thread...~%")
  (let ((result (with-timeout (0.1 ':timed-out)
                  (hibernate-thread))))
    (unless (eq result ':timed-out)
      (log-error "Unexpected with-timeout value from a hibernating thread.")))
  (forced-format "~&;; Hibernate/awaken thread tests completed~%"))

;;; ---------------------------------------------------------------------------

#-threads-not-available
(defun symbol-value-in-thread-tests ()
  (forced-format "~&;; Performing symbol-value-in-thread tests...")
  (let* ((cv (make-condition-variable :class 'state-cv))
         (thread
          (spawn-thread
           "Binding thread"
           #'(lambda (cv)
               (let ((*v* 4)
                     (*x* 5)
                     (*y* 6))
                 (declare (special *v* *x* *y))
                 (makunbound '*y*)
                 (with-lock-held (cv)
                   (setf (state-of cv) ':hibernating)
                   (condition-variable-signal cv))
                 (forced-format "~&;;   Binding thread hibernating...")
                 (hibernate-thread)
                 (forced-format "~&;;   Binding thread awakened.")
                 (with-lock-held (cv)
                   (setf (state-of cv) ':awake)
                   (condition-variable-signal cv))))
           cv)))
    (with-lock-held (cv)
      (unless (eq (state-of cv) ':hibernating)
        (condition-variable-wait cv)))
    (sleepy-time)
    (flet ((test (symbol result)
             (let ((test-result
                    (multiple-value-list 
                     (symbol-value-in-thread symbol thread))))
               (unless (equal test-result result)
                 (log-error "~s call with ~s failed:~
                             ~%~9t~s expected; ~s returned."
                            'symbol-value-in-thread 
                            symbol result test-result)))))
      (test '*v* '(4 t))
      (test '*w* '(nil t))
      (test '*x* '(5 t))
      (test '*y* '(nil nil))
      (test '*z* '(3 t))
      (test 'pi  (list pi 't))
      (test '*garbage* '(nil nil)))
    (awaken-thread thread)
    (with-lock-held (cv)
      (unless (eq (state-of cv) ':awake)
        (condition-variable-wait cv)))
    (forced-format "~&;; Symbol-value-in-thread tests completed~%")))

;;; ---------------------------------------------------------------------------

(defun atomic-incf/decf-tests ()
  ;; Test atomic-incf/decf basic operation (atomic-operation exclusion not
  ;; tested):
  (forced-format "~&;; Testing atomic-incf/decf...~%")
  (let* ((x 0))
    (atomic-incf x 2)
    (atomic-decf x)
    (unless (= x 1)
      (log-error "Incorrect atomic-incf/decf result: ~s"
                 x)))
  (forced-format "~&;; Atomic-incf/decf test completed.~%"))

;;; ---------------------------------------------------------------------------

(defun portable-threads-tests ()
  (let ((start-real-time (get-internal-real-time)))
    (forced-format "~&;; Starting portable threads tests...~%")
    (basic-lock-tests)
    (atomic-incf/decf-tests) 
    #+threads-not-available
    (basic-nonthreaded-thread-tests)
    #-threads-not-available
    (let ((all-threads (all-threads)))
      (with-timeout-tests)
      (basic-thread-tests)
      (nonrecursive-lock-contention-tests)
      (recursive-lock-contention-tests)
      (condition-variables-tests)
      (hibernate/awaken-thread-tests)
      (symbol-value-in-thread-tests)
      (forced-format "~&;; Checking for unreclaimed threads...~%")
      (sleepy-time)                     ; allow some time for cleanups
      (let ((new-all-threads (all-threads)))
        (unless (= (length all-threads)
                   (length new-all-threads))
          (let ((unexpected-remaining-threads                 
                 (set-difference new-all-threads all-threads))
                (unexpected-missing-threads                 
                 (set-difference all-threads new-all-threads)))
            (when unexpected-remaining-threads
              (log-error "Unexpected remaining missing: ~s"
                         unexpected-remaining-threads))
            (when unexpected-missing-threads
              (log-error "Unexpected threads missing: ~s"
                         unexpected-missing-threads)))))
      (thread-timing-tests)
      (condition-variable-timing-tests))
    (forced-format
     "~&;; Portable threads tests completed (~,2f seconds real time).~%"
     (/ (float (- (get-internal-real-time) start-real-time))
        (float internal-time-units-per-second)))))

;;; ---------------------------------------------------------------------------

(when *autorun-modules*
  (portable-threads-tests))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================


