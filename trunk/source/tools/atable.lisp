;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/atable.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Mar 12 05:09:22 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Autotransitioning "Fast Hash" Tables
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill
;;;
;;; Copyright (C) 2008-2010, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-15-08 File created.  (Corkill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(atable                      ; data type
            atable-count
            atable-test
            assoc-eq-list
            assoc-eql-list
            assoc-equal-list
            assoc-equalp-list
            clear-atable
            compute-atable-size-transitions
            delete-entry
            get-entry
            keys-only-atable            ; data type
            keys-only-eq-list
            keys-only-eql-list
            keys-only-equal-list
            keys-only-equalp-list
            make-atable
            map-atable)))

;;; ---------------------------------------------------------------------------
;;;  Test <--> index lookups
;;;    - keys-only tables are even indicies, key/value tables are odd indicies
;;;    - hash-tables are 0 & 1, lists are 2-9

(eval-when (#+cmu :compile-toplevel     ; CMUCL requires compile-time
                                        ; definition to support
                                        ; LOAD-TIME-VALUE usage (below)
            :load-toplevel :execute) 
  (defparameter *atable-test-vector*
      (vector nil nil 'eq 'eq 'eql 'eql 'equal 'equal 'equalp 'equalp)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (with-full-optimization ()
    (defun determine-atable-index (test keys-only)
      (if keys-only
          (ecase test
            (eq 2)
            (eql 4)
            (equal 6)
            (equalp 8))
          (ecase test
            (eq 3)
            (eql 5)
            (equal 7)
            (equalp 9))))))

(defun determine-atable-test (index)
  (declare (fixnum index))
  (svref (load-time-value *atable-test-vector*) index))
(defcm determine-atable-test (index)
  `(svref (load-time-value *atable-test-vector*) (the fixnum ,index)))

;;; ---------------------------------------------------------------------------
;;;  Transition values

(defvar *atable-transition-sizes*
    (make-array '(10) :initial-element 10))

;;; ---------------------------------------------------------------------------
;;;  Atable-data "structure"

(defconstruct %atable-data (list count))

;;; ---------------------------------------------------------------------------
;;;  Atable "structure"

(defconstruct (atable :constructor %make-atable)
    (type-index data))

;;; ---------------------------------------------------------------------------

(defun atable-count (atable)
  (let* ((index (atable-type-index atable))
         (data (atable-data atable))
         (list? (>& index 1)))
    (if list?
        (%atable-data-count data)
        (hash-table-count data))))

;;; ---------------------------------------------------------------------------

(defun atable-test (atable)
  (let* ((index (atable-type-index atable))
         (data (atable-data atable))
         (list? (>& index 1)))
    (if list?
        (determine-atable-test index)
        (hash-table-test data))))

;;; ---------------------------------------------------------------------------
;;;  Atable constructor

(defun make-atable (&key (test 'eql) (size 0)
                         keys-only
                         rehash-size rehash-threshold)
  (with-full-optimization ()
    (unless (symbolp test)
      (let ((test-name (nth-value 2 (function-lambda-expression test))))
        (when (symbolp test-name)
          (setf test test-name))))
    (let* ((index (determine-atable-index test keys-only))
           (transition-size (svref *atable-transition-sizes* index))
           (table-data
            (cond ((>& size transition-size)
                   (setf index (if keys-only 0 1))
                   (apply (if keys-only
                              #'make-keys-only-hash-table-if-supported
                              #'make-hash-table)
                          :test test 
                          :size size 
                          `(,@(when rehash-size
                                `(:rehash-size ,rehash-size))
                              ,@(when rehash-size 
                                  `(:rehash-threshold ,rehash-threshold)))))
                  (t (make-%atable-data nil 0)))))
      (%make-atable index table-data))))

;;; ---------------------------------------------------------------------------
;;;  Atable reader

(macrolet ((make-reader ()
             (flet ((make-list-fn (test)
                      `(let* ((list (%atable-data-list data))
                              (sublist (member key list :test ',test)))
                         (if sublist
                             (values (car (the cons sublist)) 't)
                             (values nil nil))))
                    (make-assoc-fn (test)
                      `(let* ((alist (%atable-data-list data))
                              (acons (assoc key alist :test ',test)))
                         (if acons
                             (values (cdr (the cons acons)) 't)
                             (values nil nil)))))
               `(defun get-entry (key atable)
                  (with-full-optimization ()
                    (let ((index (atable-type-index atable))
                          (data (atable-data atable)))
                      (declare (fixnum index))
                      (case index
                       ((0 1) (gethash key data))
                       (2 ,(make-list-fn 'eq))
                       (3 ,(make-assoc-fn 'eq))
                       (4 ,(make-list-fn 'eql))
                       (5 ,(make-assoc-fn 'eql))
                       (6 ,(make-list-fn 'equal))
                       (7 ,(make-assoc-fn 'equal))
                       (8 ,(make-list-fn 'equalp))
                       (9 ,(make-assoc-fn 'equalp)))))))))
  (make-reader))

;;; ---------------------------------------------------------------------------
;;;  Atable writer

(macrolet ((make-writer ()
             (flet ((make-list-fn (test)
                      `(let* ((list (%atable-data-list data))
                              (sublist (member key list :test ',test)))
                         (unless sublist
                           (let ((threshold 
                                  (1+& (svref 
                                        (load-time-value *atable-transition-sizes*)
                                        index))))
                             (cond 
                              ((<& (incf& (%atable-data-count data))
                                   threshold)
                               (push key (%atable-data-list data)))
                              ;; Transition to hash-table:
                              (t (let ((ht (make-keys-only-hash-table-if-supported
                                            :test (determine-atable-test index)
                                            :size threshold)))
                                   (dolist (key (%atable-data-list data))
                                     (setf (gethash key ht) key))
                                   ;; Add new entry
                                   (setf (gethash key ht) key)
                                   (setf (atable-type-index atable) 0)
                                   (setf (atable-data atable) ht))))))
                         key))
                    (make-assoc-fn (test)
                      `(let* ((alist (%atable-data-list data))
                              (acons (assoc key alist :test ',test)))
                         (cond
                          ;; Replace value:
                          (acons (setf (cdr acons) nv))
                          (t (let ((threshold 
                                    (1+& (svref 
                                          (load-time-value *atable-transition-sizes*)
                                          index))))
                               (cond
                                ;; Add assoc-based entry:
                                ((<& (incf& (%atable-data-count data))
                                     threshold)
                                 (setf (%atable-data-list data)
                                       (acons key nv alist)))
                                ;; Transition to hash-table:
                                (t (let ((ht (make-hash-table
                                              :test (determine-atable-test index))))
                                     (dolist (acons (%atable-data-list data))
                                       (setf (gethash (car acons) ht) (cdr acons)))
                                     ;; Add new entry
                                     (setf (gethash key ht) nv)
                                     (setf (atable-type-index atable) 1)
                                     (setf (atable-data atable) ht)))))))
                         nv)))
               `(defun (setf get-entry) (nv key atable)
                  (with-full-optimization ()
                    (let ((index (atable-type-index atable))
                          (data (atable-data atable)))
                      (declare (fixnum index))
                      (case index
                        ((0 1) (setf (gethash key data) nv))
                        (2 ,(make-list-fn 'eq))
                        (3 ,(make-assoc-fn 'eq))
                        (4 ,(make-list-fn 'eql))
                        (5 ,(make-assoc-fn 'eql))
                        (6 ,(make-list-fn 'equal))
                        (7 ,(make-assoc-fn 'equal))
                        (8 ,(make-list-fn 'equalp))
                        (9 ,(make-assoc-fn 'equalp)))))))))
  (make-writer))

;;; ---------------------------------------------------------------------------
;;;  Atable delete-entry

(macrolet ((make-deleter ()
             (flet ((make-list-fn (test)
                      `(let* ((list (%atable-data-list data))
                              (delete-performed? nil))
                         (declare (type list list))
                         (flet ((test-fn (item)
                                  ;; Tell CMUCL & SBCL we can't use float-eql
                                  #+(or cmu sbcl)
                                  ,@(when (eq test 'eql)
                                      `((declare (notinline ,test))))
                                  (when (,test key item)
                                    (setf delete-performed? 't))))
                           (declare (dynamic-extent #'test-fn))
                           (setf (%atable-data-list data)
                                 (delete-if #'test-fn list)))
                         (when delete-performed?
                           (decf& (%atable-data-count data))
                           't)))
                    (make-assoc-fn (test)
                      `(let* ((alist (%atable-data-list data))
                              (delete-performed? nil))
                         (declare (type list alist))
                         (flet ((test-fn (item)
                                  ;; Tell CMUCL & SBCL we can't use float-eql
                                  #+(or cmu sbcl)
                                  ,@(when (eq test 'eql)
                                      `((declare (notinline ,test))))
                                  (when (,test key (car (the cons item)))
                                    (setf delete-performed? 't))))
                           (declare (dynamic-extent #'test-fn))
                           (setf (%atable-data-list data)
                                 (delete-if #'test-fn alist)))
                         (when delete-performed?
                           (decf& (%atable-data-count data))
                           't))))
               `(defun delete-entry (key atable)
                  (with-full-optimization ()
                    (let ((index (atable-type-index atable))
                          (data (atable-data atable)))
                      (declare (fixnum index))
                      (case index
                        (0              ; keys-only hash table
                         (let ((result (remhash key data)))
                           (when result 
                             (let ((threshold 
                                    (-& (svref
                                         (load-time-value *atable-transition-sizes*)
                                         index)
                                        2))
                                   (count (hash-table-count data)))
                               (when (<& count threshold)
                                 (let ((test (hash-table-test data)))
                                   (setf (atable-type-index atable)
                                         (determine-atable-index test 't))
                                   (setf (atable-data atable)
                                         (make-%atable-data
                                          (loop for key being each hash-key in data
                                              collect key)
                                          count)))))
                             result)))
                        (1              ; key/value hash-table
                         (let ((result (remhash key data)))
                           (when result 
                             (let ((threshold 
                                    (-& (svref
                                         (load-time-value *atable-transition-sizes*)
                                         index)
                                        2))
                                   (count (hash-table-count data)))
                               (when (<& count threshold)
                                 (let ((test (hash-table-test data)))
                                   (setf (atable-type-index atable)
                                         (determine-atable-index test nil))
                                   (setf (atable-data atable)
                                         (make-%atable-data 
                                          (loop for key being each hash-key in data 
                                              using (hash-value value) 
                                              collect (cons key value))
                                          count)))))
                             result)))
                        (2 ,(make-list-fn 'eq))
                        (3 ,(make-assoc-fn 'eq))
                        (4 ,(make-list-fn 'eql))
                        (5 ,(make-assoc-fn 'eql))
                        (6 ,(make-list-fn 'equal))
                        (7 ,(make-assoc-fn 'equal))
                        (8 ,(make-list-fn 'equalp))
                        (9 ,(make-assoc-fn 'equalp)))))))))
  (make-deleter))

;;; ---------------------------------------------------------------------------
;;;  Atable clear

(defun clear-atable (atable &key retain-as-hash-table)
  (with-full-optimization ()
    (let ((index (atable-type-index atable))
          (data (atable-data atable)))
      (cond 
       ;; Hash-table implementation:
       ((<=& index 1)
        (if retain-as-hash-table
            (clrhash data)
            (let ((test (hash-table-test data)))
              (setf (atable-type-index atable)
                    (determine-atable-index test (zerop& index)))
              (setf (atable-data atable)
                    (make-%atable-data nil 0))
              ;; return nil:
              nil)))
       ;; List-based implementation:
       (t (setf (%atable-data-count data) 0)
          (setf (%atable-data-list data) nil))))))
  
;;; ---------------------------------------------------------------------------
;;;  Atable mapper

(defun map-atable (function atable)
  #+cmu (declare (notinline coerce))
  (let ((fn (coerce function 'function)))
    (declare (function fn))
    (with-full-optimization ()
      (let ((index (atable-type-index atable))
            (data (atable-data atable)))
        (cond 
         ;; Hash-table implementation:
         ((<=& index 1)
          (maphash fn data))
         ;; Member (keys-only) implementations:
         ((evenp& index)
          (dolist (key (%atable-data-list data))
            (funcall fn key key)))
         ;; Assoc-based implementations:
         (t (dolist (acons (%atable-data-list data))
              (declare (type cons acons))
              (funcall fn (car acons) (cdr acons)))))))))
  
;;; ---------------------------------------------------------------------------
;;;  Transition-value determination/setting

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; used to avoid iteration over-optimization by CMUCL & SBCL:
  (defvar *timer-result*)
  
  (defmacro timer (n &body body)
    `(let ((%start-time% (get-internal-run-time)))
       (dotimes (i (the fixnum ,n))
         (declare (fixnum i)) 
         (setf *timer-result* 
               (with-full-optimization ()
                 ,@body)))
       (/ (- (get-internal-run-time) %start-time%)
          (load-time-value (float internal-time-units-per-second))))))

;;; ---------------------------------------------------------------------------

(defun bsearch-for-transition (max-value timer-iterations
                               sprint-fn marathon-fn keygen-fn)
  (declare (function sprint-fn marathon-fn keygen-fn))
  (let ((maximum-test-iterations 100)
        (min-value 1)
        (test-value (floor& max-value 2)))
    (dotimes (i maximum-test-iterations)
      (declare (fixnum i))
      (when (=& min-value test-value)
        (return-from bsearch-for-transition test-value))
      (let* ((key (funcall keygen-fn test-value))
             (sprint-time 
              (timer timer-iterations (funcall sprint-fn key)))
             (marathon-time
              (timer timer-iterations (funcall marathon-fn key))))
        (if (<$ sprint-time marathon-time)
            (setf min-value test-value)
            (setf max-value test-value))
        (setf test-value (+& min-value (floor& (-& max-value min-value) 2)))))))

;;; ---------------------------------------------------------------------------
;;; Compute the size transition values where hash-tables outperform lists

(defun compute-atable-size-transitions ()
  (flet ((string-it (i)
           (make-string 5 :initial-element (code-char (+& i 32)))))
    (macrolet ((compute-it (test keys-only keygen-fn)
                 (let ((max-size 60)
                       (index (determine-atable-index test keys-only))                       )
                   `(let ((at (make-atable :test ',test
                                           :keys-only ,keys-only))
                          (ht (make-atable :test ',test
                                           :keys-only ,keys-only
                                           :size ,(* 2 max-size))))
                      (loop for i fixnum from ,max-size downto 1 do
                            (let ((key (funcall ,keygen-fn i)))
                              (setf (get-entry key at) i)
                              (setf (get-entry key ht) i)))
                      (with-full-optimization ()
                        (flet ((at-lookup (key)
                                 (get-entry key at))
                               (ht-lookup (key)
                                 (get-entry key ht)))
                          (declare (dynamic-extent #'at-lookup #'ht-lookup))
                          (let ((result
                                 (bsearch-for-transition
                                  ,max-size 250000
                                  #'at-lookup #'ht-lookup ,keygen-fn)))
                            (format t "~&;; ~s~@[ (keys only)~*~] transition: ~s~%"
                                    ',test
                                    ',keys-only
                                    (setf (svref *atable-transition-sizes* ,index)
                                          result)))))))))
      (compute-it eq t #'identity)
      (compute-it eq nil #'identity)
      (compute-it eql t #'identity)
      (compute-it eql nil #'identity)
      (compute-it equal t #'string-it)
      (compute-it equal nil #'string-it)
      (compute-it equalp t #'string-it)
      (compute-it equalp nil #'string-it)
      *atable-transition-sizes*)))

;;; ---------------------------------------------------------------------------

#+not-yet
(eval-when (:load-toplevel)
  (compute-atable-size-transitions))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

