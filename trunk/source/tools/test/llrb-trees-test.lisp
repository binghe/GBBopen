;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/test/llrb-trees-test.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Thu Jul 16 16:26:39 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                               LLRB Tests
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :gbbopen-tools)

(defvar *x* nil)

;;; ---------------------------------------------------------------------------

(defun check-traversal (tree)
  (let ((last-key -infinity&))
    (llrb-map 
     #'(lambda (node)
         (let ((key (rbt-node-key node)))
           (unless (>& key last-key)
             (error "Wrong key ordering"))
           (setf last-key key)))
     tree)))

;;; ---------------------------------------------------------------------------

(defun print-tree (node &optional (indent 0))
  (when node
    (let ((left (rbt-node-right node))
	  (right (rbt-node-left node)))
      (when left
	(print-tree left (1+ indent)))
      (loop for i from 1 to indent
	 do
	   (format t "  "))
      (format t "~a ~:[B~;R~]~%" (rbt-node-key node) (rbt-node-is-red? node))
      (when right
	(print-tree right (1+ indent))))))

;;; ---------------------------------------------------------------------------

(defun llrb-print (node)
  (llrb-prefix-map #'(lambda (node) (format t "~&~s~%" node))
                   node)
  (values))

;;; ---------------------------------------------------------------------------

(defun new (n) 
  (setq *x* (llrb-insert n *x* n))
  (llrb-print *x*))       

;;; ===========================================================================
;;;   Timing

(defun big-test (n)
  (let ((numbers (make-hash-table :size n))
        tree
        ht
        continuep 
        key)
    (dotimes (i n)
      (let ((key (random n)))
        (setf (gethash key numbers) key)))
    (format t "~&;; Numbers: ~a~%" (hash-table-count numbers))
    ;; Build the LLRH tree:
    (printv "Building LLRH tree:")
    (time
     (with-hash-table-iterator (next numbers)
       (multiple-value-setq (continuep key) (next))
       (setf tree (make-llrb-root key key))
       (while (progn
                (multiple-value-setq (continuep key) (next))
                continuep)
         (setf tree (llrb-insert key tree key)))))
    (unless (= (hash-table-count numbers) (llrb-count tree))
      (error "Wrong count"))
    (check-traversal tree)
    ;; Now build the hash table:
    (printv "Building hash table:")
    (time
     (with-hash-table-iterator (next numbers)
       (setf ht (make-hash-table :size (hash-table-count numbers)))
       (while (progn
                (multiple-value-setq (continuep key) (next))
                continuep)
         (setf (gethash key ht) key))))
    ;; LLRH retrievals:
    (printv "LLRB retrievals:")
    (time
     (with-hash-table-iterator (next numbers)
       (while (progn
                (multiple-value-setq (continuep key) (next))
                continuep)
         (llrb-get-node key tree))))
    ;; Hash-table retrievals:
    (printv "Hash-table retrievals:")
    (time
     (with-hash-table-iterator (next numbers)
       (while (progn
                (multiple-value-setq (continuep key) (next))
                continuep)
         (gethash key ht))))))

;;; ===========================================================================
;;;  Trip test

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro logger ((s &body setup) &body body) 
    `(progn (format t "~&;; ~40,,,'-<-~>~%;; ~a~%" ,s)
            ,@setup
            (llrb-print *x*)
            ,@body
            (if *x* 
                (llrb-print *x*) 
                (format t "#<empty>~%")))))

(defun test ()
  (logger ("Delete missing from empty tree:"
           (setf *x* nil))
          (setf *x* (llrb-delete 5 *x*)))
  (print-tree *x*)
  (logger ("Delete root:"
           (setf *x* (make-llrb-root 5 5)))
          (setf *x* (llrb-delete 5 *x*)))
  (print-tree *x*)
  (logger ("Delete missing (left) from singleton tree:"
           (setf *x* (make-llrb-root 5 5)))
          (setf *x* (llrb-delete 1 *x*)))
  (print-tree *x*)
  (logger ("Delete missing (right) from singleton tree:"
           (setf *x* (make-llrb-root 5 5)))
          (setf *x* (llrb-delete 9 *x*)))
  (print-tree *x*)
  (logger ("Delete 1 from 5-1 tree:"
           (setf *x* (make-llrb-root 5 5))
           (setf *x* (llrb-insert 1 *x* 1)))
          (setf *x* (llrb-delete 1 *x*)))
  (print-tree *x*)
  (logger ("Delete 9 from 1-5-9 tree:"
           (setf *x* (make-llrb-root 5 5))
           (setf *x* (llrb-insert 1 *x* 1))
           (setf *x* (llrb-insert 9 *x* 1)))
          (setf *x* (llrb-delete 9 *x*)))
  (print-tree *x*)
  (logger ("Make 1:8 tree:"
           (setf *x* (make-llrb-root 1 1))
           (loop for i from 2 to 8 do
		(setf *x* (llrb-insert i *x* i)))))  
  (print-tree *x*)
  (logger ("Delete 5 from 5-1 tree:"
           (setf *x* (make-llrb-root 5 5))
           (setf *x* (llrb-insert 1 *x* nil))
	   (setf *x* (llrb-delete 5 *x*))))
  (print-tree *x*))

(test)
  
;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

