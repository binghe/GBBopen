;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/llrb-tree.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Jul 22 14:27:29 2009 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                               LLRB Trees
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Dan Corkill & Eric O'Connor
;;;
;;; Copyright (C) 2008-2009, Dan Corkill <corkill@GBBopen.org> 
;;; Part of the GBBopen Project (see LICENSE for license information).
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  06-26-08 File created.  (Corkill)
;;;  07-21-09 File released.  (O'Connor)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(compare
            compare&
            llrb-node                   ; structure type
            llrb-tree                   ; structure type
            llrb-tree-count
            llrb-tree-delete
            llrb-tree-node              ; not yet documented
            llrb-tree-p
            llrb-tree-test
            llrb-tree-value
            make-llrb-tree
            map-llrb-tree)))

;;; ---------------------------------------------------------------------------

(defstruct (llrb-node 
            (:constructor make-llrb-node (key value red-p))
            (:copier))
  key
  value
  (red-p nil :type boolean)
  (left nil)
  (right nil))

(defmethod print-object ((object llrb-node) stream)
  (cond
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (object stream)
        (format stream "~s ~:[B~;R~] ~s ~s"
                (llrb-node-key object)
                (llrb-node-red-p object)
                (let ((left (llrb-node-left object)))
                  (and left (llrb-node-key left)))
                (let ((right (llrb-node-right object)))
                  (and right (llrb-node-key right)))))
      ;; Print-object must return object:
      object)))

;;; ---------------------------------------------------------------------------
;;;  LLRB tree object (contains the count, test, and rtb-nodes)
;;;
;;;  `test' is a two-argument comparison function that must return a fixnum 
;;;   where:
;;;         negative => a < b
;;;         zero     => a = b
;;;         positive => a > b

(defstruct (llrb-tree
            (:constructor %make-llrb-tree (test))
            (:copier nil))
  (count 0 :type integer)
  test
  (root nil :type (or llrb-node null)))
            
(defmethod print-object ((object llrb-tree) stream)
  (cond
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (object stream)
        (format stream "~@[~s ~]~s with ~s entr~:@p"
                (let ((test (llrb-tree-test object)))
                  (if (functionp test)
                      (nth-value 2 (function-lambda-expression test))
                      test))
                (type-of object)
                (llrb-tree-count object)))
      ;; Print-object must return object:
      object)))

;;; ---------------------------------------------------------------------------

(defvar *%llrb-delete-succeeded%* nil)  ; used to signal successful llrb-delete
(defvar *%llrb-insert-succeeded%* nil)  ; used to signal added llrb-node

;;; ---------------------------------------------------------------------------
;;;  Move these to declared-numerics and complete them with all declared types

(defun compare (a b)
  (- a b))

(defun compare& (a b)
  (-& a b))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro llrb-node-is-red? (node)
    (with-once-only-bindings (node)
      `(and ,node (llrb-node-red-p ,node))))
  
  (defmacro llrb-node-left-left (node)
    `(let ((left (llrb-node-left ,node)))
       (when left (llrb-node-left left))))

  (defmacro llrb-node-right-left (node)
    `(let ((right (llrb-node-right ,node)))
       (when right (llrb-node-left right))))

  (defmacro llrb-node-left-right (node)
    `(let ((left (llrb-node-left ,node)))
       (when left (llrb-node-right left))))
  
  ) ;; end eval-when

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-rotate-left (node)
    (declare (type llrb-node node))
    (let ((x (llrb-node-right node)))
      (declare (type llrb-node x))
      (setf (llrb-node-right node) (llrb-node-left x))
      (setf (llrb-node-left x) node)
      (setf (llrb-node-red-p x) (llrb-node-red-p node))
      (setf (llrb-node-red-p node) 't)
      ;; Return x:
      x)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-rotate-right (node)
    (let ((x (llrb-node-left node)))
      (declare (type llrb-node x))
      (setf (llrb-node-left node) (llrb-node-right x))
      (setf (llrb-node-right x) node)
      (setf (llrb-node-red-p x) (llrb-node-red-p node))
      (setf (llrb-node-red-p node) 't)
      ;; Return x:
      x)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-color-flip (node)
    (declare (type llrb-node node))
    ;; Toggle node, left, & right colors:
    (let ((left (llrb-node-left node))
          (right (llrb-node-right node)))
      (declare (type (or llrb-node null) left right))
      (setf (llrb-node-red-p node) (not (llrb-node-red-p node)))
      (when left
        (setf (llrb-node-red-p left) (not (llrb-node-red-p left))))
      (when right
        (setf (llrb-node-red-p right) (not (llrb-node-red-p right)))))))
  
;;; ===========================================================================
;;;  Insertion

(defun make-llrb-root (key value)
  (setf *%llrb-insert-succeeded%* 't)
  (make-llrb-node key value nil))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-fixup (node)
    (declare (type llrb-node node))
    (when (and (llrb-node-is-red? (llrb-node-right node))
               (not (llrb-node-is-red? (llrb-node-left node))))
      (setf node (llrb-rotate-left node)))
    ;; Fix two reds in a row on the way up:
    (when (and (llrb-node-is-red? (llrb-node-left node))
               (llrb-node-is-red? (llrb-node-left (llrb-node-left node))))
      (setf node (llrb-rotate-right node)))
    ;; Split (eliminate) 4-nodes on the way up:
    (when (and (llrb-node-is-red? (llrb-node-left node))
               (llrb-node-is-red? (llrb-node-right node)))
      (llrb-color-flip node))
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-insert (key node value test)
    (declare (type (or llrb-node null) node)
             (type function test))
    (cond
     ;; Empty LLRB tree (insert at the bottom):
     ((not node) 
      (setf *%llrb-insert-succeeded%* 't)
      (make-llrb-node key value 't))
     ;; Do an interior insert:
     (t (let ((result (funcall test key (llrb-node-key node))))
          ;; Standard BST insert:
          (cond 
           ((zerop& result) 
            (setf (llrb-node-value node) value))
           ((minusp& result)
            (setf (llrb-node-left node)
                  (llrb-insert key (llrb-node-left node) value test)))
           (t (setf (llrb-node-right node)
                    (llrb-insert key (llrb-node-right node) value test))))
          (setf node (llrb-fixup node)))
        ;; Return node
        node))))

;;; ===========================================================================
;;;  Deletion

(with-full-optimization ()
  (defun llrb-move-red-left (node)
    (declare (type llrb-node node))
    (llrb-color-flip node)
    (when (llrb-node-is-red? (llrb-node-right-left node))
      (setf (llrb-node-right node) (llrb-rotate-right (llrb-node-right node)))
      (setf node (llrb-rotate-left node))
      (llrb-color-flip node))
    ;; Return node:
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-move-red-right (node)
    (declare (type llrb-node node))
    (llrb-color-flip node)
    (when (llrb-node-is-red? (llrb-node-left-left node))
      (setf node (llrb-rotate-right node))
      (llrb-color-flip node))
    ;; Return node:
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-delete-min (node)
    (declare (type (or llrb-node null) node))
    (unless node (error "Have to deal with this & root!"))
    ;; Remove node on bottom level (node must be red by invariant):
    (unless (llrb-node-left node)
      (return-from llrb-delete-min nil))
    ;; Push red link down, if necessary:
    (when (and (not (llrb-node-is-red? (llrb-node-left node)))
               (not (llrb-node-is-red? (llrb-node-left-left node))))
      (setf node (llrb-move-red-left node)))
    ;; Move down one level:
    (setf (llrb-node-left node) (llrb-delete-min (llrb-node-left node)))
    ;; Fix right-leaning red links and eliminate 4-nodes on the way up:
    (llrb-fixup node)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-delete (key node test)
    (declare (type (or llrb-node null) node)
             (type function test))
    (when node
      (cond 
       ;; Left: 
       ((minusp& (funcall test key (llrb-node-key node)))
        ;; push red right, if necessary:
        (when (and (not (llrb-node-is-red? (llrb-node-left node)))
                   (not (llrb-node-is-red? (llrb-node-left-left node))))
          (setf node (llrb-move-red-left node)))
        ;; move down (left):
        (setf (llrb-node-left node) 
              (llrb-delete key (llrb-node-left node) test)))
       ;; Equal or Right:
       (t 
        ;; Rotate to push red right:
        (when (llrb-node-is-red? (llrb-node-left node))
          (setf node (llrb-rotate-right node)))
        ;; If Equal and at bottom, delete node (by returning nil):
        (when (and (zerop& (funcall test key (llrb-node-key node)))
                   (not (llrb-node-right node)))
          (setf *%llrb-delete-succeeded%* 't)
          (return-from llrb-delete nil))
        ;; Push red right, if necessary:
        (when (and (not (llrb-node-is-red? (llrb-node-right node)))
                   (not (llrb-node-is-red? (llrb-node-right-left node))))
          (setf node (llrb-move-red-right node)))
        (cond 
         ;; If equal and not at bottom, replace current node's values with
         ;; successor's values and delete successor:
         ((zerop& (funcall test key (llrb-node-key node)))
          (let* ((right (llrb-node-right node))
                 (successor (llrb-min-node right)))
            (setf *%llrb-delete-succeeded%* 't)
            (setf (llrb-node-key node) (llrb-node-key successor))
            (setf (llrb-node-value node) (llrb-node-value successor))
            (setf (llrb-node-right node) (llrb-delete-min right))))
         ;; Otherwise, move down (right):
         (t (setf (llrb-node-right node) 
                  (llrb-delete key (llrb-node-right node) test))))))
      ;; Fix right-leaning red links and eliminate 4-nodes on the way up:
      (llrb-fixup node))))

;;; ===========================================================================
;;;  Traversal & retrieval

(with-full-optimization ()
  (defun llrb-get-node (key node test)
    (declare (type (or llrb-node null) node)
             (type function test))
    (while node
      (let ((result (funcall test key (llrb-node-key node))))
        (cond
         ((zerop& result) (return node))
         ((minusp& result) (setf node (llrb-node-left node)))
         (t (setf node (llrb-node-right node))))))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-min-node (node)
    (declare (type (or llrb-node null) node))
    (let ((result node))
      (while (setf node (llrb-node-left node))
        (setf result node))
      result)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-map (fn node)
    (declare (type (or llrb-node null) node)
             (type function fn))
    (when node
      (llrb-map fn (llrb-node-left node))
      (funcall fn node)
      (llrb-map fn (llrb-node-right node)))))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-prefix-map (fn node)
    (declare (type (or llrb-node null) node)
             (type function fn))
    (when node
      (funcall fn node)
      (llrb-prefix-map fn (llrb-node-left node))
      (llrb-prefix-map fn (llrb-node-right node)))))

;;; ===========================================================================
;;;  Public interface

(defun make-llrb-tree (&optional (comparision-test #'compare))
  (%make-llrb-tree (coerce comparision-test 'function)))

;;; ---------------------------------------------------------------------------

(defun llrb-tree-node (key llrb-tree)
  (let ((root-node (llrb-tree-root llrb-tree)))
    (llrb-get-node key root-node (llrb-tree-test llrb-tree))))

;;; ---------------------------------------------------------------------------

(defun llrb-tree-value (key llrb-tree &optional default)
  (let ((tree-node (llrb-tree-node key llrb-tree)))
    (if tree-node
        (values (llrb-node-value tree-node) 't)
        default)))

;;; ---------------------------------------------------------------------------

(defun (setf llrb-tree-value) (value key llrb-tree &optional default)
  (declare (ignore default))
  (let ((root-node (llrb-tree-root llrb-tree))
        (*%llrb-insert-succeeded%* nil))
    (setf (llrb-tree-root llrb-tree)
          (if root-node
              (llrb-insert key root-node value (llrb-tree-test llrb-tree))
              (make-llrb-root key value)))
    (when *%llrb-insert-succeeded%* (incf (llrb-tree-count llrb-tree))))
  value)

;;; ---------------------------------------------------------------------------

(defun llrb-tree-delete (key llrb-tree)
  (let ((root-node (llrb-tree-root llrb-tree))
        (*%llrb-delete-succeeded%* nil))
    (setf (llrb-tree-root llrb-tree)
          (llrb-delete key root-node (llrb-tree-test llrb-tree)))
    (when *%llrb-delete-succeeded%*
      (decf (llrb-tree-count llrb-tree))
      ;; Return success
      't)))

;;; ---------------------------------------------------------------------------

(defun map-llrb-tree (fn llrb-tree)
  (llrb-map 
   #'(lambda (node) 
       (funcall fn (llrb-node-key node) (llrb-node-value node)))
   (llrb-tree-root llrb-tree)))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

