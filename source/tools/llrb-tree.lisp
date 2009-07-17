;;;; -*- Mode:Common-Lisp; Package:GBBOPEN-TOOLS; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/source/tools/llrb-tree.lisp *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Fri Jul 17 11:53:24 2009 *-*
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
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package :gbbopen-tools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(compare
            compare&
            get-llrb-tree-node
            llrb-tree-count
            llrb-tree-delete
            llrb-tree-insert
            llrb-tree-test
            make-llrb-tree
            map-llrb-tree)))

;;; ---------------------------------------------------------------------------

(defstruct (rbt-node 
            (:constructor make-rbt-node (key value red-p))
            (:copier))
  key
  value
  (red-p nil :type boolean)
  (left nil)
  (right nil))

(defmethod print-object ((object rbt-node) stream)
  (cond
   (*print-readably* (call-next-method))
   (t (print-unreadable-object (object stream)
        (format stream "~s ~:[B~;R~] ~s ~s"
                (rbt-node-key object)
                (rbt-node-red-p object)
                (let ((left (rbt-node-left object)))
                  (and left (rbt-node-key left)))
                (let ((right (rbt-node-right object)))
                  (and right (rbt-node-key right)))))
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
            (:constructor make-llrb-tree (&optional (test 'compare)))
            (:copier))
  (count 0 :type integer)
  test
  (root nil :type (or rbt-node nil)))
            
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

;;; ---------------------------------------------------------------------------
;;;  Move these to declared-numerics and complete them with all declared types

(defun compare (a b)
  (- a b))

(defun compare& (a b)
  (-& a b))

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro rbt-node-is-red? (node)
    (with-once-only-bindings (node)
      `(and ,node (rbt-node-red-p ,node))))
  
  (defmacro rbt-node-left-left (node)
    `(let ((left (rbt-node-left ,node)))
       (when left (rbt-node-left left))))

  (defmacro rbt-node-right-left (node)
    `(let ((right (rbt-node-right ,node)))
       (when right (rbt-node-left right))))

  (defmacro rbt-node-left-right (node)
    `(let ((left (rbt-node-left ,node)))
       (when left (rbt-node-right left))))
  
  ) ;; end eval-when

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-rotate-left (node)
    (declare (type rbt-node node))
    (let ((x (rbt-node-right node)))
      (declare (type rbt-node x))
      (setf (rbt-node-right node) (rbt-node-left x))
      (setf (rbt-node-left x) node)
      (setf (rbt-node-red-p x) (rbt-node-red-p node))
      (setf (rbt-node-red-p node) 't)
      ;; Return x:
      x)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-rotate-right (node)
    (let ((x (rbt-node-left node)))
      (declare (type rbt-node x))
      (setf (rbt-node-left node) (rbt-node-right x))
      (setf (rbt-node-right x) node)
      (setf (rbt-node-red-p x) (rbt-node-red-p node))
      (setf (rbt-node-red-p node) 't)
      ;; Return x:
      x)))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-color-flip (node)
    (declare (type rbt-node node))
    ;; Toggle node, left, & right colors:
    (let ((left (rbt-node-left node))
          (right (rbt-node-right node)))
      (declare (type (or rbt-node null) left right))
      (setf (rbt-node-red-p node) (not (rbt-node-red-p node)))
      (when left
        (setf (rbt-node-red-p left) (not (rbt-node-red-p left))))
      (when right
        (setf (rbt-node-red-p right) (not (rbt-node-red-p right)))))))
  
;;; ===========================================================================
;;;  Insertion

(defun make-llrb-root (key value)
  (make-rbt-node key value nil))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-fixup (node)
    (declare (type rbt-node node))
    (when (and (rbt-node-is-red? (rbt-node-right node))
               (not (rbt-node-is-red? (rbt-node-left node))))
      (setf node (llrb-rotate-left node)))
    ;; Fix two reds in a row on the way up:
    (when (and (rbt-node-is-red? (rbt-node-left node))
               (rbt-node-is-red? (rbt-node-left (rbt-node-left node))))
      (setf node (llrb-rotate-right node)))
    ;; Split (eliminate) 4-nodes on the way up:
    (when (and (rbt-node-is-red? (rbt-node-left node))
               (rbt-node-is-red? (rbt-node-right node)))
      (llrb-color-flip node))
    node))

;;; ---------------------------------------------------------------------------

(defun llrb-insert (key node value &optional (test 'compare))
  (declare (type (or rbt-node null) node))
  (cond
   ;; Empty LLRB tree (insert at the bottom):
   ((not node) (make-rbt-node key value 't))
   ;; Do an interior insert:
   (t (let ((result (funcall test key (rbt-node-key node))))
        ;; Standard BST insert:
        (cond 
         ((zerop& result) 
          (setf (rbt-node-value node) value))
         ((minusp& result)
          (setf (rbt-node-left node)
                (llrb-insert key (rbt-node-left node) value test)))
         (t (setf (rbt-node-right node)
                  (llrb-insert key (rbt-node-right node) value test))))
	(setf node (llrb-fixup node)))
      ;; Return node
      node)))

;;; ===========================================================================
;;;  Deletion

(with-full-optimization ()
  (defun llrb-lean-right (node)
    (declare (type rbt-node node))
    ;; Make a left-leaning 3-node lean to the right:
    (setf node (llrb-rotate-right node))
    (let ((right (rbt-node-right node)))
      (setf (rbt-node-red-p node) (rbt-node-red-p right))
      (setf (rbt-node-red-p right) 't))
    ;; Return node:
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-move-red-left (node)
    (declare (type rbt-node node))
    (llrb-color-flip node)
    (when (rbt-node-is-red? (rbt-node-left-right node))
      (setf (rbt-node-right node) (llrb-rotate-right (rbt-node-right node)))
      (setf node (llrb-rotate-left node))
      (llrb-color-flip node))
    ;; Return node:
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-move-red-right (node)
    (declare (type rbt-node node))
    (llrb-color-flip node)
    (when (rbt-node-is-red? (rbt-node-left-left node))
      (setf node (llrb-rotate-right node))
      (llrb-color-flip node))
    ;; Return node:
    node))

;;; ---------------------------------------------------------------------------

(with-full-optimization ()
  (defun llrb-delete-min (node)
    (declare (type (or rbt-node null) node))
    (unless node (error "Have to deal with this & root!"))
    ;; Remove node on bottom level (node must be red by invariant):
    (unless (rbt-node-left node)
      (return-from llrb-delete-min nil))
    ;; Push red link down, if necessary:
    (when (and (not (rbt-node-is-red? (rbt-node-left node)))
               (not (rbt-node-is-red? (rbt-node-left-left node))))
      (setf node (llrb-move-red-left node)))
    ;; Move down one level:
    (setf (rbt-node-left node) (llrb-delete-min (rbt-node-left node)))
    ;; Fix right-leaning red links and eliminate 4-nodes on the way up:
    (llrb-fixup node)))

;;; ---------------------------------------------------------------------------

(defun llrb-delete (key node &optional (test #'compare))
  (declare (type (or rbt-node null) node))
  (when node
    (let ((result (funcall test key (rbt-node-key node))))
      (cond 
	;; Left: 
	((minusp& result)
	 ;; push red right, if necessary:
	 (when (and (not (rbt-node-is-red? (rbt-node-left node)))
		    (not (rbt-node-is-red? (rbt-node-left-left node))))
	   (setf node (llrb-move-red-left node)))
	 ;; move down (left):
	 (setf (rbt-node-left node) 
	       (llrb-delete key (rbt-node-left node) test)))
	;; Equal or Right:
	(t 
         ;; Rotate to push red right:
	 (when (rbt-node-is-red? (rbt-node-left node))
	   (setf node (llrb-lean-right node)))
         ;; If Equal and at bottom, delete node (by returning nil):
	 (when (and (zerop& result) 
		    (not (rbt-node-right node)))
           (setf *%llrb-delete-succeeded%* 't)
	   (return-from llrb-delete nil))
         ;; Push red right, if necessary:
	 (when (and (not (rbt-node-is-red? (rbt-node-right node)))
		    (not (rbt-node-is-red? (rbt-node-left-right node))))
	   (setf node (llrb-move-red-right node)))
	 (setf result (funcall test key (rbt-node-key node)))
	 (cond 
	   ;; If equal and not at bottom, replace current node's values with
	   ;; successor's values and delete successor:
	   ((zerop& result)
	    (let* ((right (rbt-node-right node))
		   (successor (llrb-min-node right)))
              (setf *%llrb-delete-succeeded%* 't)
	      (setf (rbt-node-key node) (rbt-node-key successor))
	      (setf (rbt-node-value node) (rbt-node-value successor))
              (setf (rbt-node-right node) (llrb-delete-min right))))
	   ;; Otherwise, move down (right):
	   (t (setf (rbt-node-right node) 
		    (llrb-delete key (rbt-node-right node) test)))))))
    ;; Fix right-leaning red links and eliminate 4-nodes on the way up:
    (llrb-fixup node)))

;;; ===========================================================================
;;;  Traversal & retrieval

(defun llrb-get-node (key node test)
  (declare (type (or rbt-node null) node))
  (while node
    (let ((result (funcall test key (rbt-node-value node))))
      (cond
       ((zerop& result) (return node))
       ((minusp& result) (setf node (rbt-node-left node)))
       (t (setf node (rbt-node-right node)))))))

;;; ---------------------------------------------------------------------------

(defun llrb-min-node (node)
  (declare (type (or rbt-node null) node))
  (let ((result node))
    (while (setf node (rbt-node-left node))
      (setf result node))
    result))

;;; ---------------------------------------------------------------------------

(defun llrb-map (fn node)
  (declare (type (or rbt-node null) node))
  (when node
    (llrb-map fn (rbt-node-left node))
    (funcall fn node)
    (llrb-map fn (rbt-node-right node))))

;;; ---------------------------------------------------------------------------

(defun llrb-prefix-map (fn node)
  (declare (type (or rbt-node null) node))
  (when node
    (funcall fn node)
    (llrb-prefix-map fn (rbt-node-left node))
    (llrb-prefix-map fn (rbt-node-right node))))

;;; ===========================================================================
;;;  Public interface (MAKE-LLRB-NODE is defined above)

(defun llrb-tree-insert (key llrb-tree value)
  (let ((root-node (llrb-tree-root llrb-tree)))
    (setf (llrb-tree-root llrb-tree)
          (if root-node
              (llrb-insert key root-node value (llrb-tree-test llrb-tree))
              (make-llrb-root key value))))
  (incf (llrb-tree-count llrb-tree))
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
  (llrb-map #'(lambda (node) 
                      (funcall fn (rbt-node-key node) (rbt-node-value node)))
            (llrb-tree-root llrb-tree)))

;;; ---------------------------------------------------------------------------

(defun get-llrb-tree-node (key llrb-tree)
  (let ((root-node (llrb-tree-root llrb-tree)))
    (llrb-get-node key root-node (llrb-tree-test llrb-tree))))

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================

