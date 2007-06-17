;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cxml-stp)

#+sbcl
(declaim (optimize (debug 2)))

(defclass parent-node (node)
  ((%base-uri)
   (%children :accessor %children)))


;;; base URI

(defvar *check-uri-syntax* t)

(defgeneric %base-uri (node))
(defmethod %base-uri ((node node)) (or (slot-value node '%base-uri) ""))
(defmethod (setf %base-uri) (newval (node node))
  (when (and newval *check-uri-syntax* (not (search "://" newval)))
    (warn "base URI does not look like an absolute URL: ~S" newval))
  (setf (slot-value node '%base-uri) (or newval "")))

(defun fill-in-base-uri (removed-child)
  (setf (%base-uri removed-child)
	(find-base-uri removed-child)))

(defun find-base-uri (node)
  (loop
     for n = node then parent
     for parent = (parent node)
     for uri = (%base-uri n)
     while (and (equal uri "") parent)
     finally (return uri)))

(defgeneric (setf base-uri) (newval node))


;;;; Children


;;; CHILDREN-related methods on NODE

(defmethod map-children (result-type fn (node parent-node))
  (map result-type fn (%children node)))


;;; CHILDREN-related convenience functions

(defun prepend-child (child parent)
  (insert-child child parent 0))

(defun append-child (child parent)
  (insert-child child parent (length (%children parent))))

(defun nth-child (idx parent)
  (elt (%children parent) idx))

(defun find-child
    (value parent &rest args &key from-end (start 0) end key test)
  (apply #'find value (%children parent) args))

(defun find-child-if
    (predicate parent &rest args &key from-end (start 0) end key)
  (apply #'find-if predicate (%children parent) args))

(defun filter-children
    (predicate parent &rest args &key from-end (start 0) end count key)
  (apply #'remove-if-not predicate (%children parent) args))

(defun delete-child (child parent &key from-end test start end count key)
  (setf test (or test #'eql))
  (delete-child-if (lambda (c) (funcall test child c))
		   parent
		   :from-end from-end
		   :start start
		   :end end
		   :key key))
							    

;;; CHILDREN-related functions we define

(defgeneric insert-child (parent child position))

(defgeneric delete-child-if
    (predicate parent &rest args &key from-end start end count key))

(defgeneric replace-children (parent seq &key start1 end1 start2 end2))

(defgeneric check-insertion-allowed (parent child position))
(defgeneric check-deletion-allowed (parent child position))
(defgeneric check-replacement-allowed (parent children))

(defmethod insert-child ((parent parent-node) child i)
  (check-insertion-allowed parent child i)
  (unless (%children parent)
    (setf (%children parent) (make-array 1 :fill-pointer 0 :adjustable t)))
  (let ((children (%children parent)))
    (cxml-dom::make-space children 1)
    (move children children i (1+ i) (- (length children) i))
    (incf (fill-pointer children))
    (setf (elt children i) child))
  (setf (parent child) parent))

(defmethod delete-child-if
    (predicate (parent parent-node)
     &rest args
     &key from-end (start 0) end count key)
  (let ((c (%children parent))
	(result nil))
    (setf key (or key #'identity))
    (setf count (or count (length c)))
    (setf end (or end (length c)))
    (unless (and (<= 0 start (length c))
		 (<= end (length c))
		 (<= start end))
      (error "invalid bounding index designators"))
    (when c			  ;nothing to delete if not a vector yet
      (if from-end
	  (let ((i (1- end)))
	    (cxml::while (and (>= i start) (plusp count))
	      (let ((loser (elt c i)))
		(when (funcall predicate (funcall key loser))
		  (check-deletion-allowed parent loser i)
		  (when (typep loser 'element)
		    (fill-in-base-uri loser))
		  (cxml-dom::move c c (1+ i) i (- (length c) i 1))
		  (decf (fill-pointer c))
		  (setf (parent loser) nil)
		  (decf count)
		  (setf result t)))
	      (decf i)))
	  (let ((tbd (- end start))
		(i start))
	    (cxml::while (and (plusp tbd) (plusp count))
	      (let ((loser (elt c i)))
		(cond
		  ((funcall predicate (funcall key loser))
		   (check-deletion-allowed parent loser i)
		   (when (typep loser 'element)
		     (fill-in-base-uri loser))
		   (cxml-dom::move c c (1+ i) i (- (length c) i 1))
		   (decf (fill-pointer c))
		   (setf (parent loser) nil)
		   (decf count)
		   (setf result t))
		  (t
		   (incf i))))
	      (decf tbd)))))
    result))

;; zzz optimize me
(defmethod replace-children
    ((parent parent-node) seq &rest args &key start1 end1 start2 end2)
  (setf start1 (or start1 0))
  (setf start2 (or start2 0))
  (setf end1 (or end1 (length (%children parent))))
  (setf end2 (or end2 (length seq)))
  (let* ((old (%children parent))
	 (tmp (replace (copy-seq old)
		       seq
		       :start1 start1
		       :end1 end1
		       :start2 start2
		       :end2 end2)))
    (check-replacement-allowed parent tmp)
    (setf (%children parent)
	  (cond
	    ((zerop (length tmp))
	     nil)
	    ((listp tmp)
	     (make-array (length tmp)
			 :fill-pointer (length tmp)
			 :adjustable t
			 :initial-contents tmp))
	    (t
	     tmp)))
    (loop
       for i from start1 below end1
       for loser = (elt old i)
       do
	 (unless (find loser seq :start start2 :end end2)
	   (fill-in-base-uri loser)
	   (setf (parent loser) nil)))
    (loop
       for i from start2 below end2
       for winner = (elt seq i)
       do
	 (unless (find winner old :start start1 :end end1)
	   (setf (parent winner) parent))))
  t)
