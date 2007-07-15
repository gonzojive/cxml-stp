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

(in-package :cxml-stp-impl)

#+sbcl
(declaim (optimize (debug 2)))


;;;; Class PARENT-NODE

;;; base URI

(defgeneric (setf base-uri) (newval node)
  (:documentation
   "@arg[newval]{string, the new base URI}
    @arg[node]{an @class{parent-node}}
    @return{the new base URI}
    @short{Sets the node's base URI.}"))

(defgeneric %base-uri (node))
(defmethod %base-uri ((node node)) (or (slot-value node '%base-uri) ""))
(defmethod (setf %base-uri) (newval (node node))
  (when (and (plusp (length newval))
	     *check-uri-syntax*
	     (not (search "://" newval)))
    (warn "base URI does not look like an absolute URL: ~S" newval))
  (setf (slot-value node '%base-uri) (or newval "")))

(defun maybe-fill-in-base-uri (removed-child)
  (when (typep removed-child 'element)
    (fill-in-base-uri removed-child)))

(defun fill-in-base-uri (removed-child)
  (setf (%base-uri removed-child)
	(find-base-uri removed-child)))

(defun find-base-uri (node)
  (loop
     for n = node then parent
     for parent = (parent n)
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
  "@arg[child]{a @class{node}}
   @arg[parent]{a @class{parent-node}}
   @short{Adds @code{child} as the first child of @code{parent}, if allowed.}

   Signals an error if the child already has a parent."
  (insert-child child parent 0))

(defun append-child (parent child)
  "@arg[child]{a @class{node}}
   @arg[parent]{a @class{parent-node}}
   Adds @code{child} as the last child of @code{parent}, if allowed.

   Signals an error if the child already has a parent."
  (insert-child parent child (length (%children parent))))

(defun delete-nth-child (idx parent)
  "@arg[idx]{a non-negative integer}
   @arg[parent]{a @class{parent-node}}
   Removes child @code{idx} of @code{parent}, if allowed."
  (let ((old (%children parent)))
    (delete-child-if (constantly t) parent :start idx :count 1)
    (elt old idx)))

(defun delete-child (child parent &key from-end test start end count key)
  "@arg[child]{an object}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @return{a @class{node} or nil}
   Searches for an child node of @code{parent} that satisfies the @code{test}
   and removes it, if allowed."
  (setf test (or test #'eql))
  (delete-child-if (lambda (c) (funcall test child c))
		   parent
		   :from-end from-end
		   :start start
		   :end end
		   :count count
		   :key key))

(defun insert-child-before (parent new-child ref-child)
  "@arg[parent]{a @class{parent-node}}
   @arg[new-child]{a @class{node}}
   @arg[ref-child]{a @class{node}}
   @short{Adds @code{new-child} before @code{ref-child} as a child node of
   @code{parent}, if allowed.}

   Signals an error if the child already has a parent.

   Also signals an error if @code{ref-child} is not a child of @code{parent}."
  (let ((idx (child-position ref-child parent)))
    (unless idx
      (stp-error "referenced child not found: ~A" ref-child))
    (insert-child parent new-child idx)))

(defun insert-child-after (parent new-child ref-child)
  "@arg[parent]{a @class{parent-node}}
   @arg[new-child]{a @class{node}}
   @arg[ref-child]{a @class{node}}
   @short{Adds @code{new-child} after @code{ref-child} as a child node of
   @code{parent}, if allowed.}

   Signals an error if the child already has a parent.

   Also signals an error if @code{ref-child} is not a child of @code{parent}."
  (let ((idx (child-position ref-child parent)))
    (unless idx
      (stp-error "referenced child not found: ~A" ref-child))
    (insert-child parent new-child (1+ idx))))

(defun replace-child (parent new-child old-child)
  "@arg[parent]{a @class{parent-node}}
   @arg[new-child]{a @class{node}}
   @arg[old-child]{a @class{node}}
   @short{Adds @code{new-child} instead of @code{old-child} as a child node of
   @code{parent}, if allowed.}

   Signals an error if the new child already has a parent.

   Also signals an error if @code{old-child} is not a child of @code{parent}."
  (let ((idx (child-position old-child parent)))
    (unless idx
      (stp-error "old child not found: ~A" old-child))
    (replace-children parent
		      (list new-child)
		      :start1 idx
		      :end1 (1+ idx))))

;;; CHILDREN-related functions we define

(defgeneric insert-child (parent child position)
  (:documentation
   "@arg[parent]{a @class{parent-node}}
    @arg[child]{a @class{node}}
    @arg[position]{a non-negative integer}
    @short{Adds @code{child} as a child node of @code{parent} at position
      @code{position} if allowed.}

   Signals an error if the new child already has a parent.

   Also signals an error if @code{position} is greater than the number
   @code{parent}'s child nodes."))

(defgeneric delete-child-if
    (predicate parent &rest args &key from-end start end count key)
  (:documentation
   "@arg[predicate]{a designator for a function of one argument that returns
     a generalized boolean}
    @arg[parent]{a @class{node}}
    @arg[from-end]{a generalized boolead}
    @arg[start, end]{bounding index designators for @code{parent}'s child list}
    @arg[key]{a designator for a function of one argument, or nil}
    @arg[test]{a designator for a function of two arguments, or nil}
    @return{a @class{node} or nil}
    Searches for an child node of @code{parent} that satisfies @code{predicate}
    and removes it, if allowed."))

(defgeneric replace-children (parent seq &key start1 end1 start2 end2)
  (:documentation
   "@arg[parent]{a @class{node}}
    @arg[seq]{a sequence of @class{node}s}
    @arg[start1, end1]{bounding index designators for @code{parent}'s child list}
    @arg[start2, end2]{bounding index designators for @code{seq}}
    Modifies the child list of @code{parent} by replacing its child nodes
    bounded by @code{start1} and @code{end1} with the elements of
    @code{seq} bounded by @code{start2} and @code{end2}, if allowed."))

(defgeneric check-insertion-allowed (parent child position))
(defgeneric check-deletion-allowed (parent child position))
(defgeneric check-replacement-allowed (parent children))

(defmethod insert-child ((parent parent-node) (child node) i)
  (check-insertion-allowed parent child i)
  (%unchecked-insert-child parent child i)
  (setf (%parent child) parent))

(defun %unchecked-insert-child (parent child i)
  (unless (%children parent)
    (setf (%children parent) (make-array 1 :fill-pointer 0 :adjustable t)))
  (let ((children (%children parent)))
    (cxml-dom::make-space children 1)
    (cxml-dom::move children children i (1+ i) (- (length children) i))
    (incf (fill-pointer children))
    (setf (elt children i) child))
  (setf (%parent child) parent))

(defun %nuke-nth-child (parent i)
  (let* ((c (%children parent))
	 (loser (elt c i)))
    (maybe-fill-in-base-uri loser)
    (cxml-dom::move c c (1+ i) i (- (length c) i 1))
    (decf (fill-pointer c))
    (setf (%parent loser) nil)))

(defmethod delete-child-if
    (predicate (parent parent-node) &key from-end start end count key)
  (let ((c (%children parent))
	(result nil))
    (setf start (or start 0))
    (setf key (or key #'identity))
    (setf count (or count (length c)))
    (setf end (or end (length c)))
    (unless (and (<= 0 start (length c))
		 (<= end (length c))
		 (<= start end))
      (stp-error "invalid bounding index designators"))
    (when c			  ;nothing to delete if not a vector yet
      (if from-end
	  (let ((i (1- end)))
	    (cxml::while (and (>= i start) (plusp count))
	      (let ((loser (elt c i)))
		(when (funcall predicate (funcall key loser))
		  (check-deletion-allowed parent loser i)
		  (maybe-fill-in-base-uri loser)
		  (cxml-dom::move c c (1+ i) i (- (length c) i 1))
		  (decf (fill-pointer c))
		  (setf (%parent loser) nil)
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
		   (maybe-fill-in-base-uri loser)
		   (cxml-dom::move c c (1+ i) i (- (length c) i 1))
		   (decf (fill-pointer c))
		   (setf (%parent loser) nil)
		   (decf count)
		   (setf result t))
		  (t
		   (incf i))))
	      (decf tbd)))))
    result))

;; zzz geht das nicht besser?
(defmethod replace-children
    ((parent parent-node) seq &key start1 end1 start2 end2)
  (setf start1 (or start1 0))
  (setf start2 (or start2 0))
  (setf end1 (or end1 (length (%children parent))))
  (setf end2 (or end2 (length seq)))
  (let ((old (%children parent)))
    (unless (and (<= 0 start1 (length old))
		 (<= end1 (length old))
		 (<= start1 end1))
      (error "invalid bounding index designators"))
    (let* ((new (if old
		    (replace (alexandria:copy-array old)
			     seq
			     :start1 start1
			     :end1 end1
			     :start2 start2
			     :end2 end2)
		    (make-array (length seq)
				:fill-pointer (length seq)
				:adjustable t
				:initial-contents seq)))
	   (new-list (coerce new 'list)))
      (unless (equal new-list (remove-duplicates new-list))
	(stp-error "duplicate children after replacement"))
      (loop
	 for i from start2 below end2
	 for winner = (elt seq i)
	 for p = (parent winner)
	 do
	   (when (and p (not (eq p parent)))
	     (stp-error "node already has a parent: ~A" winner)))
      (check-replacement-allowed parent new)
      (setf (%children parent) new)
      (loop
	 for i from start1 below end1
	 for loser = (elt old i)
	 do
	 (unless (find loser seq :start start2 :end end2)
	   (maybe-fill-in-base-uri loser)
	   (setf (%parent loser) nil)))
      (loop
	 for i from start2 below end2
	 for winner = (elt seq i)
	 do
	   (setf (%parent winner) parent))))
  t)

(defreader parent-node ((base-uri "") (children nil))
  (setf (%base-uri this) base-uri)
  (dolist (child children)
    (append-child this child)))
