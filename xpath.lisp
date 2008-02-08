;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 Ivan Shvedunov. All rights reserved.
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

(defun vector->pipe (vector &optional (start 0))
  (if (>= start (length vector))
      nil
      (xpath::make-pipe (elt vector start)
			(vector->pipe vector (1+ start)))))


;;;; XPath protocol implementation for STP

;;;; FIXME: xpath-protocol:child-pipe destructively normalizes the STP tree!

(defmethod xpath-protocol:local-name ((node stp:node))
  (local-name node))

(defmethod xpath-protocol:namespace-prefix ((node stp:node))
  (namespace-prefix node))

(defmethod xpath-protocol:parent-node ((node stp:node))
  (stp:parent node))

(defmethod xpath-protocol:namespace-uri ((node stp:node))
  (namespace-uri node))

(defmethod xpath-protocol:qualified-name ((node stp:node))
  (qualified-name node))

(defmethod xpath-protocol:base-uri ((node dom:node))
  (stp:base-uri node))

(defmethod xpath-protocol:child-pipe ((node stp:node))
  nil)

(defmethod xpath-protocol:child-pipe ((node stp:parent-node))
  (normalize-text-nodes! node)
  (vector->pipe (%children node)))

(defmethod xpath-protocol:attribute-pipe ((node stp:node))
  nil)

(defmethod xpath-protocol:attribute-pipe ((node stp:element))
  (list-attributes node))

(defmethod xpath-protocol:namespace-pipe ((node stp:node))
  (when (stp:parent node)
    (xpath-protocol:namespace-pipe (stp:parent node))))

(defstruct (stp-namespace
	     (:constructor make-stp-namespace (parent prefix uri)))
  parent
  prefix
  uri)

(defmethod xpath-protocol:child-pipe ((node stp-namespace)) nil)
(defmethod xpath-protocol:attribute-pipe ((node stp-namespace)) nil)
(defmethod xpath-protocol:namespace-pipe ((node stp-namespace)) nil)

(defmethod xpath-protocol:parent-node ((node stp-namespace))
  (stp-namespace-parent node))
(defmethod xpath-protocol:local-name ((node stp-namespace))
  (stp-namespace-prefix node))
(defmethod xpath-protocol:qualified-name ((node stp-namespace))
  (stp-namespace-prefix node))
(defmethod xpath-protocol:namespace-uri ((node stp-namespace))
  (stp-namespace-uri node))

(defmethod xpath-protocol:namespace-pipe ((node stp:element))
  (let ((node node)
	(table (make-hash-table :test 'equal))
	(current '()))
    (labels ((yield (prefix uri)
	       (unless (gethash prefix table)
		 (let ((nsnode (make-stp-namespace node prefix uri)))
		   (setf (gethash prefix table) nsnode)
		   (push nsnode current))))
	     (iterate ()
	       (if current
		   (cons (pop current) #'iterate)
		   (recurse)))
	     (recurse ()
	       (etypecase node
		 (null)
		 (element
		     (map-extra-namespaces #'yield node)
		   (yield (%namespace-prefix node)
			  (%namespace-uri node))
		   (dolist (a (%attributes node))
		     (when (plusp (length (namespace-prefix a)))
		       (yield (namespace-prefix a) (namespace-uri a))))
		   (setf node (stp:parent node))
		   (iterate))
		 (document
		  (yield "xml" "http://www.w3.org/XML/1998/namespace")
		  (yield "xmlns" "http://www.w3.org/2000/xmlns/")
		  (setf node nil)
		  (iterate)))))
      (recurse))))

(defmethod xpath-protocol:string-value ((node node))
  (string-value node))

(defmethod xpath-protocol:node-type-p ((node node) type)
  (declare (ignore type))
  nil)

(defmethod xpath-protocol:node-type-p ((node stp-namespace) type)
  (declare (ignore type))
  nil)

(macrolet ((deftypemapping (class keyword)
	     `(defmethod xpath-protocol:node-type-p
		  ((node ,class) (type (eql ,keyword)))
		t)))
  (deftypemapping comment :comment)
  (deftypemapping processing-instruction :processing-instruction)
  (deftypemapping text :text)
  (deftypemapping attribute :attribute)
  (deftypemapping element :element)
  (deftypemapping stp-namespace :namespace))

(defun normalize-text-nodes! (node)
  (when (typep node 'stp:parent-node)
    (let ((children (%children node)))
      (when (loop
	       for child across children
	       for a = nil then b
	       for b = (typep child 'text)
	       thereis (and b (or a (zerop (length (stp:data child))))))
	(let ((previous nil)
	      (results '()))
	  (stp:do-children (child node)
	    (cond
	      ((not (typep child 'stp:text))
	       (when previous
		 (push (stp:make-text
			(apply #'concatenate 'string (nreverse previous)))
		       results)
		 (setf (%parent (car results)) node)
		 (setf previous nil))
	       (push child results))
	      (previous
	       (push (stp:data child) previous))
	      ((zerop (length (stp:data child))))
	      (t
	       (setf previous (list (stp:data child))))))
	  (when previous
	    (push (stp:make-text
		   (apply #'concatenate 'string (nreverse previous)))
		  results)
	    (setf (%parent (car results)) node))
	  (setf (cxml-stp-impl::%children node)
		(let ((n (length results)))
		  (make-array n
			      :fill-pointer n
			      :initial-contents (nreverse results)))))))))
