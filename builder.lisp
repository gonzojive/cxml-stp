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

(defun make-stp-builder ()
  (make-instance 'builder))

(defclass builder ()
  ((root :accessor builder-root)
   (elements :initform nil :accessor builder-elements)
   (doctype :initform nil :accessor builder-doctype)
   (internal-subset-sink :initform nil
			 :accessor builder-internal-subset-sink)))

(defmethod sax:start-dtd ((builder builder) name publicid systemid)
  (setf (builder-doctype builder)
	(make-document-type name systemid publicid "")))

(defmethod sax:start-internal-subset ((builder builder))
  (setf (builder-internal-subset-sink builder) (cxml:make-string-sink)))

(macrolet ((def (name &rest args)
	     `(defmethod ,name ((builder builder) ,@args)
		(let ((sink (builder-internal-subset-sink builder)))
		  (when sink (,name sink ,@args))))))
  (def sax:unparsed-entity-declaration name public-id system-id notation-name)
  (def sax:external-entity-declaration kind name public-id system-id)
  (def sax:internal-entity-declaration kind name value)
  (def sax:notation-declaration name public-id system-id)
  (def sax:element-declaration name model)
  (def sax:attribute-declaration element-name attribute-name type default))

(defmethod sax:end-internal-subset ((builder builder))
  (setf (internal-subset (builder-doctype builder))
	(string-trim "[]"
		     (sax:end-document
		      (builder-internal-subset-sink builder))))
  (setf (builder-internal-subset-sink builder) nil))

(defmethod sax:start-element ((builder builder) uri lname qname attrs)
  (let ((element (make-element qname uri)))
    (dolist (a attrs)
      (let ((uri (sax:attribute-namespace-uri a)))
	(unless (equal uri "http://www.w3.org/2000/xmlns/")
	  (let ((b (make-attribute (sax:attribute-value a)
				   (sax:attribute-qname a)
				   uri)))
	    (add-attribute element b)))))
    (let ((parent (car (builder-elements builder))))
      (if parent
	  (append-child parent element)
	  (setf (builder-root builder) element)))
    (push element (builder-elements builder))))

(defmethod sax:end-element ((builder builder) uri lname qname)
  (declare (ignore uri lname qname))
  (pop (builder-elements builder)))

;; zzz normalisieren?
(defmethod sax:characters ((builder builder) data)
  (append-child (car (builder-elements builder)) (make-text data)))

(defmethod sax:processing-instruction ((builder builder) target data)
  (append-child (car (builder-elements builder))
		(make-processing-instruction target data)))

(defmethod sax:comment ((builder builder) data)
  (append-child (car (builder-elements builder)) (make-comment data)))

(defmethod sax:end-document ((builder builder))
  (let ((result (make-document (builder-root builder))))
    (when (builder-doctype builder)
      (setf (document-type result) (builder-doctype builder)))
    result))
