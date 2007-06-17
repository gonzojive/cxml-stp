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


;;;; Class ATTRIBUTE

(defclass attribute (leaf-node)
  ((local-name :reader local-name :accessor (setf %local-name))
   (prefix :reader namespace-prefix :accessor %prefix)
   (namespace-uri :reader namespace-uri :accessor (setf %namespace-uri))
   (value :accessor attribute-value)))

(defun make-attribute (value name &optional (uri ""))
  (let ((result (make-instance 'attribute)))
    (multiple-value-bind (prefix local-name)
	(cxml::split-qname name)
      (setf (local-name result) uri)
      (rename-attribute result prefix uri)
      (setf (attribute-value result) value))
    result))

(defmethod copy ((node attribute))
  (let ((result (make-instance 'attribute)))
    (setf (%local-name result) (%local-name node))
    (setf (%namespace-prefix result) (%namespace-prefix node))
    (setf (%namespace-uri result) (%namespace-uri node))
    (setf (attribute-value result) (attribute-value node))
    result))

(defmethod detach ((node attribute))
  (when (parent node)
    (delete-attribute node (parent node))))

(defmethod string-value ((node attribute))
  (attribute-value node))

;; zzz WRONG! this excludes surrogates, which would only be correct on SBCL
;; if we used its full character range, and is always incorrect on allegro. 
;; (The rest of cxml repeats the same mistake though.)
(defun xml-characters-p (str)
  (every (lambda (c)
	   (let ((code (char-code c)))
	     (or (eql code 9)
		 (eql code 10)
		 (eql code 13)
		 (<= 32 code #xd7ff)
		 (<= #xe000 code #xfffd)
		 (<= #x10000 code #x10ffff))))
	 str))

(defmethod (setf attribute-value) :before (newval (node attribute))
  (unless (xml-characters-p newval)
    (stp-error "new attribute value includes characters that cannot be ~
                represented in XML at all: ~S"
	       newval)))

(defmethod (setf local-name) (newval (node attribute))
  (check-nc-name newval)
  (when (equal newval "xmlns")
    (stp-error "attempt to represent a namespace declaration as an ATTRIBUTE"))
  (setf (%local-name node) newval))

(defmethod qualified-name ((node attribute))
  (let ((prefix (namespace-prefix node))
	(local-name (local-name node)))
    (if (plusp (length prefix))
	(format nil "~A:~A" prefix local-name)
	local-name)))

(defun xor (a b)
  (if a (not b) b))

(defun rename-attribute (attribute prefix uri)
  (unless prefix (setf prefix ""))
  (unless uri (setf uri ""))
  (when (equal prefix "xmlns")
    (stp-error "attempt to represent a namespace declaration as an ATTRIBUTE"))
  (when (xor (equal uri "http://www.w3.org/XML/1998/namespace")
	     (equal prefix "xml"))
    (stp-error "prefix/URI mismatch for `xml' namespace"))
  (cond
    ((zerop (length prefix))
      (unless (zerop (length uri))
	(stp-error "attribute with URI but no prefix"))
      (values
       (setf (%namespace-prefix attribute) "")
       (setf (%namespace-uri attribute) "")))
    ((zerop (length uri))
      (stp-error "attribute with prefix but no URI"))
    (t
      (let ((parent (parent attribute)))
	(when parent
	  (let ((old (find-local-namespace prefix parent)))
	    (when (and old (not (equal uri old)))
	      (stp-error "conflicting namespaces when renaming attribute")))))
      (check-nc-name prefix)
      (check-namespace-uri uri)
      (values
       (setf (%namespace-prefix attribute) prefix)
       (setf (%namespace-uri attribute) uri)))))

(defmethod unparse ((node attribute) handler)
  (stp-error "attempt to unparse an attribute in isolation"))
