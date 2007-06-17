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

(defclass attribute (leaf-node)
  ((local-name :reader local-name :accessor %local-name)
   (prefix :reader namespace-prefix :accessor %prefix)
   (namespace-uri :reader namespace-uri :accessor %namespace-uri)
   (value :accessor attribute-value)))

(defclass comment (leaf-node)
  ((data :initarg :data :accessor data)))

(defclass document-type (leaf-node)
  ((root-element-name :accessor root-element-name)
   (system-id :accessor system-id)
   (public-id :accessor public-id)
   (internal-subset :accessor internal-subset)))

(defclass document (parent-node) ())

(defclass element (parent-node)
  ((local-name :reader local-name :accessor %local-name)
   (prefix :reader namespace-prefix :accessor %prefix)
   (namespace-uri :reader namespace-uri :accessor %namespace-uri)
   (attributes :accessor %attributes)
   (namespaces :accessor %namespaces)))

(defclass leaf-node (node) ())

(defclass node ()
  ((parent :reader parent :writer (setf %parent))))

(defclass parent-node (node)
  ((%base-uri :initform nil)
   (%children :initform nil :accessor %children)))

(defclass processing-instruction (leaf-node)
  ((target :initarg :target :accessor target)
   (data :initarg :data :accessor data)))

(defclass text (leaf-node)
  ((data :initarg :data :accessor data)))
