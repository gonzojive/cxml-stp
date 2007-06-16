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

(defvar *check-uri-syntax* t)

(defgeneric %base-uri (node))
(defmethod %base-uri ((node node)) (or (slot-value node '%base-uri) ""))
(defmethod (setf %base-uri) (newval (node node))
  (when (and newval *check-uri-syntax* (not (search "://" newval)))
    (warn "base URI does not look like an absolute URL: ~S" newval))
  (setf (slot-value node '%base-uri) (or newval "")))

(defgeneric insertion-allowed (parent child position)) ;position?!

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
