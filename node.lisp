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

(defvar *check-uri-syntax* t)
(defun check-namespace-uri (uri)
  (when (and *check-uri-syntax* (not (search "://" uri)))
    (warn "namespace URI does not look like an absolute URL: ~S" uri)))


;;;; Class NODE

(defclass node ()
  ((parent :reader parent :writer (setf %parent))))

(defgeneric string-value (node))

(defun document (node)
  (check-type node node)
  (loop
     for parent = node then (parent parent)
     while (and parent (not (typep parent 'document)))
     finally (return parent)))

(defun root (node)
  (check-type node node)
  (loop
     for p = (parent node) then (parent p)
     and q = node then p
     while p
     finally (return q)))

(defgeneric base-uri (node)) ;fixme: hier muessen wir wissen, ob specified
(defmethod base-uri ((node node))
  (let ((parent (parent node)))
    (if parent
        (base-uri parent)
        "")))

(defgeneric detach (node))
(defmethod detach ((node node))
  (when (parent node)
    (delete-child node (parent node))))

;;; kinderkram
;;; das ist noch unvollstaendig
(defgeneric map-children (result-type function node))
(defmacro do-children ((var node &optional result) &body body)
  `(block nil
     (map-children (lambda (,var) ,@body) ,node)
     ,result))
(defun list-children (node)
  (map-children 'list #'identity node))

(defgeneric copy (node))
(defgeneric unparse (node handler))

;; print-object nicht vergessen

;;; (defun query (node xpath)
;;;   ;; fixme
;;;   )
