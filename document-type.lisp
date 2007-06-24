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


;;;; Class DOCUMENT-TYPE

(defun make-document-type
    (root-element-name &optional system-id public-id internal-subset)
  (let ((result (make-instance 'document-type)))
    (setf (root-element-name result) root-element-name)
    (setf (system-id result) system-id)
    (setf (public-id result) public-id)
    (setf (internal-subset result) internal-subset)
    result))

(defmethod copy ((node document-type))
  (let ((result (make-instance 'document-type)))
    (setf (root-element-name result) (root-element-name node))
    (setf (system-id result) (system-id node))
    (setf (public-id result) (public-id node))
    (setf (internal-subset result) (internal-subset node))
    result))

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defun check-xml-name (str)
  (unless (namep str)
    (stp-error "not a Name: ~S" str)))

(defmethod (setf root-element-name) :before (newval (node document-type))
  (unless (zerop (length newval))
    (check-xml-name newval)))

(defmethod (setf internal-subset) :before (newval (node document-type))
  (unless (zerop (length newval))
    (handler-case
	(cxml:parse-rod
	 (concatenate 'string "<!DOCTYPE dummy [" newval "]><dummy/>")
	 nil)
      (cxml:well-formedness-violation (c)
	(stp-error "attempt to set internal subset to a value that is not ~
                    well-formed: ~A"
		   c)))))

(defmethod (setf public-id) :before (newval (node document-type))
  (when (equal newval "")
    (setf newval nil))
  (when (and newval (null (system-id node)))
    (stp-error "attempt to set public-id, but no system-id is set"))
  ;; zzz hier muss mehr geprueft werden?
  ;; was ist mit ' und " gleichzeitig?
  (unless (every #'cxml::pubid-char-p newval)
    (stp-error "malformed public id: ~S" newval)))

(defmethod (setf system-id) :before (newval (node document-type))
  (when (equal newval "")
    (setf newval nil))
  (when (and (public-id node) (null newval))
    (stp-error "attempt to remove system-id, but public-id is set")))

(defmethod string-value ((node document-type))
  "")

(defmethod serialize ((node document-type) handler)
  (sax:start-dtd handler
		 (root-element-name node)
		 (public-id node)
		 (system-id node))
  (unless (zerop (length (internal-subset node)))
    (sax:unparsed-internal-subset handler (internal-subset node)))
  (sax:end-dtd handler))


;;; printing

(defmethod slots-for-print-object append ((node document-type))
  '((:root-element-name root-element-name)
    (:system-id system-id)
    (:public-id public-id)
    (:internal-subset internal-subset)))

(defreader document-type
    (root-element-name system-id public-id internal-subset)
  (setf (root-element-name this) root-element-name)
  (setf (system-id this) system-id)
  (setf (public-id this) public-id)
  (setf (internal-subset this) internal-subset))
