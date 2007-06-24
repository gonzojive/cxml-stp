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


;;;; Class ELEMENT

(defun make-element (name &optional (uri ""))
  (let ((result (make-instance 'element)))
    (multiple-value-bind (prefix local-name)
	(cxml::split-qname name)
      (setf (namespace-prefix result) prefix)
      (setf (namespace-uri result) uri)
      (setf (local-name result) local-name))
    result))

(defmethod copy ((node element))
  (let ((result (make-instance 'element)))
    (setf (%namespace-prefix result) (%namespace-prefix node))
    (setf (%local-name result) (%local-name node))
    (setf (%namespace-uri result) (%namespace-uri node))
    (setf (%namespaces result)
	  (when (%namespaces node)
	    (alexandria:copy-hash-table (%namespaces node))))
    (setf (%attributes result) (copy-attributes node))
    (setf (%base-uri result) (find-base-uri node))
    (do-children (child node)
      (append-child result (copy child)))
    result))

(defun copy-attributes (new old)
  (mapcar (lambda (x)
	    (let ((y (copy x)))
	      (setf (%parent y) new)
	      y))
	  (%attributes old)))

(defun of-name (name &optional (uri ""))
  (lambda (x)
    (and (typep x 'element)
	 (or (null name) (equal (local-name x) name))
	 (equal (namespace-uri x) uri))))

(defun find-extra-namespace (prefix element)
  (when (%namespaces element)
    (gethash prefix (%namespaces element))))

(defun add-attribute (element attribute)
  (check-type element element)
  (check-type attribute attribute)
  (assert-orphan attribute)
  (let ((local-name (local-name attribute))
	(prefix (namespace-prefix attribute))
	(uri (namespace-uri attribute)))
    (when (and (plusp (length prefix))
	       (not (equal "xml" prefix)))
      (when (and (equal prefix (namespace-prefix element))
		 (not (equal uri (namespace-uri element))))
	(stp-error "namespace collision with element when adding ~A to ~A"
		   attribute element))
      (let ((extra-uri (find-extra-namespace prefix element)))
	(when (and extra-uri (not (equal extra-uri uri)))
	  (stp-error "collision with extra namespaces when adding ~A to ~A"
		     attribute element))))
    (let ((other (find-attribute-namespace prefix element)))
      (when (and other (not (equal other uri)))
	(stp-error "collision with attribute namespace when adding ~A to ~A"
		   attribute element)))
    (let ((old (find-attribute-named element local-name uri)))
      (when old
	(%remove-attribute old)))
    (%add-attribute attribute element)
    (setf (%parent attribute) element)))

(defun %add-attribute (attribute element)
  (push attribute (%attributes element)))

(defun %remove-attribute (attribute)
  (alexandria:deletef (%attributes (parent attribute)) attribute)
  (setf (%parent attribute) nil))

(defun remove-attribute (element attribute)
  (check-type element element)
  (check-type attribute attribute)
  (unless (eq (parent attribute) element)
    (stp-error "attempt to remove ~A from non-parent ~A" attribute element))
  (%remove-attribute attribute))

(defun find-attribute-named (element name &optional (uri ""))
  (find-attribute-if (of-name name uri) element))

(defun find-attribute-if (test element)
  (find-if test (%attributes element)))

(defun attribute-value-named (element name &optional (uri ""))
  (let ((a (find-attribute-named element name uri)))
    (if a
	(value a)
	nil)))

(defun (setf attribute-value-named) (newval element name &optional (uri ""))
  (let ((a (find-attribute-named element name uri)))
    (if a
	(setf (value a) newval)
	(add-attribute (make-attribute newval name uri) element))
    newval))

(defun list-attributes (element)
  (copy-list (%attributes element)))

(defun map-attributes (result-type fn element)
  (map result-type fn (%attributes element)))

(defun qualified-name (node)
  (let ((prefix (namespace-prefix node))
	(local-name (local-name node)))
    (if (plusp (length prefix))
	(format nil "~A:~A" prefix local-name)
	local-name)))

(defun find-namespace (prefix element)
  (cond
    ((find-local-namespace prefix element))
    ((parent element)
      (find-namespace prefix (parent element)))
    ((equal prefix "")
      "")
    (t
      nil)))

(defun find-attribute-namespace (prefix element)
  (unless (equal prefix "")
    (let ((a (find prefix
		   (%attributes element)
		   :key #'namespace-prefix
		   :test #'equal)))
      (if a
	  (namespace-uri a)
	  nil))))

(defun find-local-namespace (prefix element)
  (cond
    ((equal prefix (namespace-prefix element))
      (namespace-uri element))
    ((equal prefix "xmlns")
      "http://www.w3.org/2000/xmlns/")
    ((find-extra-namespace prefix element))
    (t
      (find-attribute-namespace prefix element))))

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defun nc-name-p (str)
  (and (namep str) (cxml::nc-name-p str)))

(defun check-nc-name (str)
  (unless (nc-name-p str)
    (stp-error "not an NCName: ~A" str)))

(defgeneric (setf local-name) (newval node))
(defmethod (setf local-name) (newval (node element))
  (check-nc-name newval)
  (setf (%local-name node) newval))

(defun (setf namespace-uri) (newval element)
  (check-type element element)
  (unless newval
    (setf newval ""))
  (unless (equal newval (%namespace-uri element))
    (if (zerop (length newval))
	(unless (zerop (length (%namespace-prefix element)))
	  (stp-error "attempt to set empty URI on element with a prefix"))
	(check-namespace-uri newval))
    (when (or (find-extra-namespace (%namespace-prefix element) element)
	      (find-attribute-namespace (%namespace-prefix element) element))
      (stp-error "cannot change element URI because of a conflicting ~
                  declaration for its prefix"))
    (when (xor (equal newval "http://www.w3.org/XML/1998/namespace")
	       (equal (%namespace-prefix element) "xml"))
      (stp-error "prefix/URI mismatch for `xml' namespace"))
    (setf (%namespace-uri element) newval))
  newval)

(defun (setf namespace-prefix) (newval element)
  (check-type element element)
  (unless newval
    (setf newval ""))
  (when (plusp (length newval))
    (check-nc-name newval))
  (let ((uri (find-local-namespace newval element)))
    (if uri
	(unless (or (equal uri (%namespace-uri element))
		    (equal newval "xml"))
	  (stp-error "conflicting declarations in namespace prefix change"))
	(when (and (equal (%namespace-uri element) "") ;not for unintialized
		   (not (zerop (length newval))))
	  (stp-error "cannot assign prefix to element in no namespace"))))
  (setf (%namespace-prefix element) newval))

(defun childp (a b)
  (loop
     for node = a then (parent node)
     while node
     thereis (eq node b)))

(defmethod check-insertion-allowed ((parent element) child i)
  (check-type child node)
  (assert-orphan child)
  (typecase child
    (element
     (when (childp parent child)
       (stp-error "attempt to add a node as its own descendant")))
    ((or comment processing-instruction text))
    (t
     (stp-error "not a valid child of an element: ~A" child))))

(defmethod check-deletion-allowed ((parent element) (child node) i))

(defmethod check-replacement-allowed ((parent element) children)
  (map nil
       (lambda (x)
	 (check-insertion-allowed parent x :dummy))
       children))

;; trivial optimization
(defmethod replace-children
    ((parent element) seq &key start1 end1 start2 end2)
  (setf start1 (or start1 0))
  (setf start2 (or start2 0))
  (setf end1 (or end1 (length (%children parent))))
  (setf end2 (or end2 (length seq)))
  (cond
    ((and (eql (- start1 end1) (length (%children parent)))
	  (eql start2 end2))
      (do-children (loser parent)
	(fill-in-base-uri loser)
	(setf (%parent loser) nil))
      (setf (fill-pointer (%children parent)) 0))
    (t
     (call-next-method)))
  t)

(defun add-extra-namespace (element prefix uri)
  (unless prefix (setf prefix ""))
  (unless uri (setf uri ""))
  (unless
      (cond
	((equal prefix "xmlns")
	 (unless (equal uri "")
	   (stp-error "attempt to declare `xmlns' prefix"))
	 t)
	((equal prefix "xml")
	 (unless (equal uri "http://www.w3.org/XML/1998/namespace")
	   (stp-error "incorrect URI for `xml' namespace"))
	 t)
	((equal uri "http://www.w3.org/XML/1998/namespace")
	 (stp-error "incorrect prefix for `xml' namespace")))
    (cond
      ((plusp (length prefix))
        (check-nc-name prefix)
        (check-namespace-uri uri))
      ((plusp (length uri))
        (check-namespace-uri uri)))
    (let ((old (find-local-namespace prefix element)))
      (when (and old (not (equal old uri)))
	(stp-error "extra namespace conflicts with existing declarations")))
    (unless (%namespaces element)
      (setf (%namespaces element) (make-hash-table :test 'equal)))
    (setf (gethash prefix (%namespaces element)) uri)
    uri))

(defun remove-extra-namespace (element prefix)
  (when (%namespaces element)
    (remhash (or prefix "") (%namespaces element))))

(defun collect-local-namespaces (element)
  ;; zzz ERH optimiert das noch fuer den fall nur eines ergebnisses
  (let ((result (if (%namespaces element)
		    (alexandria:copy-hash-table (%namespaces element))
		    (make-hash-table :test 'equal))))
    (setf (gethash (%namespace-prefix element) result)
	  (%namespace-uri element))
    (dolist (a (%attributes element))
      (setf (gethash (namespace-prefix a) result) (namespace-uri a)))
    result))

(defmethod serialize ((node element) handler)
  (let ((uri (%namespace-uri node))
	(local-name (%local-name node))
	(qname (qualified-name node))
	(attrs (mapcar (lambda (a)
			 (sax:make-attribute 
			  :namespace-uri (namespace-uri a)
			  :local-name (local-name a)
			  :qname (qualified-name a)
			  :value (value a)))
		       (%attributes node)))
	(element-parent
	 (when (typep (parent node) 'element)
	   (parent node))))
    (maphash (lambda (prefix uri)
	       (unless (equal prefix "xml")
		 (let ((upper (when element-parent
				(find-namespace prefix element-parent))))
		   (unless (or (equal upper uri)
			       (and (null upper) (zerop (length uri))))
		     (push (if (plusp (length prefix))
			       (sax:make-attribute 
				:namespace-uri "http://www.w3.org/2000/xmlns/"
				:local-name prefix
				:qname (concatenate 'string "xmlns:" prefix)
				:value uri)
			       (sax:make-attribute 
				:namespace-uri "http://www.w3.org/2000/xmlns/"
				:local-name "xmlns"
				:qname "xmlns"
				:value uri))
			   attrs)))))
	     (collect-local-namespaces node))
    (sax:start-element handler uri local-name qname attrs)
    (map nil (lambda (x) (serialize x handler)) (%children node))
    (sax:end-element handler uri local-name qname)))

(defmethod (setf base-uri) (newval (node element))
  (setf (%base-uri node) newval))

(defun escape-uri (string)
  (with-output-to-string (out)
    (loop for c across (cxml::rod-to-utf8-string string) do
	  (let ((code (char-code c)))
	    ;; http://www.w3.org/TR/xlink/#link-locators
	    (if (or (>= code 127) (<= code 32) (find c "<>\"{}|\\^`"))
		(format out "%~2,'0X" code)
		(write-char c out))))))

(defmethod base-uri ((node element))
  (let ((xml-base
	 (or (attribute-value-named node
				    "base"
				    "http://www.w3.org/XML/1998/namespace")
	     (%base-uri node)))
	(parent (parent node)))
    (if parent
	(puri:merge-uris xml-base (base-uri parent))
	xml-base)))

;;; below a literal translation of XOM's Java code for BASE-URI.
;;; Unfortunately I don't understand a word of what's going on here, hence
;;; the trivial definition above instead.

;;;(defmethod base-uri ((node element))
;;;  (let ((defaults "")
;;;	(relative-uri (%base-uri node)))
;;;    (loop
;;;       for n = node then (parent node)
;;;       while n
;;;       do
;;;	 (let ((%base-uri (%base-uri n)))
;;;	   (when (and (plusp (length relative-uri))
;;;		      (not (equals relative-uri %base-uri)))
;;;	     (return (merge-uris relative-uri defaults)))
;;;	   (when (typep n 'document)
;;;	     (return (merge-uris %base-uri defaults)))
;;;	   (let ((xml-base (attribute-value-named
;;;			    n
;;;			    "base"
;;;			    "http://www.w3.org/XML/1998/namespace")))
;;;	     (when xml-base
;;;	       (setf xml-base (escape-uri xml-base))
;;;	       (cond
;;;		 ((zerop (length xml-base))
;;;		   (setf defaults (get-entity-uri node)))
;;;		 (t
;;;		   (cond
;;;		     ((zerop (length defaults))
;;;		       (setf defaults xml-base))
;;;		     (...isopaque...
;;;		      (return defaults))
;;;		     (t
;;;		      (setf defaults (merge-uris xml-base defaults))))
;;;		   (when (isabsolute xml-base)
;;;		     (return defaults)))))))
;;;       finally				;parent is null
;;;	 (return (merge-uris %base-uri defaults)))))

(defmethod string-value ((node element))
  (with-output-to-string (s)
    (labels ((recurse (x)
	       (do-children (child x)
		 (typecase child
		   ((or comment processing-instruction))
		   (text (write-string (string-value child)))
		   (element (recurse child))))))
      (recurse node))))


;;; printing

(defmethod slots-for-print-object append ((node named-node-mixin))
  '((:local-name local-name)
    (:namespace-prefix namespace-prefix)
    (:namespace-uri namespace-uri)))

(defmethod slots-for-print-object append ((node element))
  '((:attributes %attributes)
    (:extra-namespaces namespaces-for-print)))

(defun namespaces-for-print (element)
  (when (%namespaces element)
    (loop
       for prefix being each hash-key in (%namespaces element)
       using (hash-value uri)
       collect `(,prefix ,uri))))

(defreader named-node-mixin (local-name namespace-prefix namespace-uri)
  (setf (%local-name this) local-name)
  (setf (%namespace-prefix this) namespace-prefix)
  (setf (%namespace-uri this) namespace-uri))

(defreader element (attributes extra-namespaces)
  (dolist (a attributes)
    (add-attribute this a))
  (loop for (prefix uri) on extra-namespaces do
       (add-extra-namespace this prefix uri)))
