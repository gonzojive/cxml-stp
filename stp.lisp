(in-package :cxml-stp)

(defclass node ()
  (;; (document :reader document)
   ;; (base-uri :accessor base-uri)
   (parent :accessor parent)))

(defun document (node)
  (check-type node node)
  (loop
     for parent = this then (parent parent)
     while (and parent (not (typep parent 'document)))
     finally (return parent)))

(defun document (node)
  (check-type node node)
  (loop
     for p = parent then (parent p)
     and q = p
     while p
     finally (return q)))

(defgeneric base-uri (node)) ;fixme: hier muessen wir wissen, ob specified
(defmethod base-uri ((node node))
  (let ((parent (parent node)))
    (if parent
	(base-uri parent)
	"")))

(defmethod detach ((node node))
  (when (parent node)
    (delete-child node (parent node))))

(defgeneric string-value (node))
(defgeneric children (node))
(defgeneric copy (node))
(defgeneric unparse (node handler))

(defgeneric node= (node))
(defmethod node= ((node node) x)
  (eq node x))

(defgeneric (setf base-uri) (newval node)) ;s.o.


;; print-object nicht vergessen

(defun query (node xpath)
  ;; fixme
  )




(defgeneric make-document ((root element))
  ;; fixme
  )

(defgeneric make-document ((document document))
  ;; wtf?
  )

(defclass document (parent-node)
  )

(defun document-type (document)
  )

(defun root-element (document)
  )

(defun (setf root-element) (newval document)
  (check-type element newval)
  )

(defun check-root (document)
  ;; was macht das?
  )

(defclass parent-node (node)
  ((children :reader children)))

(defun (setf childen) (newval node)
  (replace-children node newval))

(defun replace-children (node new-children &key start1 end1 start2 end2)
  )

(defun insert-child (child parent position)
  )

(defun prepend-child (child parent)
  )

(defun append-child (child parent)
  )

(defun child-position (child parent &key start end test key from-end)
  )

(defun delete-child (child document &key start end count test from-end)
  )

(defun delete-child-if (predicate document &key start end count key from-end)
  )




(defclass attribute ...)

(defmethod detach ((node attribute))
  (when (parent node)
    (delete-attribute node (parent node))))
