(defpackage :cxml-stp-system
  (:use :asdf :cl))
(in-package :cxml-stp-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :cxml-stp
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "node")
     (:file "parent-node")
     (:file "leaf-node")
     (:file "document")
     (:file "element")
     (:file "attribute")
     (:file "document-type")
     (:file "comment")
     (:file "processing-instruction")
     (:file "text"))
    :depends-on (:cxml :alexandria))
