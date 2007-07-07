(defpackage :cxml-stp-test
  (:use :cl :rt :stp))

(in-package :cxml-stp-test)

(defmethod xmlconf::serialize-document ((document node))
  (let ((cxml-stp::*serialize-canonical-notations-only-p* t))
    (serialize document (cxml:make-octet-vector-sink :canonical 2))))

(defun stp-test (filename handler &rest args)
  (declare (ignore handler))
  (apply #'cxml:parse-file
	 filename
	 (read-from-string "#.(cxml-stp:make-builder)")
	 :recode t
	 args))


#+(or)
(let ((cxml-stp::*serialize-canonical-notations-only-p* t))
  (xmlconf::run-all-tests 'xmlconf::stp-test
			  "/home/david/2001/XML-Test-Suite/xmlconf/"))


(deftest text.constructor
    (data (make-text "test"))
  "test")

(defun assert-equal (a b)
  (unless (equal a b)
    (error "assertion failed: ~S and ~S are not EQUAL" a b)))

(defmacro expect-exception (form type)
  `(handler-case
       (progn
	 ,form
	 (error "expected a condition of type ~A" ',type))
     (,type ())))

(defmacro define-exception-test (name form type)
  `(deftest ,name
       (progn
	 (expect-exception ,form ,type)
	 (values))))

(deftest text.legal
    (let ((text (make-text "name")))
      (dolist (str '("Hello"
		     "hello there"
		     "  spaces on both ends  "
		     " quotes \" \" quotes"
		     " single \'\' quotes"
		     " both double and single \"\'\"\' quotes"
		     " angle brackets <  > <<<"
		     #.(format nil " carriage returns ~C~C"
			(code-char 13) (code-char 13))
		     #.(format nil " newlines ~C~C"
			(code-char 10) (code-char 10))
		     #.(format nil " both ~C~C"
			(code-char 13) (code-char 10))
		     #.(format nil " tab ~C foo"
			(code-char 9))
		     " CDATA end: ]]>"
		     " <![CDATA[ CDATA end: ]]>"
		     " &amp; "
		     " ampersands & &&& &name; "))
	(setf (data text) str)
	(assert-equal (data text) str)
	(assert-equal (string-value text) str))
      (values)))

(deftest text.nil
    (let ((text (make-text "name")))
      (setf (data text) nil)
      (data text))
  "")

(define-exception-test text.illegal
    (let ((text (make-text "name")))
      (setf (data text) (format nil "test ~C test" (code-char 0))))
  stp-error)

(deftest text.serialize
    (let ((text (make-text "name"))
	  (pairs '("Hello"
		   "hello there"
		   "  spaces on both ends  "
		   ;; zzz CXML traditionally escapes quotes without good
		   ;; reason:
		   (" quotes \" \" quotes"
		    " quotes &quot; &quot; quotes")
		   (" both double and single \"\'\"\' quotes"
		    " both double and single &quot;\'&quot;\' quotes")
		   " single \'\' quotes"
		   ("<>" "&lt;&gt;")
		   ("&amp;" "&amp;amp;")
		   ("]]>" "]]&gt;")
		   (#.(string (code-char 13)) "&#13;")
		   "=,.!@#$%^*()_-'[]{}+/?;:`|\\")))
      (loop
	 for (in out) in (mapcar (lambda (x) (if (listp x) x (list x x)))
				 pairs)
	 do
	   (setf (data text) in)
	   (let ((sink (cxml:make-string-sink)))
	     (serialize text sink)
	     (assert-equal (sax:end-document sink) out)))
      (values)))

(deftest text.copy
    (let* ((c1 (make-text "test"))
	   (c2 (copy c1)))
      (assert-equal (data c1) (data c2))
      (assert-equal nil (parent c2))
      (assert-equal (type-of c2) 'text)
      (values)))

;;; zzz surrogate testing is going to be a mess, because cxml will have to
;;; support both Lisps with 16 bit and with 21 bit characters.  Not the
;;; mention all the surrogate-related bugs we've got.
;;;   - testSurrogates
;;;   - testNonBMPText
;;;   - testEndOfBMP
;;;   - testHighSurrogateWithNoLowSurrogate

(deftest text.leaf-node
    (let ((c1 (make-text "data")))
      (assert-equal 0 (count-children-if #'identity c1))
      (expect-exception (nth-child 0 c1) error)
      (assert-equal nil (parent c1))
      (let ((e (make-element "test")))
	(append-child e c1)
	(assert-equal e (parent c1))
	(assert-equal c1 (nth-child 0 e))
	(delete-child c1 e)
	(assert-equal 0 (count-children-if #'identity e)))
      (values)))

(do-tests)
