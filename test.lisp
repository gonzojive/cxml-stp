(in-package :cxml-stp)

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
