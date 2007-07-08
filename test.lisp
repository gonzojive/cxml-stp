;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 David Lichteblau. All rights reserved.
;;; (but mostly transcribed from nu/xom/tests/*)

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


(defun assert-equal (a b)
  (unless (equal a b)
    (error "assertion failed: ~S and ~S are not EQUAL" a b)))

(defmacro expect-exception (form type)
  `(handler-case
       (progn
	 ,form
	 (error "expected a condition of type ~A" ',type))
     (,type ())))

(defun serialize-to-string (node)
  (let ((sink (cxml:make-string-sink)))
    (serialize node sink)
    (sax:end-document sink)))

(defmacro define-exception-test (name form type)
  `(deftest ,name
       (progn
	 (expect-exception ,form ,type)
	 (values))))

(rem-all-tests)


;;;; TEXT

(deftest text.constructor
    (data (make-text "test"))
  "test")

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
	   (assert-equal (serialize-to-string text) out))
      (values)))

(deftest text.copy
    (let* ((c1 (make-text "test"))
	   (c2 (copy c1)))
      (assert (not (eq c1 c2)))
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


;;;; COMMENT

(deftest comment.constructor
    (data (make-comment "test"))
  "test")

(deftest comment.constructor2
    (data (make-comment ""))
  "")

(deftest comment.constructor3
    (data (make-comment "- - "))
  "- - ")

(deftest comment.copy
    (let* ((c1 (make-comment "test"))
	   (c2 (copy c1)))
      (assert (not (eq c1 c2)))
      (assert-equal (data c1) (data c2))
      (assert-equal nil (parent c2))
      (assert-equal (type-of c2) 'comment)
      (values)))

(deftest comment.serialize
    (let ((c (make-comment "0123456789012345678901234567890123456789")))
      (assert-equal (serialize-to-string c)
		    "<!--0123456789012345678901234567890123456789-->")
      (values)))

;;; zzz das pruefen wir nicht
;; (define-exception-test comment.cr
;;     (make-comment (format nil "foo ~C bar" (code-char 13)))
;;   stp-error)

(deftest comment.setf
    (let ((c (make-comment "test")))
      (setf (data c) "legal")
      (assert-equal (data c) "legal")
      (assert-equal (string-value c) "legal")
      (expect-exception (setf (data c) "test -- test") stp-error)
      (expect-exception (setf (data c) "test-") stp-error)
      (setf (data c) nil)
      (assert-equal (data c) "")
      (values)))

;;; zzz
;;;   - testSurrogates
;;;   - testForbidUnmatchedSurrogatesInComments

(deftest comment.leaf-node
    (let ((c1 (make-comment "data")))
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

(deftest comment.document
    (let ((c1 (make-comment "data"))
	  (root (make-element "root")))
      (assert-equal nil (document c1))
      (append-child root c1)
      (assert-equal nil (document c1))
      (let ((document (make-document root)))
	(assert-equal document (document c1)))
      (values)))

(deftest comment.funny-characters-allowed
    (assert-equal (serialize-to-string (make-comment "<test>&amp;&greater;"))
		  "<!--<test>&amp;&greater;-->")
  nil)

(define-exception-test comment.only-char-allowed
    (make-comment (format nil " ~C " (code-char 1)))
  stp-error)


(do-tests)
