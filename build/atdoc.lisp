(asdf:operate 'asdf:load-op :cxml-stp)
(asdf:operate 'asdf:load-op :atdoc)
(let* ((base (asdf:component-pathname (asdf:find-system :cxml-stp)))
       (atdoc-directory (merge-pathnames "doc/" base)))
  (ensure-directories-exist atdoc-directory)
  (atdoc:generate-documentation '(:cxml-stp)
				atdoc-directory
				:index-title "cxml-stp API reference"
				:heading "cxml-stp"
				:css "cxml-stp.css"))
