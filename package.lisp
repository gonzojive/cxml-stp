(defpackage :cxml-stp
  (:use :cl)
  (:export #:*check-uri-syntax*

	   #:node
	   #:parent
	   #:root
	   #:base-uri
	   #:detach
	   #:string-value
	   #:copy
	   #:serialize
	   ;; #:query
	   #:map-children
	   #:do-children
	   #:list-children
	   #:nth-child
	   #:first-child
	   #:first-child
	   #:find-child
	   #:find-child-if
	   #:child-position
	   #:child-position-if
	   #:filter-children

	   #:document
	   #:make-document
	   #:document-element

	   #:parent-node
	   #:prepend-child
	   #:append-child
	   #:delete-child
	   #:delete-nth-child
	   #:insert-child
	   #:insert-child-before
	   #:insert-child-after
	   #:delete-child-if
	   #:replace-children
	   #:replace-child

	   ;; #:named-node
	   #:local-name
	   #:namespace-prefix
	   #:namespace-uri
	   #:of-name
	   #:qualified-name

	   #:element
	   #:make-element
	   #:add-attribute
	   #:remove-attribute
	   #:find-attribute-named
	   #:find-attribute-if
	   #:attribute-value-named
	   #:attribute-value-if
	   #:list-attributes
	   #:map-attributes
	   #:find-namespace
	   #:find-local-namespace
	   #:find-extra-namespace
	   #:add-extra-namespace
	   #:remove-extra-namespace

	   #:attribute
	   #:make-attribute
	   #:attribute-value
	   #:rename-attribute

	   #:comment
	   #:data
	   
	   #:processing-instruction
	   #:target

	   #:document-type
	   #:system-id
	   #:public-id
	   #:internal-subset

	   #:text)
  (:documentation
   "STP is a data structure for well-formed XML documents, designed for
    Common Lisp.  Inspired by XOM, it provides an alternative to the W3C's
    DOM.

    @begin[Classes]{section}
    @aboutclass{schema}
    @aboutclass{rng-error}
    @aboutclass{dtd-compatibility-error}
    @end{section}
    @begin[Parsing and validating]{section}
    @aboutfun{parse-schema}
    @aboutfun{parse-compact}
    @aboutfun{make-validator}
    @aboutfun{make-dtd-compatibility-handler}
    @aboutfun{serialize-grammar}
    @end{section}
    @begin[Grammar introspection]{section}
    The following classes and function are exported so that users can
    take a peek at the internals of the parsed and simplified grammar.

    @aboutfun{schema-start}
    @aboutclass{attribute}
    @aboutclass{choice}
    @aboutclass{data}
    @aboutclass{element}
    @aboutclass{empty}
    @aboutclass{group}
    @aboutclass{interleave}
    @aboutclass{list-pattern}
    @aboutclass{not-allowed}
    @aboutclass{one-or-more}
    @aboutclass{pattern}
    @aboutclass{ref}
    @aboutclass{text}
    @aboutclass{value}
    @aboutfun{pattern-child}
    @aboutfun{pattern-a}
    @aboutfun{pattern-b}
    @aboutfun{pattern-name}
    @aboutfun{pattern-element}
    @aboutfun{pattern-type}
    @aboutfun{pattern-string}
    @aboutfun{pattern-value}
    @aboutfun{pattern-params}
    @aboutfun{pattern-except}
    @end{section}"))
