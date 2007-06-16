(defpackage :cxml-stp
  (:use :cl)
  (:export )
  (:documentation
   "@code{cxml-stp} implements ___.
    Relax NG} schema validation for Closure XML.

    Support for @a[http://relaxng.org/compact-20021121.html]{Compact Syntax}
    and @a[http://relaxng.org/compatibility-20011203.html]{DTD Compatibility}
    is included.

    @begin[Example]{section}
    @begin{pre}(cxml:parse-file \"test.xml\"
                 (cxml-rng:make-validator
                  (cxml-rng:parse-schema #p\"test.rng\")))
    @end{pre}
    @end{section}
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
