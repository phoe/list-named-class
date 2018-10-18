;;;; list-named-class.asd

(asdf:defsystem #:list-named-class
  :description "CLOS extension - name classes after lists of symbols"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop)
  :components ((:file "list-named-class")
               (:file "swank")))

(asdf:defsystem #:list-named-class/protest
  :description "LIST-NAMED-CLASS with PROTEST integration"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:list-named-class
               #:protest/base)
  :components ((:file "protest")))
