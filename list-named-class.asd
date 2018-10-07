;;;; list-named-class.asd

(asdf:defsystem #:list-named-class
  :description "CLOS extension - name classes after lists of symbols"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop)
  :components ((:file "list-named-class")))
