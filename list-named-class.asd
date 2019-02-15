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

(asdf:defsystem #:list-named-class/test
  :description "Tests for LIST-NAMED-CLASS"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:list-named-class
               #:1am)
  :components ((:file "test")))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system ':list-named-class))))
  (asdf:load-system :list-named-class/test)
  (uiop:symbol-call :1am :run
                    (symbol-value (find-symbol "*LIST-NAMED-CLASS-TESTS*"
                                               :list-named-class/test))))
