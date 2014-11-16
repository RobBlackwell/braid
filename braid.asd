;;;; braid.asd

(asdf:defsystem #:braid
  :version "0.0.1"
  :author "Rob Blackwell"
  :description "A trivial abstraction over HTTP request and response messages."
  :serial t
  :components ((:file "package")
							 (:file "braid")))





