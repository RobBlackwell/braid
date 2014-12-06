;;;; braid.asd

(asdf:defsystem #:braid
  :version "0.0.3"
  :author "Rob Blackwell"
  :description "A trivial abstraction over HTTP request and response messages."
  :serial t
  :depends-on (#:puri)
  :components ((:file "package")
			   (:file "braid")))





