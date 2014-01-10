;;;; braid.asd

(asdf:defsystem #:braid
  :version "0.0.1"
  :author "Rob Blackwell"
  :description "A trivial abstraction over HTTP request and response messages."
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "message")
	       (:file "status-code")))




