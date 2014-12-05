;;;; braid.asd

(asdf:defsystem #:braid
  :version "0.0.2
"
  :author "Rob Blackwell"
  :description "A trivial abstraction over HTTP request and response messages."
  :serial t
	:depends-on (#:alexandria
							 #:puri)
  :components ((:file "package")
							 (:file "braid")))





