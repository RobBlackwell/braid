braid
=====

Braid provides a trivial abstraction over HTTP request and response
messages so that they can be decoupled from specific web servers and
clients. This allows web applications to be modelled as functions that
take a request and return a response.

(defun my-handler (request)
	(make-response :body "Hello World"))
	
Adapters are used to hook up braid to real web servers - See
[braid-hunchentoot](https://github.com/RobBlackwell/braid-hunchentoot)
for example.

The default implementation of request and response objects is as
plists. This makes them easy to print out, debug and extend. Headers
are modelled as alists.

This is not a new idea; Clojure Ring, Ruby Rack and Python WSGI all
have a similar philosophy. Braid is designed for Common Lisp.

THIS IS EXPERIMENTAL CODE THAT IS SUBJECT TO CHANGE. I welcome
feedback, but it's probably too early to consider including in
Quicklisp yet. That doesnt stop you trying it with quicklisp by using
[local-projects](http://www.quicklisp.org/beta/faq.html).

Rob Blackwell
January 2014
