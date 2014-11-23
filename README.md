# Braid

## Introduction

Braid provides a trivial abstraction over HTTP request and response
messages so that they can be decoupled from specific web frameworks,
servers and clients.

This allows web applications to be modelled as simple request handler
functions that take a request and return a response.

## Concepts

Requests have a request-method, uri, headers and a body. The default
implementation is as a plist, which makes them easy to eyeball thus:

	> (braid:make-request :uri "http://www.google.co.uk")
	(:REQUEST-METHOD :GET :URI "http://www.google.co.uk" :HEADERS NIL :BODY NIL)

Accessors are provided for each of the elements.

	> (braid:request-method (braid:make-request :uri "http://www.google.co.uk"))
	:GET

Responses are modelled similarly with status, headers and body.

	> (braid:make-response :body "hello world")
	(:STATUS 200 :HEADERS NIL :BODY "hello world")

Request handlers are functions that take one argument, a request, and
return a response (or NIL signifying that they haven't handled the
request).

    (defun my-request-handler (request)
    	(make-response :body "Hello World"))

This simple abstraction can then be extended with the concept of
[braid-middleware](https://github.com/RobBlackwell/braid-middleware)

Adapters are used to hook up braid to real web servers - See
[braid-hunchentoot](https://github.com/RobBlackwell/braid-hunchentoot)
for example.

Braid can also be used with web clients - See
[braid-drakma](https://github.com/RobBlackwell/braid-drakma)

Functionality can be layered with middleware. E.g. see
[braid-yason](https://github.com/RobBlackwell/braid-yason) for JSON
support.

This is not a new idea; Clojure Ring, Ruby Rack and Python WSGI all
have a similar philosophy. Braid is designed for Common Lisp.

THIS IS EXPERIMENTAL CODE THAT IS SUBJECT TO CHANGE. I welcome
feedback, but it's probably too early to consider including in
Quicklisp yet. That doesnt stop you trying it with quicklisp by using
[local-projects](http://www.quicklisp.org/beta/faq.html).

Rob Blackwell    
November 2014
