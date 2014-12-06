# Braid

## Introduction

Braid provides a trivial abstraction over HTTP request and response
messages so that they can be decoupled from specific web frameworks,
servers and clients.

This allows web applications to be modelled as simple request handler
functions that take a request and return a response.

## Concepts

Requests have a method, uri, headers and a body and can be created
like this:

	> (make-http-request :uri "http://www.google.co.uk")
    #<HTTP-REQUEST http://www.google.co.uk/ #x21021E393D>

Accessors are provided for each of the elements.

    > (http-request-method (make-http-request :uri "http://www.google.co.uk"))
    :GET

Responses are modelled similarly with status, headers and body.

    > (make-http-response :body "Hello world")
    #<HTTP-RESPONSE 200 #x21021E727D>

Request handlers are functions that take one argument, an
http-request, and return an http-response (or NIL signifying that they
haven't handled the request).

    (defun my-http-request-handler (http-request)
    	(make-http-response :body "Hello World"))

Adapters are used to hook up braid to real web servers - See
[braid-hunchentoot](https://github.com/RobBlackwell/braid-hunchentoot)
for example.

This simple abstraction can then be extended, see
[braid-util](https://github.com/RobBlackwell/braid-util) and
[braid-middleware](https://github.com/RobBlackwell/braid-middleware)

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
December 2014
