;;;; braid.lisp

(in-package #:braid)

;;; Define an interface for working with HTTP messages.

(defgeneric headers (message) 
  (:documentation "Returns the headers from an HTTP message."))

(defgeneric (setf headers) (new-value message) 
  (:documentation "Sets the value of the HTTP message headers."))

(defgeneric header (message header-name) 
  (:documentation "Returns the value of the HTTP message header with the given name."))

(defgeneric (setf header) (new-value message header-name) 
  (:documentation "Sets the value of the HTTP message header with the given name."))

(defgeneric body (message) 
  (:documentation "Returns the body from an HTTP message."))

(defgeneric (setf body) (new-value message) 
  (:documentation "Sets the body of an HTTP message."))

(defgeneric copy-message (message)
	(:documentation "Returns a copy of an HTTP message with a copy of the headers."))

;;; and a default implementation based on a plist.
;;; e.g. (:headers (:content-type "text/plain") :body "Hello world")

(defmethod headers ((message cons))
  (getf message :headers))

(defmethod (setf headers) (new-value (message cons))
  (setf (getf message :headers) new-value))

(defmethod header ((message t) header-name)
	(getf (headers message) header-name))

(defmethod (setf header) (new-value (message t) header-name)
	(setf (getf (headers message) header-name) new-value))

(defmethod body ((message cons))
  (getf message :body))

(defmethod (setf body) (new-value (message cons))
  (setf (getf message :body) new-value))

(defmethod copy-message ((message cons))
  (copy-tree message))

;;; HTTP requests extend messages to include a method and a URI.

(defgeneric request-method (request) 
  (:documentation "Returns the request method from an HTTP request. One of :get :put :post :head etc."))

(defgeneric (setf request-method) (new-value request) 
  (:documentation "Sets the request method of an HTTP request."))

(defgeneric uri (request) 
  (:documentation "Returns the uri from an HTTP request."))

(defgeneric (setf uri) (new-value request) 
  (:documentation "Sets the uri of an HTTP request."))

;;; e.g. (:request-method :get :uri "/" :headers (:accept "text/plain") :body nil)

(defmethod request-method ((request cons))
  (getf request :request-method))

(defmethod (setf request-method) (new-value (request cons))
  (setf (getf request :request-method) new-value))

(defmethod uri ((request cons))
  (getf request :uri))

(defmethod (setf uri) (new-value (request cons))
  (setf (getf request :uri) new-value))

;;; HTTP responses extend messages with a status code.

(defgeneric status (response) 
  (:documentation "Returns the status code from an HTTP response. E.g. 200 for OK."))

(defgeneric (setf status) (new-value response) 
  (:documentation "Sets the status code of an HTTP response."))

;;; e.g. (:status 200 :headers (:content-type "text/plain") :body "Hello world")

(defmethod status ((response cons))
  (getf response :status))

(defmethod (setf status) (new-value (request cons))
  (setf (getf request :status) new-value))

;;; Constructors:

(defun make-request (&key (uri nil) (request-method :get) (headers nil) (body nil))
  "Creates a new HTTP request."
  (list :request-method request-method :uri uri :headers headers :body body))

(defun make-response (&key (status 200) (headers nil) (body nil))
  "Creates a new HTTP response."
  (list :status status :headers headers :body body))

;;; Common error responses

(defun make-not-found-response (&key (headers nil) (body "Not found"))
	"Returns a 404 not found response."
	(braid:make-response :status 404 :headers headers :body body))

(defun make-not-found-handler (&key (headers nil) (body "Not found"))
	"Returns a handler that always returns a 404 not found response."
	(lambda (request)
		(make-not-found-response :headers headers :body body)))

(defun make-internal-server-error-response (&key (headers nil) (body "Internal Server Error"))
	"Returns a 500 internal server error response."
	(braid:make-response :status 500 :headers headers :body body))

(defun make-internal-server-error-handler (&key (headers nil) (body "Not found"))
	"Returns a handler that always returns a 500 internal server error response."
	(lambda (request)
		(make-internal-server-error-response :headers headers :body body)))

;;; End
