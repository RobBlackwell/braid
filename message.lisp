;;;; message.lisp

(in-package #:braid)

;;; Define an interface for working with HTTP messages,

(defgeneric message-headers (message) 
  (:documentation "Returns the headers from an HTTP message."))

(defgeneric (setf message-headers) (new-value message) 
  (:documentation "Sets the value of the HTTP message headers."))

(defgeneric message-header (message header-name) 
  (:documentation "Returns the value of the HTTP message header with the given name."))

(defgeneric (setf message-header) (header-value header header-name) 
  (:documentation "Sets the value of the HTTP message header with the given name."))

(defgeneric message-body (message) 
  (:documentation "Returns the body from an HTTP message."))

(defgeneric (setf message-body) (new-value message) 
  (:documentation "Sets the body from an HTTP message."))

;;; and a default implementation based on a plist.

(defmethod message-headers ((message cons))
  (getf message :headers))

(defmethod (setf message-headers) (new-value (message cons))
  (setf (getf message :headers) new-value))

(defmethod message-header ((message t) header-name)
  (rest (assoc header-name (message-headers message) :test #'string=)))

(defmethod (setf message-header) (header-value (message t) header-name)
  (setf (getf message :headers) (acons header-name header-value (getf message :headers))))

(defmethod message-body ((message cons))
  (getf message :body))

(defmethod (setf message-body) (new-value (message cons))
  (setf (getf message :body) new-value))

;;; HTTP requests extend messages to include a method and a URI.

(defgeneric request-method (request) 
  (:documentation "Returns the request method from an HTTP request. One of :get :put :post :head etc."))

(defgeneric (setf request-method) (new-value request) 
  (:documentation "Sets the request method of an HTTP request."))

(defgeneric request-uri (request) 
  (:documentation "Returns the uri from an HTTP request."))

(defgeneric (setf request-uri) (new-value request) 
  (:documentation "Sets the uri of an HTTP request."))

(defmethod request-method ((request cons))
  (getf request :method))

(defmethod (setf request-method) (new-value (request cons))
  (setf (getf request :method) new-value))

(defmethod request-uri ((request cons))
  (getf request :uri))

(defmethod (setf request-uri) (new-value (request cons))
  (setf (getf request :uri) new-value))

;;; HTTP responses extend messages with a status code.

(defgeneric response-status-code (response) 
  (:documentation "Returns the status code from an HTTP response. E.g. 200 for OK."))

(defgeneric (setf response-status-code) (new-value response) 
  (:documentation "Sets the status code of an HTTP response."))

(defmethod response-status-code ((response cons))
  (getf response :status-code))

(defmethod (setf response-status-code) (new-value (request cons))
  (setf (getf request :status-code) new-value))

;;;

(defun ensure-body (body)
	"A body must either be a string, array or nil."
	(when body
		(typecase body
			(string body)
			(array body)
			(t (format nil "~a" body)))))

;;; Constructors:

(defun make-request (uri &key (method :get) (headers nil) (body nil))
  "Creates a new HTTP request."
  (list :method method :uri uri :headers headers :body body))

(defun make-response (&key (status-code +http-ok+) (headers nil) (body nil))
  "Creates a new HTTP response."
  (list :status-code status-code :headers headers :body (ensure-body body)))
