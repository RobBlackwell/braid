;;;; braid.lisp

(in-package #:braid)

;;; Define an interface for working with HTTP requests.

(defgeneric http-request-uri (http-request) 
  (:documentation "Returns the uri from an HTTP request."))

(defgeneric http-request-method (http-request) 
  (:documentation "Returns the request method from an HTTP request. One of :get :put :post :head etc."))

(defgeneric http-request-headers (http-request) 
  (:documentation "Returns the headers from an HTTP request."))

(defgeneric http-request-header (http-request header-name) 
  (:documentation "Returns the header named HEADER-NAME from from the HTTP REQUEST."))

(defgeneric (setf http-request-header) (new-value http-request header-name) 
  (:documentation "Sets the value of the HTTP request header with the given name."))

(defgeneric http-request-body (http-request) 
  (:documentation "Returns the body from an HTTP request."))

;;; and a default implementation

(defclass http-request ()
  ((uri :initarg :uri :accessor http-request-uri)
   (method :initarg :method :accessor http-request-method)
   (headers :initarg :headers :accessor http-request-headers)
   (body :initarg :body :accessor http-request-body)))

(defun make-http-request (&key (uri nil) (method :get) (headers nil) (body nil))
  "Creates a new HTTP request."
  (make-instance 'http-request :method method :uri (puri:parse-uri uri) :headers headers :body body))

(defmethod http-request-header ((http-request http-request) header-name)
  (getf (http-request-headers http-request) header-name))

(defmethod (setf http-request-header) (new-value (http-request http-request) header-name)
  (setf (getf (http-request-headers http-request) header-name) new-value))

(defmethod print-object ((http-request http-request) stream)
  (print-unreadable-object (http-request stream :type t :identity t)
    (princ (http-request-uri http-request) stream)))

;;; Define an interface for working with HTTP responses

(defgeneric http-response-headers (http-response) 
  (:documentation "Returns the headers from an HTTP response."))

(defgeneric http-response-header (http-response header-name) 
  (:documentation "Returns the header named HEADER-NAME from from the HTTP RESPONSE."))

(defgeneric (setf http-response-header) (new-value http-response header-name) 
  (:documentation "Sets the value of the HTTP response header with the given name."))

(defgeneric http-response-body (http-response) 
  (:documentation "Returns the body from an HTTP response."))

(defgeneric http-response-status (http-response) 
  (:documentation "Returns the status code from an HTTP response. E.g. 200 for OK."))

;;; and a default implementatation

(defclass http-response ()
  ((status :initarg :status :accessor http-response-status)
   (headers :initarg :headers :accessor http-response-headers)
   (body :initarg :body :accessor http-response-body)))

(defun make-http-response (&key (status 200) (headers nil) (body nil))
  "Creates a new HTTP response."
  (make-instance 'http-response :status status :headers headers :body body))

(defmethod http-response-header ((http-response http-response) header-name)
  (getf (http-response-headers http-response) header-name))

(defmethod (setf http-response-header) (new-value (http-response http-response) header-name)
  (setf (getf (http-response-headers http-response) header-name) new-value))

(defmethod print-object ((http-response http-response) stream)
  (print-unreadable-object (http-response stream :type t :identity t)
    (princ (http-response-status http-response) stream)))

;;; End
