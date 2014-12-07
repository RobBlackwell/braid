;;;; braid.lisp

(in-package #:braid)

;;; Define an interface for working with HTTP messages:

(defgeneric http-message-body (http-message) 
  (:documentation "Returns the body from an HTTP-MESSAGE."))

(defgeneric http-message-headers (http-message) 
  (:documentation "Returns the headers from an HTTP-MESSAGE."))

(defgeneric http-message-header (http-message header-name) 
  (:documentation "Returns the header named HEADER-NAME from from the HTTP-MESSAGE."))

;;; and a default implementation:

(defclass http-message ()
  ((headers :initarg :headers :accessor http-message-headers)
   (body :initarg :body :accessor http-message-body)))

;;; Use a property list to represent headers:

(defmethod http-message-header ((http-message http-message) header-name)
  (getf (http-message-headers http-message) header-name))

(defmethod (setf http-message-header) (new-value (http-message http-message) header-name)
  (setf (getf (http-message-headers http-message) header-name) new-value))

;;; Define an interface for working with HTTP requests.

(defgeneric http-request-uri (http-request) 
  (:documentation "Returns the uri from an HTTP request."))

(defgeneric http-request-method (http-request) 
  (:documentation "Returns the request method from an HTTP request. One of :get :put :post :head etc."))

;;; and a default implementation

(defclass http-request (http-message)
  ((uri :initarg :uri :accessor http-request-uri)
   (method :initarg :method :accessor http-request-method)))

(defun make-http-request (&key (uri nil) (method :get) (headers nil) (body nil))
  "Creates a new HTTP request."
  (make-instance 'http-request :method method :uri (puri:parse-uri uri) :headers headers :body body))

(defmethod print-object ((http-request http-request) stream)
  (print-unreadable-object (http-request stream :type t :identity t)
    (princ (http-request-uri http-request) stream)))

;;; Define an interface for working with HTTP responses

(defgeneric http-response-status (http-response) 
  (:documentation "Returns the status code from an HTTP response. E.g. 200 for OK."))

;;; and a default implementatation

(defclass http-response (http-message)
  ((status :initarg :status :accessor http-response-status)))

(defun make-http-response (&key (status 200) (headers nil) (body nil))
  "Creates a new HTTP response."
  (make-instance 'http-response :status status :headers headers :body body))

(defmethod print-object ((http-response http-response) stream)
  (print-unreadable-object (http-response stream :type t :identity t)
    (princ (http-response-status http-response) stream)))

;;; End
