;;;; status-code.lisp

(in-package #:braid)

(defvar *http-reason-phrase-map* (make-hash-table) 
  "Maps status codes to reason phrases.")

(defun status-code-reason-phrase (status-code)
  "Returns the reason code for a given status code. E.g. 200 -> OK"
  (gethash status-code *http-reason-phrase-map*))

(defmacro define-status-code (status-code reason)
  "Defines and exports an appropriately named constant for the status
code and memoizes the reason for use by status-code-reason-phrase."
  (let ((sym (intern (string-upcase (concatenate 'string "+http-" (substitute #\- #\Space (substitute #\Space #\- reason)) "+")))))
    `(progn 
       (define-constant ,sym ,status-code)
       (export (quote ,sym))
       (setf (gethash ,status-code *http-reason-phrase-map*) ,reason))))

;;; From RFC2616

(define-status-code 100 "Continue")
(define-status-code 101 "Switching Protocols")
(define-status-code 200 "OK")
(define-status-code 201 "Created")
(define-status-code 202 "Accepted")
(define-status-code 203 "Non-Authoritative Information")
(define-status-code 204 "No Content")
(define-status-code 205 "Reset Content")
(define-status-code 206 "Partial Content")
(define-status-code 300 "Multiple Choices")
(define-status-code 301 "Moved Permanently")
(define-status-code 302 "Found")
(define-status-code 303 "See Other")
(define-status-code 304 "Not Modified")
(define-status-code 305 "Use Proxy")
(define-status-code 307 "Temporary Redirect")
(define-status-code 400 "Bad Request")
(define-status-code 401 "Unauthorized")
(define-status-code 402 "Payment Required")
(define-status-code 403 "Forbidden")
(define-status-code 404 "Not Found")
(define-status-code 405 "Method Not Allowed")
(define-status-code 406 "Not Acceptable")
(define-status-code 407 "Proxy Authentication Required")
(define-status-code 408 "Request Time-out")
(define-status-code 409 "Conflict")
(define-status-code 410 "Gone")
(define-status-code 411 "Length Required")
(define-status-code 412 "Precondition Failed")
(define-status-code 413 "Request Entity Too Large")
(define-status-code 414 "Request-URI Too Large")
(define-status-code 415 "Unsupported Media Type")
(define-status-code 416 "Requested range not satisfiable")
(define-status-code 417 "Expectation Failed")
(define-status-code 500 "Internal Server Error")
(define-status-code 501 "Not Implemented")
(define-status-code 502 "Bad Gateway")
(define-status-code 503 "Service Unavailable")
(define-status-code 504 "Gateway Time-out")
(define-status-code 505 "HTTP Version not supported")


