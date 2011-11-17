;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2011, Peter Seibel. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :toot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging API -- the server logs HTTP requests and miscelaneous
;;; messages using these two generic functions, called on logger
;;; objects held by the server.

(defgeneric log-access (logger request)
  (:documentation "Write a log entry for the request to the access log."))

(defgeneric log-message (logger log-level format-string &rest format-arguments)
  (:documentation "Write a log entry to the message log."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Null logger

(defmethod log-access ((logger null) request)
  (declare (ignore request)))

(defmethod log-message ((logger null) log-level format-string &rest format-arguments)
  (declare (ignore log-level format-string format-arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple logger for logging to an open character stream.

(defclass stream-logger ()
  ((destination :initarg :destination :reader destination)
   (lock :initform (make-lock "log-lock") :reader lock))
  (:documentation "A logger that writes to a given stream."))

(defvar *default-logger* (make-instance 'stream-logger :destination *error-output*))

(defmethod log-access ((logger stream-logger) request)
  (let ((out (destination logger)))
    (with-lock-held ((lock logger))
      (format out "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
              (remote-addr request)
              (request-header :x-forwarded-for request)
              (authorization request)
              (iso-time)
              (request-method request)
              (request-uri request)
              (server-protocol request)
              (status-code request)
              (content-length request)
              (request-header :referer request)
              (request-header :user-agent request))
      (finish-output out))))

(defmethod log-message ((logger stream-logger) log-level format-string &rest format-arguments)
  (let ((out (destination logger)))
    (with-lock-held ((lock logger))
      (format out "[~A~@[ [~A]~]] ~?~%" (iso-time) log-level format-string format-arguments)
      (finish-output out))))
