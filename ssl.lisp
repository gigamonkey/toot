
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :hunchentoot)

(defclass ssl-acceptor (acceptor)
  ((ssl-certificate-file :initarg :ssl-certificate-file :reader acceptor-ssl-certificate-file)
   (ssl-privatekey-file :initarg :ssl-privatekey-file :reader acceptor-ssl-privatekey-file)
   (ssl-privatekey-password :initform nil :initarg :ssl-privatekey-password :reader acceptor-ssl-privatekey-password))
  (:default-initargs :port 443))

(defmethod acceptor-ssl-p ((acceptor ssl-acceptor)) t)

(defmethod initialize-instance :after ((acceptor ssl-acceptor) &rest initargs)
  (declare (ignore initargs))
  ;; OpenSSL doesn't know much about Lisp pathnames...
  (setf (slot-value acceptor 'ssl-privatekey-file)
        (namestring (truename (acceptor-ssl-privatekey-file acceptor)))
        (slot-value acceptor 'ssl-certificate-file)
        (namestring (truename (acceptor-ssl-certificate-file acceptor)))))

(defmethod initialize-connection-stream ((acceptor ssl-acceptor) stream)
  ;; attach SSL to the stream if necessary
  (call-next-method 
   acceptor
   (cl+ssl:make-ssl-server-stream 
    stream
    :certificate (acceptor-ssl-certificate-file acceptor)
    :key (acceptor-ssl-privatekey-file acceptor)
    :password (acceptor-ssl-privatekey-password acceptor))))


