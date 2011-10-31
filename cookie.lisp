
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

(in-package :toot)

(defclass cookie ()
  ((name :initarg :name :reader cookie-name :type string)
   (value :initarg :value :accessor cookie-value :initform "")
   (expires :initarg :expires :initform nil :accessor cookie-expires)
   (path :initarg :path :initform nil :accessor cookie-path)
   (domain :initarg :domain :initform nil :accessor cookie-domain)
   (secure :initarg :secure :initform nil :accessor cookie-secure)
   (http-only :initarg :http-only :initform nil :accessor cookie-http-only)))

(defmethod initialize-instance :around ((cookie cookie) &rest init-args)
  "Ensure COOKIE has a correct slot-value for NAME."
  (let ((name (getf init-args :name)))
    (unless (http-token-p name)
      (parameter-error "~S is not a legal name for a cookie." name)))
  (call-next-method))

(defun set-cookie (name request &key (value "") expires path domain secure http-only)
  (let ((place (assoc name (cookies-out request) :test #'string=))
        (cookie (make-instance 'cookie
                   :name name
                   :value value
                   :expires expires
                   :path path
                   :domain domain
                   :secure secure
                   :http-only http-only)))
    (cond
      (place (setf (cdr place) cookie))
      (t (push (cons name cookie) (cookies-out request))))
    cookie))

(defun cookie-date (universal-time)
  (and universal-time (rfc-1123-date universal-time)))

(defun stringify-cookie (cookie)
  (format nil
          "~A=~A~:[~;~:*; expires=~A~]~:[~;~:*; path=~A~]~:[~;~:*; domain=~A~]~:[~;; secure~]~:[~;; HttpOnly~]"
          (cookie-name cookie)
          (url-encode (format nil "~A" (cookie-value cookie)) +utf-8+)
          (cookie-date (cookie-expires cookie))
          (cookie-path cookie)
          (cookie-domain cookie)
          (cookie-secure cookie)
          (cookie-http-only cookie)))