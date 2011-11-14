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

(defclass cookie ()
  ((name      :initarg :name                    :reader name :type string)
   (value     :initarg :value     :initform ""  :accessor value)
   (expires   :initarg :expires   :initform nil :accessor expires)
   (path      :initarg :path      :initform nil :accessor path)
   (domain    :initarg :domain    :initform nil :accessor domain)
   (secure    :initarg :secure    :initform nil :accessor secure)
   (http-only :initarg :http-only :initform nil :accessor http-only)))

(defmethod initialize-instance :around ((cookie cookie) &key name &allow-other-keys)
  "Ensure COOKIE has a correct slot-value for NAME."
  (unless (http-token-p name)
    (parameter-error "~S is not a legal name for a cookie." name))
  (call-next-method))

(defun stringify-cookie (cookie)
  (with-slots (name value expires path domain secure http-only) cookie
    (format
     nil
     "~a=~a~@[; expires=~a~]~@[; path=~a~]~@[; domain=~a~]~@[~*; secure~]~@[~*; HttpOnly~]"
     name
     (url-encode (princ-to-string value) +utf-8+)
     (and expires (rfc-1123-date expires))
     path
     domain
     secure
     http-only)))

(defun http-token-p (token)
  "Tests whether TOKEN is a string which is a valid 'token'
according to HTTP/1.1 \(RFC 2068)."
  (and (stringp token)
       (plusp (length token))
       (every (lambda (char)
                (and ;; CHAR is US-ASCII but not control character or ESC
                     (< 31 (char-code char) 127)
                     ;; CHAR is not 'tspecial'
                     (not (find char "()<>@,;:\\\"/[]?={} " :test #'char=))))
              token)))