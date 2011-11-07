;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

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

(in-package :cl-user)

(defpackage :toot
  (:use :cl :cl-ppcre :chunga :flexi-streams :bordeaux-threads :alexandria)
  (:shadow :defconstant)
  ;; see asdf system definition
  (:import-from :toot-asd :*toot-version*)
  (:export :*catch-errors-p*
           :*default-connection-timeout*
           :*default-content-type*
           :*file-upload-hook*
           :*header-stream*
           :*default-external-format*
           :*lisp-errors-log-level*
           :*lisp-warnings-log-level*
           :*log-lisp-backtraces-p*
           :*log-lisp-errors-p*
           :*log-lisp-warnings-p*
           :*methods-for-post-parameters*
           :*show-lisp-backtraces-p*
           :*show-lisp-errors-p*
           :*tmp-directory*

           ;; HTTP status codes
           :+http-accepted+
           :+http-authorization-required+
           :+http-bad-gateway+
           :+http-bad-request+
           :+http-conflict+
           :+http-continue+
           :+http-created+
           :+http-expectation-failed+
           :+http-failed-dependency+
           :+http-forbidden+
           :+http-gateway-time-out+
           :+http-gone+
           :+http-internal-server-error+
           :+http-length-required+
           :+http-method-not-allowed+
           :+http-moved-permanently+
           :+http-moved-temporarily+
           :+http-multi-status+
           :+http-multiple-choices+
           :+http-no-content+
           :+http-non-authoritative-information+
           :+http-not-acceptable+
           :+http-not-found+
           :+http-not-implemented+
           :+http-not-modified+
           :+http-ok+
           :+http-partial-content+
           :+http-payment-required+
           :+http-precondition-failed+
           :+http-proxy-authentication-required+
           :+http-request-entity-too-large+
           :+http-request-time-out+
           :+http-request-uri-too-large+
           :+http-requested-range-not-satisfiable+
           :+http-reset-content+
           :+http-see-other+
           :+http-service-unavailable+
           :+http-switching-protocols+
           :+http-temporary-redirect+
           :+http-unsupported-media-type+
           :+http-use-proxy+
           :+http-version-not-supported+

           :abort-request-handler

           :port
           :address
           :taskmaster
           :persistent-connections-p
           :read-timeout
           :write-timeout
           :access-logger
           :message-logger

           :authorization
           :content-length
           :content-type
           :cookie-domain
           :cookie-expires
           :cookie-http-only
           :cookie-in
           :cookie-name
           :cookie-out
           :cookie-path
           :cookie-secure
           :cookie-value
           :cookies-in
           :cookies-out
           :default-document-directory
           :escape-for-html
           :get-parameter
           :get-parameters
           :handle-if-modified-since
           :serve-file
           :request-header
           :response-header
           :host
           :http-token-p
           :toot-condition
           :toot-error
           :toot-warning
           :initialize-connection-stream
           :maybe-invoke-debugger
           :mime-type
           :no-cache
           :parameter
           :parameter-error
           :post-parameter
           :post-parameters
           :process-connection
           :process-request
           :query-string
           :raw-post-data
           :real-remote-addr
           :reason-phrase
           :recompute-request-parameters
           :redirect
           :referer
           :remote-addr
           :remote-port
           :request
           :request-method
           :request-uri
           :require-authorization
           :status-code
           :rfc-1123-date
           :script-name
           :send-headers
           :server-protocol
           :set-cookie
           :shutdown
           :start-server
           :start
           :stop
           :single-threaded-taskmaster
           :thread-per-connection-taskmaster
           :url-decode
           :url-encode
           :user-agent))
