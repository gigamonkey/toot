;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.
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

(in-package :cl-user)

(defpackage :toot
  (:documentation "A very simple web server.")
  (:use :cl
        :alexandria
        :bordeaux-threads
        :chunga
        :cl-ppcre
        :flexi-streams
        :puri)
  (:shadow :defconstant)
  ;; see asdf system definition
  (:import-from :toot-asd :*toot-version*)
  (:export

   :*toot-version*

   :acceptor
   :stream-logger
   :static-file-handler
   :safe-pathname-p
   :add-index

   ;; Generic functions
   :handle-request
   :generate-error-page

   ;; Starting and stopping acceptor
   :start-server
   :start-acceptor
   :stop-acceptor

   ;; Configurable parts of acceptor
   :port
   :address
   :name
   :persistent-connections-p
   :read-timeout
   :write-timeout

   ;; Pluggable parts of acceptor
   :handler
   :error-generator
   :access-logger
   :message-logger
   :taskmaster

   ;; To use in handlers
   :serve-file
   :abort-request-handler
   :no-cache
   :redirect
   :handle-if-modified-since
   :handle-range
   :require-authorization
   :with-response-body
   ;;:headers-sent-p
   :response-sent-p
   ;; Query the request
   :request-method
   :request-uri
   :server-protocol
   :request-header
   :request-headers
   :remote-addr
   :remote-port
   :real-remote-addr
   :authorization
   :get-parameters

   ;; Three ways to get at the body of the request
   :post-parameters
   :body-octets
   :body-stream

   ;; Slightly higher level access to parameters
   :get-parameter
   :post-parameter
   :parameter

   ;; Incoming cookies
   :cookie-value
   :cookies-in

   ;; Control the response
   :send-headers
   :content-length
   :content-type
   :response-header
   :set-cookie
   :status-code

   ;; Utilities
   :escape-for-html
   :url-decode
   :url-encode
   :reason-phrase

   ;; Conditions -- I'm not sure these need to be exported. Does
   ;; anyone ever need to handle them specifically? If so, I need to
   ;; understand under what circumstances and document them.

   ;;:toot-condition
   ;;:toot-error
   ;;:toot-warning
   ;;:parameter-error

   ;; Taskmaster API
   ;;:execute-acceptor
   ;;:handle-incoming-connection
   ;;:shutdown

   ;; Special vars
   :*debug-errors-p*
   :*default-connection-timeout*
   :*default-content-type*
   :*default-external-format*
   :*header-stream*
   :*lisp-errors-log-level*
   :*lisp-warnings-log-level*
   :*log-lisp-backtraces-p*
   :*log-lisp-errors-p*
   :*log-lisp-warnings-p*
   :*show-lisp-backtraces-p*
   :*show-lisp-errors-p*
   :*tmp-directory*

   ;; These are thin wrappers around the corresponding PURI functions.
   :request-scheme
   :request-host
   :request-port
   :request-path
   :request-query
   :request-authority

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
   :+http-version-not-supported+))

(defpackage :toot-tests
  (:documentation "Sanity tests for Toot.")
  (:use :cl :toot))
