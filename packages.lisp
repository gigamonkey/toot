
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

(defpackage #:hunchentoot
  (:use :cl :cl-ppcre :chunga :flexi-streams :url-rewrite)
  (:shadow #:defconstant
           #:url-encode)
  ;; see asdf system definition
  (:import-from :hunchentoot-asd :*hunchentoot-version*)
  (:export #:*acceptor*
           #:*approved-return-codes*
           #:*catch-errors-p*
           #:*default-connection-timeout*
           #:*default-content-type*
           #:*default-handler*
           #:*dispatch-table*
           #:*file-upload-hook*
           #:*handle-http-errors-p*
           #:*header-stream*
           #:*http-error-handler*
           #:*hunchentoot-default-external-format*
           #:*lisp-errors-log-level*
           #:*lisp-warnings-log-level*
           #:*log-lisp-backtraces-p*
           #:*log-lisp-errors-p*
           #:*log-lisp-warnings-p*
           #:*methods-for-post-parameters*
           #:*reply*
           #:*request*
           #:*show-lisp-backtraces-p*
           #:*show-lisp-errors-p*
           #:*tmp-directory*
           #:+http-accepted+
           #:+http-authorization-required+
           #:+http-bad-gateway+
           #:+http-bad-request+
           #:+http-conflict+
           #:+http-continue+
           #:+http-created+
           #:+http-expectation-failed+
           #:+http-failed-dependency+
           #:+http-forbidden+
           #:+http-gateway-time-out+
           #:+http-gone+
           #:+http-internal-server-error+
           #:+http-length-required+
           #:+http-method-not-allowed+
           #:+http-moved-permanently+
           #:+http-moved-temporarily+
           #:+http-multi-status+
           #:+http-multiple-choices+
           #:+http-no-content+
           #:+http-non-authoritative-information+
           #:+http-not-acceptable+
           #:+http-not-found+
           #:+http-not-implemented+
           #:+http-not-modified+
           #:+http-ok+
           #:+http-partial-content+
           #:+http-payment-required+
           #:+http-precondition-failed+
           #:+http-proxy-authentication-required+
           #:+http-request-entity-too-large+
           #:+http-request-time-out+
           #:+http-request-uri-too-large+
           #:+http-requested-range-not-satisfiable+
           #:+http-reset-content+
           #:+http-see-other+
           #:+http-service-unavailable+
           #:+http-switching-protocols+
           #:+http-temporary-redirect+
           #:+http-unsupported-media-type+
           #:+http-use-proxy+
           #:+http-version-not-supported+
           #:abort-request-handler
           #:accept-connections
           #:acceptor
           #:acceptor-access-log-destination
           #:acceptor-address
           #:acceptor-dispatch-request
           #:acceptor-error-template-directory
           #:acceptor-input-chunking-p
           #:acceptor-log-access
           #:acceptor-log-message
           #:acceptor-message-log-destination
           #:acceptor-name
           #:acceptor-output-chunking-p
           #:acceptor-persistent-connections-p
           #:acceptor-port
           #:acceptor-read-timeout
           #:acceptor-reply-class
           #:acceptor-request-class
           #:acceptor-ssl-p
           #-:hunchentoot-no-ssl #:acceptor-ssl-certificate-file               
           #-:hunchentoot-no-ssl #:acceptor-ssl-privatekey-file
           #-:hunchentoot-no-ssl #:acceptor-ssl-privatekey-password
           #:acceptor-status-message
           #:acceptor-write-timeout
           #:authorization
           #:aux-request-value
           #:content-length
           #:content-length*
           #:content-type
           #:content-type*
           #:cookie-domain
           #:cookie-expires
           #:cookie-http-only
           #:cookie-in
           #:cookie-name
           #:cookie-out
           #:cookie-path
           #:cookie-secure
           #:cookie-value
           #:cookies-in
           #:cookies-in*
           #:cookies-out
           #:cookies-out*
           #:create-folder-dispatcher-and-handler
           #:create-prefix-dispatcher
           #:create-regex-dispatcher
           #:create-static-file-dispatcher-and-handler
           #:default-document-directory
           #:define-easy-handler
           #:delete-aux-request-value
           #:dispatch-easy-handlers
           #:easy-acceptor
           #:escape-for-html
           #:execute-acceptor
           #:get-parameter
           #:get-parameters
           #:get-parameters*
           #:handle-incoming-connection
           #:handle-if-modified-since
           #:handle-request
           #:handle-static-file
           #:header-in
           #:header-in*
           #:header-out
           #:headers-in
           #:headers-in*
           #:headers-out
           #:headers-out*
           #:host
           #:http-token-p
           #:hunchentoot-condition
           #:hunchentoot-error
           #:hunchentoot-warning
           #:initialize-connection-stream
           #:log-message*
           #:maybe-invoke-debugger
           #:mime-type
           #-:hunchentoot-no-ssl #:ssl-acceptor
           #:no-cache
           #:parameter
           #:parameter-error
           #:post-parameter
           #:post-parameters
           #:post-parameters*
           #:process-connection
           #:process-request
           #:query-string
           #:query-string*
           #:raw-post-data
           #:real-remote-addr
           #:reason-phrase
           #:recompute-request-parameters
           #:redirect
           #:referer
           #:remote-addr
           #:remote-addr*
           #:remote-port
           #:remote-port*
           #:reply
           #:reply-external-format
           #:reply-external-format*
           #:request
           #:request-acceptor
           #:request-method
           #:request-method*
           #:request-uri
           #:request-uri*
           #:require-authorization
           #:reset-connection-stream
           #:return-code
           #:return-code*
           #:rfc-1123-date
           #:script-name
           #:script-name*
           #:send-headers
           #:server-protocol
           #:server-protocol*
           #:set-cookie
           #:set-cookie*
           #:shutdown
           #:single-threaded-taskmaster
           #:ssl-p
           #:start
           #:start-listening
           #:stop
           #:taskmaster
           #:taskmaster-acceptor
           #:thread-per-connection-taskmaster
           #:url-decode
           #:url-encode
           #:user-agent
           #:within-request-p))

(defpackage :simple-hunchentoot
  (:use #:cl)
  (:export #:start-server
           #:stop-server))