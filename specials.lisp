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

(in-package :toot)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar *http-reason-phrase-map* (make-hash-table)
    "Used to map numerical return codes to reason phrases.")

  (defmacro def-http-status-code (name value reason-phrase)
    "Shortcut to define constants for return codes.  NAME is a
Lisp symbol, VALUE is the numerical value of the return code, and
REASON-PHRASE is the phrase \(a string) to be shown in the
server's status line."
    `(eval-when (:compile-toplevel :execute :load-toplevel)
       (defconstant ,name ,value ,(format nil "HTTP return code \(~A) for '~A'."
                                          value reason-phrase))
       (setf (gethash ,value *http-reason-phrase-map*) ,reason-phrase))))

(defconstant +crlf+
  (make-array 2 :element-type '(unsigned-byte 8)
              :initial-contents (mapcar 'char-code '(#\Return #\Linefeed)))
  "A 2-element array consisting of the character codes for a CRLF
sequence.")

(def-http-status-code +http-continue+ 100 "Continue")
(def-http-status-code +http-switching-protocols+ 101 "Switching Protocols")
(def-http-status-code +http-ok+ 200 "OK")
(def-http-status-code +http-created+ 201 "Created")
(def-http-status-code +http-accepted+ 202 "Accepted")
(def-http-status-code +http-non-authoritative-information+ 203 "Non-Authoritative Information")
(def-http-status-code +http-no-content+ 204 "No Content")
(def-http-status-code +http-reset-content+ 205 "Reset Content")
(def-http-status-code +http-partial-content+ 206 "Partial Content")
(def-http-status-code +http-multi-status+ 207 "Multi-Status")
(def-http-status-code +http-multiple-choices+ 300 "Multiple Choices")
(def-http-status-code +http-moved-permanently+ 301 "Moved Permanently")
(def-http-status-code +http-moved-temporarily+ 302 "Moved Temporarily")
(def-http-status-code +http-see-other+ 303 "See Other")
(def-http-status-code +http-not-modified+ 304 "Not Modified")
(def-http-status-code +http-use-proxy+ 305 "Use Proxy")
(def-http-status-code +http-temporary-redirect+ 307 "Temporary Redirect")
(def-http-status-code +http-bad-request+ 400 "Bad Request")
(def-http-status-code +http-authorization-required+ 401 "Authorization Required")
(def-http-status-code +http-payment-required+ 402  "Payment Required")
(def-http-status-code +http-forbidden+ 403 "Forbidden")
(def-http-status-code +http-not-found+ 404 "Not Found")
(def-http-status-code +http-method-not-allowed+ 405 "Method Not Allowed")
(def-http-status-code +http-not-acceptable+ 406 "Not Acceptable")
(def-http-status-code +http-proxy-authentication-required+ 407 "Proxy Authentication Required")
(def-http-status-code +http-request-time-out+ 408 "Request Time-out")
(def-http-status-code +http-conflict+ 409 "Conflict")
(def-http-status-code +http-gone+ 410 "Gone")
(def-http-status-code +http-length-required+ 411 "Length Required")
(def-http-status-code +http-precondition-failed+ 412 "Precondition Failed")
(def-http-status-code +http-request-entity-too-large+ 413 "Request Entity Too Large")
(def-http-status-code +http-request-uri-too-large+ 414 "Request-URI Too Large")
(def-http-status-code +http-unsupported-media-type+ 415 "Unsupported Media Type")
(def-http-status-code +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable")
(def-http-status-code +http-expectation-failed+ 417 "Expectation Failed")
(def-http-status-code +http-failed-dependency+ 424 "Failed Dependency")
(def-http-status-code +http-internal-server-error+ 500 "Internal Server Error")
(def-http-status-code +http-not-implemented+ 501 "Not Implemented")
(def-http-status-code +http-bad-gateway+ 502 "Bad Gateway")
(def-http-status-code +http-service-unavailable+ 503 "Service Unavailable")
(def-http-status-code +http-gateway-time-out+ 504 "Gateway Time-out")
(def-http-status-code +http-version-not-supported+ 505 "Version not supported")

(defconstant +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "The three-character names of the seven days of the week - needed
for cookie date format.")

(defconstant +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "The three-character names of the twelve months - needed for cookie
date format.")

(defvar *default-content-type* "text/html"
  "The default content-type header which is returned to the client.
If this is text content type, the character set used for encoding the
response will automatically be added to the content type in a
``charset'' attribute.")

(defvar *default-charset* :utf-8
  "The default charset for text/* content-types.")

(defvar *header-stream* nil
  "If this variable is not NIL, it should be bound to a stream to
which incoming and outgoing headers will be written for debugging
purposes.")

(defvar *show-lisp-errors-p* nil
  "Whether Lisp errors in request handlers should be shown in HTML output.")

(defvar *show-lisp-backtraces-p* t
  "Whether Lisp errors shown in HTML output should contain backtrace information.")

(defvar *log-lisp-errors-p* t
  "Whether Lisp errors in request handlers should be logged.")

(defvar *log-lisp-backtraces-p* t
  "Whether Lisp backtraces should be logged.  Only has an effect if
*LOG-LISP-ERRORS-P* is true as well.")

(defvar *log-lisp-warnings-p* t
  "Whether Lisp warnings in request handlers should be logged.")

(defvar *lisp-errors-log-level* :error
  "Log level for Lisp errors.  Should be one of :ERROR \(the default),
:WARNING, or :INFO.")

(defvar *lisp-warnings-log-level* :warning
  "Log level for Lisp warnings.  Should be one of :ERROR, :WARNING
\(the default), or :INFO.")

(defvar *debug-errors-p* t
  "When true, Toot drops into the debugger on unhandled errors.
  Otherwise unhandled errors signaled while processing requests are
  logged and a 500 error returned to the client.")

(defconstant +toot-project-url+ "https://github.com/gigamonkey/toot")

(defconstant +implementation-link+
  #+:cmu "http://www.cons.org/cmucl/"
  #+:sbcl "http://www.sbcl.org/"
  #+:allegro "http://www.franz.com/products/allegrocl/"
  #+:lispworks "http://www.lispworks.com/"
  #+:openmcl "http://openmcl.clozure.com/"
  "A link to the website of the underlying Lisp implementation.")

(defvar *tmp-directory*
  #+(or :win32 :mswindows) "c:\\toot-temp\\"
  #-(or :win32 :mswindows) "/tmp/toot/"
  "Directory for temporary files created by MAKE-TMP-FILE-NAME.")

(defvar *tmp-counter-lock* (make-lock "tmp-counter-lock")
  "Lock to protect access to *tmp-counter*.")

(defvar *tmp-counter* 0
  "Counter used in creating tmp filenames.")

(defconstant +latin-1+
  (make-external-format :latin1 :eol-style :lf)
  "A FLEXI-STREAMS external format used for `faithful' input and
output of binary data.")

(defconstant +utf-8+
  (make-external-format :utf8 :eol-style :lf)
  "A FLEXI-STREAMS external format used internally for logging and to
encode cookie values.")

(defvar *default-external-format* +utf-8+
  "The external format used to compute the REQUEST object.")

(defconstant +buffer-length+ 8192
  "Length of buffers used for internal purposes.")

(defvar *default-connection-timeout* 20
  "The default connection timeout used when an acceptor is reading
from and writing to a socket stream.")

(defconstant +new-connection-wait-time+ 2
  "Time in seconds to wait for a new connection to arrive before
performing a cleanup run.")

(pushnew :toot *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>

(defvar *hyperdoc-base-uri* "http://www.gigamonkeys.com/toot/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :toot
             collect (cons symbol (concatenate 'string "#" (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol exported-symbols-alist))))
