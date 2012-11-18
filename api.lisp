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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start and stop the server

(defun start-server (&rest args &key (handler (error "Must specify handler.")) port &allow-other-keys)
  "Instantiate an acceptor and start it listening."
  (start-acceptor
   (apply #'make-instance 'acceptor
          :handler handler
          :port port
          (sans args :handler :port))))

(defun start-acceptor (acceptor)
  "Start an existing acceptor listening for connections."
  (when (listen-socket acceptor)
    (internal-error "acceptor ~A is already listening" acceptor))

  (setf (shutdown-p acceptor) nil)
  (setf (listen-socket acceptor)
        (usocket:socket-listen
         (or (address acceptor) usocket:*wildcard-host*) (port acceptor)
         :reuseaddress t
         :backlog (listen-backlog acceptor)
         :element-type '(unsigned-byte 8)))
  ;; Reset the port in case we passed 0 to get a random port.
  (setf (slot-value acceptor 'port) (usocket:get-local-port (listen-socket acceptor)))
  (execute-acceptor (taskmaster acceptor) acceptor)
  acceptor)

(defun stop-acceptor (acceptor &key soft)
  "Stop an acceptor from listening for connections. It can be
restarted with START-ACCEPTOR."
  (setf (shutdown-p acceptor) t)
  (shutdown (taskmaster acceptor))
  (when soft
    (with-lock-held ((shutdown-lock acceptor))
      ;; FIXME: seems like this should perhaps be a while loop not a
      ;; WHEN? The thread which called STOP is waiting here while all
      ;; the threads processing requests will signal on the
      ;; shutdown-queue
      (when (plusp (requests-in-progress acceptor))
        (condition-wait (shutdown-queue acceptor) (shutdown-lock acceptor)))))
  (usocket:socket-close (listen-socket acceptor))
  (setf (listen-socket acceptor) nil)
  acceptor)

(defun request-scheme (request)
  "Get the scheme part of the request's URI."
  (uri-scheme (request-uri request)))

(defun request-host (request)
  "Get the host part of the request's URI."
  (uri-host (request-uri request)))

(defun request-port (request)
  "Get the port part of the request's URI."
  (uri-port (request-uri request)))

(defun request-path (request)
  "Get the path part of the request's URI."
  (uri-path (request-uri request)))

(defun request-query (request)
  "Get the query part of the request's URI."
  (uri-query (request-uri request)))

(defun request-authority (request)
  "Get the authority part of the request's URI."
  (uri-authority (request-uri request)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defmacro with-response-body ((stream request &rest headers &key &allow-other-keys) &body body)
  "Send the response headers (any already set plus any more set via
keyword arguments to this macro) and bind the stream to which the
response body can be written to STREAM."
  (once-only (request)
    `(progn
       ;; When headers is NIL SBCL whines about unreachable code if we
       ;; generate the LOOP
       ,@(when headers
               `((loop for (name value) on (list ,@headers) by #'cddr do
                      (setf (response-header name ,request) value))))
       (let ((,stream (send-headers ,request))) ,@body)
       t)))

(defun response-header (name request)
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (response-headers request))))

(defun (setf response-header) (new-value name request)
  "Changes the current value of the outgoing http header named NAME (a
keyword or a string). If a header with this name doesn't exist, it is
created."
  (when (headers-sent-p request)
    (error "Can't set reply headers after headers have been sent."))

  (when (stringp name)
    (setf name (as-keyword name :destructivep nil)))

  (let ((entry (assoc name (response-headers request))))
    (if entry
        (setf (cdr entry) new-value)
        (push (cons name new-value) (response-headers request)))

    ;; Special case these two. This is kind of hinky, but we need to
    ;; set the slots if these headers are set since the slot values
    ;; will be used in finalize-response-headers to set the actual
    ;; header. Note that this relation is not directly symmetical.
    ;; Setting the slot in the object does not immediately set the
    ;; value in the headers alist. But it will eventually affect it in
    ;; finalize-response-headers.
    (case name
      (:content-length
       (check-type new-value integer)
       (setf (content-length request) new-value))
      (:content-type
       (check-type new-value (or null string))
       (setf (content-type request) new-value)))

    new-value))

(defun response-sent-p (request)
  "Has a response been sent."
  (headers-sent-p request))

(defun send-headers (request &key
                     (content-type *default-content-type*)
                     (charset *default-charset*)
                     (status-code +http-ok+))
  "Send the headers and return a stream to which the body of the reply
can be written. If the content-type is text/* type, the stream
returned will be a character stream that will encode the response
properly for the charset specified. If the request was a HEAD request
we dynamically abort rather than returning a stream."
  (setf (status-code request) status-code)
  (let ((stream (send-response-headers request nil content-type charset)))
    (if (text-type-p content-type)
        (make-flexi-stream stream :external-format (make-external-format charset))
        stream)))

(defun abort-request-handler (response-status-code &optional body)
  "Abort the handling of a request, sending instead a response with
the given response-status-code and either the given body or a default
body based on the error code. A request can only be aborted if
SEND-HEADERS has not been called. (SEND-HEADERS is called by
WITH-RESPONSE-BODY). If a handler neither generates a response nor
aborts, then a 404: Not Found response will be sent."
  (error 'request-aborted :response-status-code response-status-code :body body))

(defun request-header (name request)
  "Returns the incoming header with name NAME. NAME can be a
keyword (recommended) or a string."
  (cdr (assoc name (request-headers request) :test #'equalp)))

(defun authorization (request)
  "Returns as two values the user and password (if any) as encoded in
the 'AUTHORIZATION' header. Returns NIL if there is no such header."
  (let* ((authorization (request-header :authorization request))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (split ":" (base64:base64-string-to-string (subseq authorization start)))
        (values user password)))))

(defun get-parameter (name request)
  "Returns the GET parameter with name NAME (a string) - or NIL if
there is none. Search is case-sensitive."
  (cdr (assoc name (get-parameters request) :test #'string=)))

(defun post-parameter (name request)
  "Returns the POST parameter with name NAME (a string) - or NIL if
there is none. Search is case-sensitive."
  (cdr (assoc name (post-parameters request) :test #'string=)))

(defun parameter (name request)
  "Returns the GET or the POST parameter with name NAME (a string) -
or NIL if there is none. If both a GET and a POST parameter with the
same name exist the GET parameter is returned. Search is
case-sensitive."
  (or (get-parameter name request) (post-parameter name request)))

(defun real-remote-addr (request)
  "Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value."
  (let ((x-forwarded-for (request-header :x-forwarded-for request)))
    (cond (x-forwarded-for (let ((addresses (split "\\s*,\\s*" x-forwarded-for)))
                             (values (first addresses) addresses)))
          (t (remote-addr request)))))

(defun serve-file (request pathname &optional content-type (charset *default-charset*))
  "Serve the file denoted by PATHNAME. Sends a content type header
corresponding to CONTENT-TYPE or (if that is NIL) tries to determine
the content type via the file's suffix. Aborts the request with 404:
Not found if the file does not exist. Also handles if-modified-since
and range requests appropriately."
  (when (or (not pathname)
            (wild-pathname-p pathname)
            (not (fad:file-exists-p pathname))
            (fad:directory-exists-p pathname))
    (abort-request-handler +http-not-found+))

  (let ((time (or (file-write-date pathname) (get-universal-time))))
    (setf (response-header :accept-ranges request) "bytes")
    (handle-if-modified-since time request)

    (with-open-file (file pathname :direction :input :element-type 'octet :if-does-not-exist nil)
      (multiple-value-bind (start bytes-to-send) (handle-range request (file-length file))
        (when (plusp start) (file-position file start))
        (setf (status-code request) +http-ok+)
        (let* ((type (or content-type (guess-mime-type (pathname-type pathname))))
               (out (send-response-headers request bytes-to-send  type charset))
               (buf (make-array +buffer-length+ :element-type 'octet)))
          ;; FIXME: is this necessary? We shouldn't have a
          ;; flexi-stream at this point. In fact, this should probably
          ;; blow up because of that.
          #+:clisp
          (setf (flexi-stream-element-type (content-stream (acceptor request))) 'octet)
          (loop until (zerop bytes-to-send) do
               (let ((chunk-size (min +buffer-length+ bytes-to-send)))
                 (unless (eql chunk-size (read-sequence buf file :end chunk-size))
                   (error "can't read from input file"))
                 (write-sequence buf out :end chunk-size)
                 (decf bytes-to-send chunk-size)))
          (finish-output out))))))

(defun no-cache (request)
  "Adds appropriate response headers to completely prevent caching on
most browsers."
  ;; WTF is this date?! (Some cargo cult thing from PHP or maybe MSDN, it seems.)
  (setf (response-header :expires request) "Mon, 26 Jul 1997 05:00:00 GMT")
  (setf (response-header :cache-control request) "no-store, no-cache, must-revalidate, post-check=0, pre-check=0")
  (setf (response-header :pragma request) "no-cache")
  (setf (response-header :last-modified request) (rfc-1123-date)))

(defun redirect (request target &key
                 (code +http-moved-temporarily+)
                 protocol
                 host
                 port)
  "Redirects the browser to TARGET with status code CODE. Target must
be a string and CODE should be one of the 3xx status codes. If TARGET
is a full URL starting with a scheme, HOST, PORT and PROTOCOL are
ignored. Otherwise, TARGET should denote the path part of a URL and
the protocol, host, and port can be specified via keyword args. Any
values not specified will be taken from the current request. (Note,
however, that if no port was specified in the Host: header of the
request, the redirect will likewise have no explicit port; if the
protocol was changed this will result in a redirect to the default
port for the new protocol. CODE must be a 3xx redirection code and
will be sent as status code."
  (check-type code (integer 300 399))
(flet ((just-host (host-and-port)
         (subseq host-and-port 0 (position #\: host-and-port)))
       (just-port (host-and-port)
         (let ((colon (position #\: host-and-port)))
           (and colon (subseq host-and-port (1+ colon))))))
  (let ((url
         (if (uri-scheme (parse-uri target))
             target
             (let* ((requested-host (request-header :host request))
                    (current-protocol (if (ssl-certificate-file (acceptor request)) :https :http)))
               (format nil "~(~a~)://~a~@[:~a~]~a"
                       (or protocol current-protocol)
                       (or host (just-host requested-host))
                       (or port (just-port requested-host))
                       target)))))
    (setf (response-header :location request) url)
    (abort-request-handler code))))

(defun require-authorization (request &optional (realm "Toot"))
  "Sends 401: Authorization required reply to require basic HTTP
authentication (see RFC 2617) for the realm REALM."
  (setf (response-header :www-authenticate request)
        (format nil "Basic realm=\"~A\"" (quote-string realm)))
  (abort-request-handler +http-authorization-required+))

(defun handle-if-modified-since (time request)
  "Handles the If-Modified-Since header of REQUEST, sending an '304:
Not modified' response if the time represented by the UTC TIME is the
same as the value in the If-Modified-Since header. Also sets the Last
Modified header in the response to TIME."
  (setf (response-header :last-modified request) (rfc-1123-date time))
  (let ((if-modified-since (request-header :if-modified-since request))
        (time-string (rfc-1123-date time)))
    ;; simple string comparison is sufficient; see RFC 2616 14.25
    (when (and if-modified-since (equal if-modified-since time-string))
      (abort-request-handler +http-not-modified+))))

(defun handle-range (request bytes-available)
  "If the request contains a Range header returns the starting
position and the number of bytes to transfer. Otherwise returns 0 and
bytes-available. An invalid specified range is reported to the client
immediately with a '416: Requested range not satisfiable' response."
  (or
   (register-groups-bind (start end)
       ("^bytes (\\d+)-(\\d+)$" (request-header :range request) :sharedp t)
     ;; body won't be executed if regular expression does not match
     (setf start (parse-integer start))
     (setf end (parse-integer end))
     (when (or (< start 0) (>= end bytes-available))
       (setf (response-header :content-range request) (format nil "bytes 0-~D/*" (1- bytes-available)))
       (abort-request-handler
        +http-requested-range-not-satisfiable+
        (format nil "invalid request range (requested ~D-~D, accepted 0-~D)"
                start end (1- bytes-available))))
       (setf (status-code request) +http-partial-content+)
       (setf (response-header :content-range request) (format nil "bytes ~D-~D/*" start end))
       (values start (1+ (- end start))))
   (values 0 bytes-available)))

(defun cookie-value (name request)
  "Get the value of the cookie with the given name sent by the client
or NIL if no such cookie was sent."
  (when-let (cookie (cdr (assoc name (cookies-in request) :test #'string=)))
    cookie))

(defun set-cookie (name request &key (value "") expires path domain secure http-only)
  "Set a cookie to be sent with the reply."
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

(defun cookie-out (name request)
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out request) :test #'string=)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trivial static file handler

(defclass static-file-handler ()
  ((root :initarg :root :accessor root)
   (path-checker :initarg :path-checker :initform #'safe-pathname-p :accessor path-checker))

  (:documentation "A handler that serves files found under a given
  root directory. Checks the path before serving the file with
  specified path-checker which should be a function that takes the
  path and returns true if it is safe. If the path checker returns
  false, the request is aborted with 403: Forbidden."))

(defmethod initialize-instance :after ((h static-file-handler) &key &allow-other-keys)
  (with-slots (root) h
    (setf root (truename (merge-pathnames root)))))

(defmethod handle-request ((handler static-file-handler) request)
  (with-slots (root path-checker) handler
    (let ((*default-pathname-defaults* root)
          (path (request-path request)))
      (unless (funcall path-checker path)
        (abort-request-handler +http-forbidden+))
      (serve-file request (merge-pathnames (subseq (add-index path) 1))))))

(defun safe-pathname-p (path)
  "Verify that a path, translated to a file doesn't contain any tricky
bits such as '..'"
  (let ((directory (pathname-directory (subseq path 1))))
    (or (stringp directory)
        (null directory)
        (and (consp directory)
             (eql (first directory) :relative)
             (every #'stringp (rest directory))))))

(defun add-index (filename &key (name "index") (extension "html"))
  "Add an index file name to a directory filename. Defaults to index.html"
  (format nil "~a~:[~;~a~@[.~a~]~]" filename (ends-with #\/ filename) name extension))
