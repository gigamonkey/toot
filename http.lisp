;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2011, Peter Seibel. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :toot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Request handlers. New handlers can be defined by providing methods
;;; on this generic function.

(defgeneric handle-request (handler request)
  (:documentation "Used by the acceptor to handle a request. Returns
  true if the handler actually sends a response. (This is arranged by
  a default :around method. If for some reason a more-specific :around
  method is defined, it must return the same value."))

(defmethod handle-request :around (handler request)
  (call-next-method)
  (response-sent-p request))

(defmethod handle-request ((handler function) request)
  (funcall handler request))

(defmethod handle-request ((handler symbol) request)
  (funcall handler request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error page generation.

(defgeneric generate-error-page (generator request &key error backtrace)
  (:documentation "Used by acceptor to generate an error page for a
  request based on the http status code."))

(defmethod generate-error-page ((generator function) request &key error backtrace)
  (funcall generator request error backtrace))

(defmethod generate-error-page ((generator symbol) request &key error backtrace)
  (funcall generator request error backtrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Classes

(defclass acceptor ()
  (
   ;; Configuration
   (port :initarg :port :reader port)
   (address :initarg :address :reader address)
   (name :initarg :name :reader name)
   (persistent-connections-p :initarg :persistent-connections-p :accessor persistent-connections-p)
   (read-timeout :initarg :read-timeout :reader read-timeout)
   (write-timeout :initarg :write-timeout :reader write-timeout)
   (listen-backlog :initarg :listen-backlog :reader listen-backlog)
   (ssl-certificate-file :initarg :ssl-certificate-file :initform nil :reader ssl-certificate-file)
   (ssl-private-key-file :initarg :ssl-private-key-file :initform nil :reader ssl-private-key-file)
   (ssl-private-key-password :initarg :ssl-private-key-password :initform nil :reader ssl-private-key-password)

   ;; Plugins
   (handler :initarg :handler :accessor handler)
   (error-generator :initarg :error-generator :accessor error-generator)
   (taskmaster :initarg :taskmaster :reader taskmaster)
   (access-loggger
    :initarg :access-logger
    :initform (make-instance 'stream-logger :destination *error-output*)
    :accessor access-logger)
   (message-logger
    :initarg :message-logger
    :initform (make-instance 'stream-logger :destination *error-output*)
    :accessor message-logger)

   ;; State
   (listen-socket        :initform nil :accessor listen-socket)
   (shutdown-p           :initform t :accessor shutdown-p)
   (requests-in-progress :initform 0 :accessor requests-in-progress)
   (shutdown-queue       :initform (make-condition-variable) :accessor shutdown-queue)
   (shutdown-lock        :initform (make-lock "toot-shutdown") :accessor shutdown-lock))

  (:documentation "The object that listens on a socket for connections.")

  (:default-initargs
    :address nil
    :port nil
    :name (format nil "Toot ~a" *toot-version*)
    :listen-backlog 50
    :taskmaster (make-instance (if *supports-threads-p* 'thread-per-connection-taskmaster 'single-threaded-taskmaster))
    :persistent-connections-p t
    :read-timeout *default-connection-timeout*
    :write-timeout *default-connection-timeout*
    :error-generator 'default-error-message-generator))

(defmethod initialize-instance :after ((acceptor acceptor) &key &allow-other-keys)
  (with-slots (port ssl-certificate-file ssl-private-key-file) acceptor
    (unless port (setf port (if ssl-certificate-file 443 80)))
    ;; OpenSSL doesn't know much about Lisp pathnames...
    (when ssl-certificate-file
      (setf ssl-certificate-file (namestring (truename ssl-certificate-file)))
      (setf ssl-private-key-file (namestring (truename ssl-private-key-file))))))

(defmethod print-object ((acceptor acceptor) stream)
  (print-unreadable-object (acceptor stream :type t)
    (format stream "\(host ~A, port ~A)" (or (address acceptor) "*") (port acceptor))))

(defclass request ()
  (;; Information about the request itself
   (remote-addr :initarg :remote-addr :reader remote-addr) ; cgi REMOTE_ADDR
   (remote-port :initarg :remote-port :reader remote-port) ; cgi - weirdly missing
   (request-method :initarg :request-method :reader request-method) ; cgi REQUEST_METHOD
   (server-protocol :initarg :server-protocol :reader server-protocol) ; cgi SERVER_PROTOCOL
   (request-uri :initarg :request-uri :reader request-uri)
   (get-parameters :initform nil :reader get-parameters)
   (post-parameters :initform nil :reader post-parameters)
   (body-stream :initform nil :reader body-stream)
   (body-octets :initform nil :reader body-octets)
   (request-headers :initarg :request-headers :reader request-headers)
   (cookies-in :initform nil :reader cookies-in)

   ;; Information used in generating the reply
   (status-code :initform +http-ok+ :accessor status-code)
   (content-length :initform nil :accessor content-length)
   (content-type :initform *default-content-type* :accessor content-type)
   (response-charset :initform *default-charset* :accessor response-charset)
   (response-headers :initform nil :accessor response-headers)
   (cookies-out :initform nil :accessor cookies-out)

   ;; Lifecycle control
   (headers-sent-p :initform nil :accessor headers-sent-p)
   (close-stream-p :initform t :accessor close-stream-p)

   ;; Internal foo
   (acceptor :initarg :acceptor :reader acceptor)
   (content-stream :initarg :content-stream :accessor content-stream)
   (tmp-files :initform () :accessor tmp-files)))

(defmethod initialize-instance :after ((request request) &key &allow-other-keys)

  (with-slots (get-parameters request-uri request-headers cookies-in) request
    (handler-case*
        (setf
         get-parameters (parse-query-string request-uri)
         cookies-in (parse-cookies request-headers))
      (error (condition)
        (log-message request :error "Error when creating REQUEST object: ~A" condition)
        ;; we assume it's not our fault...
        (setf (status-code request) +http-bad-request+)))))

(defun parse-query-string (request-uri)
  ;; FIXME: This *substitution-char* thing seems hinky to me. Is this
  ;; really the best we can do.?
  (let ((*substitution-char* #\?))
    (form-url-encoded-list-to-alist
     (split "&" (uri-query request-uri)))))

(defun parse-cookies (request-headers)
  ;; The utf-8 decoding here is because we always encode the values in
  ;; outgoing cookies that way, i.e. by url-encoding the values using
  ;; the utf-8 encoding of characters that need escaping. The comma is
  ;; because that's how multiple Cookie headers will be joined and the
  ;; semicolon is because that's how a single Cookie headers delimits
  ;; the separate cookies.
  (form-url-encoded-list-to-alist
   (split "\\s*[,;]\\s*" (cdr (assoc :cookie request-headers)))
   +utf-8+))

;;; Convenience methods to pass along log-message calls until we hit the actual logger.

(defmethod log-message ((acceptor acceptor) log-level format-string &rest format-arguments)
  (apply #'log-message (message-logger acceptor) log-level format-string format-arguments))

(defmethod log-message ((request request) log-level format-string &rest format-arguments)
  (apply #'log-message (acceptor request) log-level format-string format-arguments))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server -- these functions interact with a taskmaster to implement
;;; the basic read a request and reply functionality.

(defun accept-connections (acceptor)
  "Accept connections on our listen socket and hand them back to the
taskmaster via handle-incoming-connection. Called by taskmaster's
execute-acceptor."
  (with-slots (listen-socket shutdown-p read-timeout write-timeout taskmaster) acceptor
    (usocket:with-server-socket (listener listen-socket)
      (loop until shutdown-p do
           (when (usocket:wait-for-input listener :ready-only t :timeout +new-connection-wait-time+)
             (when-let (connection
                        (handler-case (usocket:socket-accept listener)
                          ;; ignore condition
                          (usocket:connection-aborted-error ())))
               (set-timeouts connection read-timeout write-timeout)
               (handle-incoming-connection taskmaster acceptor connection)))))))

(defun process-connection (acceptor socket)
  "Actually process the connection accepted via accept connection.
Called by taskmaster's handle-incoming-connection, possibly in a
different thread than accept-connection is running in."
  (handler-bind ((error
                  ;; abort if there's an error which isn't caught inside
                  (lambda (cond)
                    (maybe-invoke-debugger cond)
                    (log-message
                     acceptor
                     *lisp-errors-log-level*
                     "Error while processing connection: ~A" cond)
                    (return-from process-connection)))
                 (warning
                  ;; log all warnings which aren't caught inside
                  (lambda (cond)
                    (log-message
                     acceptor
                     *lisp-warnings-log-level*
                     "Warning while processing connection: ~A" cond))))
    (usocket:with-mapped-conditions ()
      (let ((content-stream (make-socket-stream socket acceptor)))
        (unwind-protect
             ;; process requests until either the acceptor is shut
             ;; down, close-stream-p on the most recent request is T,
             ;; or the peer fails to send a request
             (loop until (shutdown-p acceptor) do
                  (multiple-value-bind (request-headers request-method url-string protocol)
                      (read-request content-stream)
                    ;; check if there was a request at all
                    (unless request-method (return))

                    (let ((transfer-encodings (cdr (assoc :transfer-encoding request-headers))))

                      (when transfer-encodings
                        (setf transfer-encodings (split "\\s*,\\s*" transfer-encodings))

                        (when (member "chunked" transfer-encodings :test #'equalp)
                          ;; turn chunking on before we read the request body
                          (setf content-stream (make-chunked-stream content-stream))
                          (setf (chunked-stream-input-chunking-p content-stream) t)))

                      (multiple-value-bind (remote-addr remote-port)
                          (get-peer-address-and-port socket)

                        (let ((lock (shutdown-lock acceptor))
                              (request (make-instance 'request
                                         :acceptor acceptor
                                         :remote-addr remote-addr
                                         :remote-port remote-port
                                         :request-headers request-headers
                                         :content-stream content-stream
                                         :request-method request-method
                                         :request-uri (parse-uri url-string)
                                         :server-protocol protocol)))
                          (with-lock-held (lock) (incf (requests-in-progress acceptor)))
                          (unwind-protect
                               (progn
                                 (process-request request)
                                 (log-access (access-logger acceptor) request))
                            (with-lock-held (lock)
                              (decf (requests-in-progress acceptor))
                              (when (shutdown-p acceptor)
                                (condition-notify (shutdown-queue acceptor)))))
                          (finish-response-body request)
                          (when (close-stream-p request) (return)))))))

          (when content-stream
            ;; As we are at the end of the requests here, we ignore
            ;; all errors that may occur while flushing and/or closing
            ;; the stream.
            (ignore-errors (force-output content-stream))
            (ignore-errors (close content-stream :abort t))))))))

(defun process-request (request)
  "Process a single request. Called repeatedly by process-connection."
  ;; SEND-RESPONSE-HEADERS will throw HEAD-REQUEST after the headers
  ;; are written if the request was a HEAD request.

  ;; FIXME: should the CATCH be inside the UNWIND-PROTECT? Hmmm.

  ;; FIXME: Also, after the call to handle-request we should perhaps
  ;; check here that if the content-length was set that the requisite
  ;; number of bytes were sent. If not we should probabaly set
  ;; close-stream-p to t since things are going to be all messed up.
  (catch 'head-request
    (unwind-protect
         (block handle-request
           (handler-bind
               ((warning (lambda (w)
                           (maybe-log-warning request w)))
                (error   (lambda (e)
                           ;; If the headers were already sent, then
                           ;; the error happened within the body and
                           ;; who knows what state things are in. So
                           ;; close the stream.
                           (when (headers-sent-p request)
                             (setf (close-stream-p request) t))
                           (let ((backtrace (get-backtrace)))
                             (maybe-log-error request e backtrace)
                             (report-error-to-client request e backtrace)
                           (return-from handle-request)))))
           (with-debugger
             (handler-case
                 (unless (handle-request (handler (acceptor request)) request)
                   (abort-request-handler +http-not-found+))
               (request-aborted (a)
                 (setf (status-code request) (response-status-code a))
                 (send-response request (or (body a) (error-body request))))))))

      (finish-output (content-stream request))
      (drain-body-stream request)
      (delete-tmp-files request))))

(defun make-socket-stream (socket acceptor)
  (let ((base-stream (usocket:socket-stream socket)))
    (if (ssl-certificate-file acceptor)
        (setup-ssl-stream acceptor base-stream)
        base-stream)))

(defun setup-ssl-stream (acceptor stream)
  ;; attach SSL to the stream if necessary
  (with-slots (ssl-certificate-file ssl-private-key-file ssl-private-key-password) acceptor
    (cl+ssl:make-ssl-server-stream
     stream
     :certificate ssl-certificate-file
     :key ssl-private-key-file
     :password ssl-private-key-password)))

(defun finish-response-body (request)
  (with-slots (content-stream) request
    (force-output content-stream)
    (when (typep content-stream 'chunked-stream)
      ;; Setting these flushes the output stream and checks if there's
      ;; unread input which would be an error.
      (setf (chunked-stream-output-chunking-p content-stream) nil)
      (setf (chunked-stream-input-chunking-p content-stream) nil))))

(defun maybe-log-warning (request warning)
  (when *log-lisp-warnings-p*
    (log-message request *lisp-warnings-log-level* "~a" warning)))

(defun maybe-log-error (request error backtrace)
  (when *log-lisp-errors-p*
    (log-message request *lisp-errors-log-level* "~a~@[~%~a~]" error
                 (and *log-lisp-backtraces-p* backtrace))))

(defun report-error-to-client (request error &optional backtrace)
  (setf (status-code request) +http-internal-server-error+)
  (send-response
   request
   (error-body request :error error :backtrace backtrace)
   :content-type "text/html"
   :charset :utf-8))

(defun error-body (request &key error backtrace)
  (let ((generator (error-generator (acceptor request))))
    (generate-error-page generator request :error error :backtrace backtrace)))

(defun drain-body-stream (request)
  (when-let (stream (slot-value request 'body-stream))
    (loop for char = (read-byte stream nil nil) while char)))

(defun delete-tmp-files (request)
  (dolist (path (tmp-files request))
    (when (and (pathnamep path) (probe-file path))
      ;; the handler may have chosen to (re)move the uploaded
      ;; file, so ignore errors that happen during deletion
      (ignore-errors* (delete-file path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Request -- reading the HTTP request from the client

(defun read-request (stream)
  "Reads incoming headers from the client via STREAM. Returns as
multiple values the headers as an alist, the request-method, the URI,
and the protocol of the request. The reading of the headers is handled
by Chunga's read-http-headers method."
  (with-character-stream-semantics
   (let ((first-line (read-initial-request-line stream)))
     (when first-line
       (unless (every #'printable-ascii-char-p first-line)
         (send-bad-request-response stream "Non-ASCII character in request line")
         (return-from read-request nil))
       (destructuring-bind (&optional request-method url-string protocol)
           (split "\\s+" first-line :limit 3)
         (unless url-string
           (send-bad-request-response stream)
           (return-from read-request nil))
         (when *header-stream*
           (format *header-stream* "~A~%" first-line))
         (let ((headers (and protocol (read-http-headers stream *header-stream*))))
           (unless protocol (setf protocol "HTTP/0.9"))
           ;; maybe handle 'Expect: 100-continue' header
           (when-let (expectations (cdr (assoc :expect headers)))
             (when (member "100-continue" (split "\\s*,\\s*" expectations) :test #'equalp)
               ;; according to 14.20 in the RFC - we should actually
               ;; check if we have to respond with 417 here
               (let ((s (make-header-stream stream)))
                 (write-status-line s +http-continue+)
                 (write-line-crlf s ""))))
           (values headers
                   (as-keyword request-method)
                   url-string
                   (as-keyword (trim-whitespace protocol)))))))))

(defun read-initial-request-line (stream)
  (handler-case
      (let ((*current-error-message* "While reading initial request line:"))
        (usocket:with-mapped-conditions ()
          (read-line* stream)))
    ((or end-of-file usocket:timeout-error) ())))

(defun printable-ascii-char-p (char)
  (<= 32 (char-code char) 126))

(defun get-peer-address-and-port (socket)
  "Returns the peer address and port of the socket SOCKET as two
values.  The address is returned as a string in dotted IP address
notation."
  (values (usocket:vector-quad-to-dotted-quad (usocket:get-peer-address socket))
          (usocket:get-peer-port socket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Post parameters API. Three choices. 1. Get the post parameters
;;; parsed into an alist. 2. Get a stream from which the request body
;;; can be read. 3. Get the post body as a vector of octets.

;; Technically, we could allow somone to call request-body-octets and
;; later call post-parameters since we could parse the octets that
;; we've saved. But if they get the stream, all bets are off so for
;; consistency we'll just say you have to pick which form you want to
;; use.

(defmethod post-parameters :before ((request request))
  "Lazily fill in the post-parameters slot with data from the request body."
  (with-slots (post-parameters body-stream body-octets) request
    (unless post-parameters
      (when body-stream (error "Request body already retrieved as a stream."))
      (when body-octets (error "Request body already retrieved as octets."))
      (setf post-parameters (read-post-parameters request)))))

(defmethod body-stream :before ((request request))
  (with-slots (post-parameters body-stream body-octets) request
    (unless body-stream
      (when post-parameters (error "Request body already retrieved as parsed post parameters."))
      (when body-octets (error "Request body already retrieved as octets."))
      (setf body-stream (request-body-stream request)))))

(defmethod body-octets :before ((request request))
  (with-slots (post-parameters body-stream body-octets) request
    (unless body-octets
      (when post-parameters (error "Request body already retrieved as parsed post parameters."))
      (when body-stream (error "Request body already retrieved as a stream."))
      (setf body-octets (read-body-octets request)))))

(defun read-post-parameters (request)
  "Read the post parameters from the body of the request and return them as an alist."

  (unless (or (request-header :content-length request) (chunking-input-p request))
    (log-message request :warning "Can't read request body because there's ~
no Content-Length header and input chunking is off.")
    (return-from read-post-parameters nil))

  (handler-case*
      (multiple-value-bind (type subtype charset)
          (parse-content-type-header (request-header :content-type request))

        (let ((external-format (charset-to-external-format charset)))

          (cond
            ((and (string-equal type "application") (string-equal subtype "x-www-form-urlencoded"))
             (parse-application/x-www-form-urlencoded request external-format))

            ((and (string-equal type "multipart") (string-equal subtype "form-data"))
             (parse-multipart/form-data request external-format)))))

    (error (condition)
      (log-message request :error "Error when reading POST parameters from body: ~A" condition)
      ;; this is not the right thing to do because it could happen
      ;; that we aren't finished reading from the request stream and
      ;; can't send a reply - to be revisited
      (setf (close-stream-p request) t)
      (abort-request-handler +http-bad-request+))))

(defun request-body-stream (request)
  "Return a stream from which the body of the request can be read. If
the request specified a content-length, this stream will not read
beyond it. And if the request is using chunked transfer encoding, the
stream will be a chunked stream that will return :eof when it gets to
the end of the input. After the request has been handled, any input
remaining on the stream will be drained."
  ;; FIXME: this should really be an octet stream, it seems. Thus the
  ;; +latin-1+ external format. But we want a flexi-stream because we
  ;; want to be able to set the stream-bound. Perhaps could look into
  ;; using trivial-gray-stream-mixin ourself to define such a stream.
  ;; (We need to return such a limited stream to make sure user code
  ;; doesn't read the beginning of the next request on a persistent
  ;; connection.)

  ;; Or maybe this function should take a charset argument which
  ;; defaults to whatever was specified in the request headers if the
  ;; content is text.
  (let ((content-length (parse-integer (request-header :content-length request) :junk-allowed t))
        (content-stream (content-stream request)))
    (cond
      (content-length
       (when (chunking-input-p request)
         ;; see RFC 2616, section 4.4
         (log-message request :warning "Got Content-Length header although input chunking is on."))
       (let ((stream (make-flexi-stream (content-stream request) :external-format +latin-1+)))
         (setf (flexi-stream-bound stream) content-length)
         stream))
      ((chunking-input-p request) content-stream))))

(defun read-body-octets (request)
  "Read the post data and return it as a vector of octets."
  (let ((content-length (parse-integer (request-header :content-length request) :junk-allowed t))
        (content-stream (content-stream request)))
    (cond
      (content-length
       (when (chunking-input-p request)
         ;; see RFC 2616, section 4.4
         (log-message request :warning "Got Content-Length header although input chunking is on."))
       (let ((content (make-array content-length :element-type 'octet)))
         (read-sequence content content-stream)
         content))

      ((chunking-input-p request)
       (loop with buffer = (make-array +buffer-length+ :element-type 'octet)
          with content = (make-array 0 :element-type 'octet :adjustable t)
          for index = 0 then (+ index pos)
          for pos = (read-sequence buffer content-stream)
          do
            (adjust-array content (+ index pos))
            (replace content buffer :start1 index :end2 pos)
          while (= pos +buffer-length+)
          finally (return content))))))

(defun charset-to-external-format (charset)
  (or
   (when charset
     (handler-case
         (make-external-format charset :eol-style :lf)
       (error ()
         (toot-warn "Ignoring unknown character set ~A in request content type." charset))))
   *default-external-format*))

(defun parse-application/x-www-form-urlencoded (request external-format)
  ;; FIXME: I'm not sure what this +latin-1+ bit is about. I think it
  ;; may just be so we can use cl-ppcre:split. In that case, maybe
  ;; better to use split-sequence:split-sequence directly on the
  ;; octets. However that seems hinky. Shouldn't we decode the the
  ;; string once and then split on #\& once it's a string?
  (form-url-encoded-list-to-alist
   (split "&" (octets-to-string (read-body-octets request) :external-format +latin-1+))
   external-format))

(defun parse-multipart/form-data (request external-format)
  "Parse the REQUEST body as multipart/form-data, assuming that its
content type has already been verified.  Returns the form data as
alist or NIL if there was no data or the data could not be parsed."
  (handler-case*
      (let ((content-stream (make-flexi-stream (content-stream request) :external-format +latin-1+)))
        (parse-rfc2388-form-data
         content-stream
         (request-header :content-type request)
         external-format
         (lambda () (first (push (tmp-filename) (tmp-files request))))))
    (error (condition)
      (log-message request :error "While parsing multipart/form-data parameters: ~A" condition)
      nil)))

(defun parse-rfc2388-form-data (stream content-type-header external-format tmp-filename-generator)
  "Creates an alist of POST parameters from the stream STREAM which is
supposed to be of content type 'multipart/form-data'."
  (let* ((parsed-content-type-header (parse-header content-type-header :value))
         (boundary (or (cdr (find-parameter
                             "BOUNDARY"
                             (header-parameters parsed-content-type-header)))
                       (return-from parse-rfc2388-form-data))))
    (loop for part in (parse-mime stream boundary tmp-filename-generator)
       for headers = (mime-part-headers part)
       for content-disposition-header = (find-content-disposition-header headers)
       for name = (cdr (find-parameter
                        "NAME"
                        (header-parameters content-disposition-header)))
       when name
       collect (cons name
                     (let ((contents (mime-part-contents part)))
                       (if (pathnamep contents)
                           (list contents
                                 (get-file-name headers)
                                 (mime-content-type part :as-string t))
                           (convert-hack contents external-format)))))))

(defun convert-hack (string external-format)
  "The rfc2388 code is buggy in that it operates on a character stream
and thus only accepts encodings which are 8 bit transparent. In order
to support different encodings for parameter values submitted, we post
process whatever string values the rfc2388 package has returned."
  (flex:octets-to-string (map '(vector (unsigned-byte 8) *) 'char-code string)
                         :external-format external-format))

(defun chunking-input-p (request)
  "Whether input chunking is currently switched on for the acceptor's
content stream."
  (chunked-stream-input-chunking-p (content-stream request)))

(defun external-format-from-content-type (content-type)
  "Creates and returns an external format corresponding to the value
of the content type header provided in CONTENT-TYPE.  If the content
type was not set or if the character set specified was invalid, NIL is
returned."
  (when content-type
    (when-let (charset (nth-value 2 (parse-content-type-header content-type)))
      (handler-case
          (make-external-format (as-keyword charset) :eol-style :lf)
        (error ()
          (toot-warn "Invalid character set ~S in request has been ignored." charset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Response -- sending back the HTTP response.

;; FIXME: probably should send HTTP/1.0 if the request was.
(defun send-bad-request-response (stream &optional additional-info)
  (write-simple-response
   stream
   +http-bad-request+
   '((:connection . "close"))
   (format nil "Your request could not be interpreted by this HTTP server~C~C~@[~A~C~C~]"
           #\Return #\Linefeed
           additional-info
           #\Return #\Linefeed)))

;;; FIXME: possibly the call site of handler-incoming-connection
;;; should be set up to allow the taskmaster to simply signal a
;;; condition or throw something
(defun send-service-unavailable-response (acceptor socket)
  "Send a response to the client before we've created a request
object. This can be used by taskmasters when they cannot accept a
connection."
  (write-simple-response
   (make-socket-stream socket acceptor)
   +http-service-unavailable+
   ;; FIXME: hmmm. this was :content rather than :content-length but
   ;; I'm thinking that was a translation error. check. And maybe
   ;; more to the point, it should be :connection . "close" like in
   ;; send-bad-request-response.
   '((:content-length . 0))))

(defun write-simple-response (stream status-code &optional headers content)
  (with-open-stream (s (make-header-stream stream))
    (write-status-line s status-code)
    (write-headers s headers)
    (write-line-crlf s "")
    (when content (write-line-crlf stream content))))

;; FIXME: technically a HEAD request SHOULD still have a
;; Content-Length header specifying "the size of the entity-body that
;; would have been sent had the request been a GET". Though that's
;; hard to do in the case where the handler writes to a stream. But
;; it's not MUST so maybe don't worry about it.

(defun send-response (request content &key content-type (charset *default-charset*))
  "Send a full response with the given content as the body."
  (let ((stream (content-stream request))
        (encoded (string-to-octets content :external-format charset)))
    (send-response-headers request (length encoded) content-type charset)
    (write-sequence encoded stream)))

(defun send-response-headers (request content-length content-type charset)
  "Send the response headers and return the stream to which the body
of the response can be written. The stream is a binary stream. The
public API function, SEND-HEADERS will wrap that stream in a
flexi-stream based on the content-type and charset, if needed. Thus
function is for functions that are going to take care of encoding the
response themselves, such as SERVE-FILE, which just dumps an already
encoded to the steam as octets. If the request was a HEAD request we
dynamically abort rather than returning a stream."
  ;; Set content-length, content-type and external format if they're
  ;; supplied by caller. They could also have been set directly before
  ;; this function was called.
  (when content-length (setf (content-length request) content-length))
  (when content-type (setf (content-type request) content-type))
  (when charset (setf (response-charset request) charset))

  (finalize-response-headers request)

  (with-slots (content-stream) request
    (let ((header-stream (make-header-stream content-stream)))
      (write-status-line header-stream (status-code request))
      (write-headers header-stream (response-headers request))
      (write-cookies header-stream (cookies-out request))
      (write-line-crlf header-stream ""))
    (setf (headers-sent-p request) t)

    (when (eql (request-method request) :head)
      (throw 'head-request nil))

    (when (string= (response-header :transfer-encoding request) "chunked")
      (unless (typep content-stream 'chunked-stream)
        (setf content-stream (make-chunked-stream content-stream)))
      (setf (chunked-stream-output-chunking-p content-stream) t))

    content-stream))

(defun finalize-response-headers (request)
  "Set certain headers automatically based on values in the request object."
  (flet ((set-header (name value) (setf (response-header name request) value)))

    (set-header :date (rfc-1123-date))
    (set-header :content-type (full-content-type request))
    (set-header :server (name (acceptor request)))
    (when (content-length request) (set-header :content-length (content-length request)))

    ;; Chunked encoding only available in http/1.1 and only needed if
    ;; we don't know the length of the content we're sending.
    (let* ((http/1.1-p (eql (server-protocol request) :http/1.1))
           (chunkedp (and http/1.1-p (not (content-length request)))))

      (when chunkedp (set-header :transfer-encoding "chunked"))

      (multiple-value-bind (keep-alive-p keep-alive-requested-p) (keep-alive-p request)
        (cond
          ((and keep-alive-p (or chunkedp (length-known-p request)))
           (setf (close-stream-p request) nil)
           (let ((read-timeout (read-timeout (acceptor request))))
             (when (and read-timeout keep-alive-requested-p)
               ;; In HTTP/1.0 keep-alive-p and keep-alive-requested-p
               ;; will always be the same. In HTTP/1.1 persistent
               ;; connections are assumed, but we'll return a
               ;; 'Keep-Alive' header if the client has explicitly
               ;; asked for one.
               (set-header :connection "Keep-Alive")
               ;; FIXME: perhaps we should set the Connection header
               ;; regardless of the read-timeout and only set this
               ;; header if there's a timeout.
               (set-header :keep-alive (format nil "timeout=~D" read-timeout)))))
          (t
           ;; If we aren't doing keep-alive then we need to tell the
           ;; client we're going to close the connection after sending
           ;; the reply.
           (setf (close-stream-p request) t)
           (set-header :connection "close")))))))

(defun length-known-p (request)
  (let ((head-request-p (eql (request-method request) :head))
        (not-modified-response-p (eql (status-code request) +http-not-modified+)))
    (or head-request-p not-modified-response-p (content-length request))))

(defun keep-alive-p (request)
  "Should the current connection be kept alive? Secondary value
indicates whether the client explicitly requested keep-alive. (Always
the same as the primary value for HTTP/1.0 but potentially different
in HTTP/1.1.)"
  (let ((connection-values (connection-values request)))
    (flet ((connection-value-p (value)
             (member value connection-values :test #'string-equal)))

      (let ((keep-alive-requested-p (connection-value-p "keep-alive")))
        (values (and (persistent-connections-p (acceptor request))
                     (case (server-protocol request)
                       (:http/1.1 (not (connection-value-p "close")))
                       (:http/1.0 keep-alive-requested-p)))
                keep-alive-requested-p)))))

(defun connection-values (request)
  ;; the header might consist of different values separated by commas
  (when-let (connection-header (request-header :connection request))
    (split "\\s*,\\s*" connection-header)))

(defun make-header-stream (stream)
  "Make a stream just for writing the HTTP headers."
  (let ((header-stream (make-flexi-stream stream :external-format :iso-8859-1)))
    (if *header-stream* (make-broadcast-stream *header-stream* header-stream) header-stream)))

(defun text-type-p (content-type)
  (cl-ppcre:scan "(?i)^text" content-type))

(defun full-content-type (request)
  "Return the value for the Content-Type header, including a charset if it's a text/* type."
  (with-slots (content-type response-charset) request
    (if (text-type-p content-type)
        (format nil "~a; charset=~(~a~)" content-type response-charset)
        content-type)))

(defun write-line-crlf (stream fmt &rest args)
  (apply #'format stream fmt args)
  (write-char #\Return stream)
  (write-char #\Linefeed stream))

(defun write-status-line (stream status-code)
  (write-line-crlf stream "HTTP/1.1 ~D ~A" status-code (reason-phrase status-code)))

(defun write-headers (stream headers)
  (loop for (key . value) in headers
     when value do (write-header-line (as-capitalized-string key) value stream)))

(defun write-cookies (stream cookies)
  (loop for (nil . cookie) in cookies
     do (write-header-line "Set-Cookie" (stringify-cookie cookie) stream)))

(defun write-header-line (key value stream)
  (let ((string (princ-to-string value)))
    (write-string key stream)
    (write-char #\: stream)
    (write-char #\Space stream)
    (let ((start 0))
      (loop
         (let ((end (or (position #\Newline string :start start) (length string))))
           ;; skip empty lines, as they confuse certain HTTP clients
           (unless (eql start end)
             (unless (zerop start) (write-char #\Tab stream))
             (write-string string stream :start start :end end)
             (write-line-crlf stream ""))
           (setf start (1+ end))
           (when (<= (length string) start) (return)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A bare-bones error page generator

(defparameter *error-css* "
body {
  font-family: \"Gill Sans\", sans-serif;
  width: 600px;
  margin: 1in auto;

}

h1 { font-size: 64px; }

div {
  text-align: center;
  background: #ddddff;
  padding: 10px;
  border: thin solid black;
  height: 300px;
}
")

(defun default-error-message-generator (request error backtrace)
  "A function that generates a bare-bones error page to be used as an error page generator."
  (let ((status-code (status-code request)))
    (with-output-to-string (s)
      (format s "~&<html><head><style type='text/css'>~a</style><title>~d: ~a</title></head><body><div><h1>~2:*~d: ~a</h1>"
              *error-css*
              status-code (reason-phrase status-code))
      (when (and error *show-lisp-errors-p*)
        (format s "<pre>~a~@[~%~%Backtrace:~%~%~a~]</pre>"
                  (escape-for-html (princ-to-string error))
                  (when (and backtrace *show-lisp-backtraces-p*)
                    (escape-for-html (princ-to-string backtrace)))))

      (flet ((escaped (arg) (and arg (escape-for-html arg))))
        (let ((host (request-header :host request)))
          (format s "<address><a href='~a'>Toot ~a</a> running on <a href='~A'>(~A ~A)</a>~@[ at ~A~:[ (port ~D)~;~]~]</address>"
              +toot-project-url+
              *toot-version*
              +implementation-link+
              (escaped (lisp-implementation-type))
              (escaped (lisp-implementation-version))
              (escaped (or host (address (acceptor request))))
              (scan ":\\d+$" (or host ""))
              (port (acceptor request)))))


      (format s "~&</div></body></html>"))))

(defun reason-phrase (status-code)
  "Returns a reason phrase for the HTTP return code STATUS-CODE (which
should be an integer) or NIL for return codes Toot doesn't know."
  (gethash status-code *http-reason-phrase-map* "No reason phrase known"))
