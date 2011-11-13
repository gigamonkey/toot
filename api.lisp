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

(in-package :toot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defmacro maybe-handle (test &body body)
  "Handle the request with BODY if TEST is true. Otherwise return
'NOT-HANDLED."
  `(cond
     (,test ,@body)
     (t 'not-handled)))

(defun cookie-out (name request)
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out request) :test #'string=)))

(defun response-header (name request)
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (response-headers request))))

(defun (setf response-header) (new-value name request)
  "Changes the current value of the outgoing http header named NAME
(a keyword or a string). If a header with this name doesn't exist, it
is created."
  (when (headers-sent-p request)
    (error "Can't set reply headers after headers have been sent."))

  (when (stringp name)
    (setf name (as-keyword name :destructivep nil)))

  (let ((entry (assoc name (response-headers request))))
    (if entry
        (setf (cdr entry) new-value)
        (push (cons name new-value) (response-headers request)))

    ;; Special case these two. This is kind of hinky, but we need to set
    ;; the slots if these are set since the slot values will be used in
    ;; finalize-response-headers to set the actual header. Note that
    ;; this relation is not directly symmetical. Setting the slot in the
    ;; object does not immediately set the value in the headers alist.
    ;; But it will eventually affect it in finalize-response-headers.
    (case name
      (:content-length
       (check-type new-value integer)
       (setf (content-length request) new-value))
      (:content-type
       (check-type new-value (or null string))
       (setf (content-type request) new-value)))


    new-value))

(defun send-headers (request &key
                     (content-type *default-content-type*)
                     (charset *default-charset*)
                     (status-code +http-ok+))
  "Send the headers and return a stream to which the body of the reply
can be written. If the content-type is text/* type, the stream
returned will be a character stream that will encode the response
properly for the charset specified."
  (setf (status-code request) status-code)
  (let ((stream (send-response-headers request nil content-type charset)))
    (if (text-type-p content-type)
        stream
        (make-flexi-stream stream :external-format (make-external-format charset)))))

(defun abort-request-handler (request response-status-code &optional body)
  "Abort the handling of a request, sending instead a response with
the given response-status-code. A request can only be aborted if
SEND-HEADERS has not been called."
  (setf (status-code request) response-status-code)
  (throw 'handler-done body))

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
Not found if the file does not exist. Also handles an
if-modified-since request appropriately."
  (when (or (not pathname)
            (wild-pathname-p pathname)
            (not (fad:file-exists-p pathname))
            (fad:directory-exists-p pathname))
    (abort-request-handler request +http-not-found+))

  (let ((time (or (file-write-date pathname) (get-universal-time))))

    ;; FIXME: do we really need to set the headers even if we might
    ;; send a not modified response? If not, let's check that first.
    (setf (response-header :last-modified request) (rfc-1123-date time))
    (setf (response-header :accept-ranges request) "bytes")

    (handle-if-modified-since time request)

    (with-open-file (file pathname :direction :input :element-type 'octet :if-does-not-exist nil)
      (let ((bytes-to-send (maybe-handle-range-header request file)))
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
  "Adds appropriate headers to completely prevent caching on most browsers."
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
  (let ((url
         (if (starts-with-scheme-p target) 
             target
             (let* ((requested-host (request-header :host request))
                    (current-protocol (if (ssl-config (acceptor request)) :https :http)))
               (format nil "~(~a~)://~a~@[:~a~]~a"
                       (or protocol current-protocol)
                       (or host (just-host requested-host))
                       (or port (just-port requested-host))
                       target)))))
    (setf (response-header :location request) url)
    (abort-request-handler request code)))

(defun just-host (host-and-port)
  (subseq host-and-port 0 (position #\: host-and-port)))

(defun just-port (host-and-port)
  (let ((colon (position #\: host-and-port)))
    (and colon (subseq host-and-port (1+ colon)))))

(defun require-authorization (request &optional (realm "Toot"))
  "Sends back appropriate headers to require basic HTTP
authentication (see RFC 2617) for the realm REALM."
  (setf (response-header :www-authenticate request)
        (format nil "Basic realm=\"~A\"" (quote-string realm)))
  (abort-request-handler request +http-authorization-required+))

(defun handle-if-modified-since (time request)
  "Handles the 'If-Modified-Since' header of REQUEST.  The date string
is compared to the one generated from the supplied universal time
TIME."
  (let ((if-modified-since (request-header :if-modified-since request))
        (time-string (rfc-1123-date time)))
    ;; simple string comparison is sufficient; see RFC 2616 14.25
    (when (and if-modified-since
               (equal if-modified-since time-string))
      (abort-request-handler request +http-not-modified+))
    (values)))

(defun enough-url (url url-prefix)
  "Returns the relative portion of URL relative to URL-PREFIX, similar
to what ENOUGH-NAMESTRING does for pathnames."
  (let ((prefix-length (length url-prefix)))
    (if (string= url url-prefix :end1 prefix-length)
        (subseq url prefix-length)
        url)))

(defun cookie-value (name request)
  "Get the value of the cookie with the given name sent by the client
or NIL if no such cookie was sent."
  (when-let (cookie (cdr (assoc name (cookies-in request) :test #'string=)))
    (slot-value cookie 'value)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal

(defun maybe-handle-range-header (request file)
  "Helper function for serve-file. Determines whether the requests
  specifies a Range header. If so, parses the header and position the
  already opened file to the location specified. Returns the number of
  bytes to transfer from the file. Invalid specified ranges are
  reported to the client with a HTTP 416 status code."
  (let ((bytes-available (file-length file)))
    (or
     (register-groups-bind (start end)
         ("^bytes (\\d+)-(\\d+)$" (request-header :range request) :sharedp t)
       ;; body won't be executed if regular expression does not match
       (setf start (parse-integer start))
       (setf end (parse-integer end))
       (when (or (< start 0) (>= end bytes-available))
         (setf (response-header :content-range request) (format nil "bytes 0-~D/*" (1- bytes-available)))
         (abort-request-handler request +http-requested-range-not-satisfiable+
                                (format nil "invalid request range (requested ~D-~D, accepted 0-~D)"
                                        start end (1- bytes-available))))
       (file-position file start)
       (setf (status-code request) +http-partial-content+)
       (setf (response-header :content-range request) (format nil "bytes ~D-~D/*" start end))
       (1+ (- end start)))
     bytes-available)))

(defun starts-with-scheme-p (string)
  "Checks whether the string STRING represents a URL which starts with
a scheme, i.e. something like 'https://' or 'mailto:'."
  (loop with scheme-char-seen-p = nil
     for c across string
     when (or (char-not-greaterp #\a c #\z)
              (digit-char-p c)
              (member c '(#\+ #\- #\.) :test #'char=))
     do (setq scheme-char-seen-p t)
     else return (and scheme-char-seen-p (char= c #\:))))
