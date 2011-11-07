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

(defun (setf content-type) (new-value request)
  "Sets the outgoing 'Content-Type' http header of REQUEST."
  (setf (header-out :content-type request) new-value))

(defun (setf content-length) (new-value request)
  "Sets the outgoing 'Content-Length' http header of REQUEST."
  (setf (header-out :content-length request) new-value))

(defun header-out-set-p (name request)
  "Returns a true value if the outgoing http header named NAME has
been specified already.  NAME should be a keyword or a string."
  (assoc* name (headers-out request)))

(defun cookie-out (name request)
  "Returns the current value of the outgoing cookie named
NAME. Search is case-sensitive."
  (cdr (assoc name (cookies-out request) :test #'string=)))

(defun header-out (name request)
  "Returns the current value of the outgoing http header named NAME.
NAME should be a keyword or a string."
  (cdr (assoc name (headers-out request))))

(defun (setf header-out) (new-value name request)
  "Changes the current value of the outgoing http
header named NAME \(a keyword or a string).  If a header with this
name doesn't exist, it is created."
  (when (headers-sent-p request)
    (error "Can't set reply headers after headers have been sent."))

  (when (stringp name)
    (setf name (as-keyword name :destructivep nil)))

  (let ((entry (assoc name (headers-out request))))
    (if entry
        (setf (cdr entry) new-value)
        (setf (slot-value request 'headers-out)
              (acons name new-value (headers-out request))))
    new-value)

  (case name
    (:content-length
     (check-type new-value integer)
     (setf (slot-value request 'content-length) new-value))
    (:content-type
     (check-type new-value (or null string))
     (setf (slot-value request 'content-type) new-value))))

(defun send-headers (request &key
                     (content-type *default-content-type*)
                     (charset *default-charset*)
                     (status-code +http-ok+))
  "Send the headers and return a stream to which the body of the reply
can be written. If the content-type is text/* type, the stream
returned will be a character stream that will encode the response
properly for the charset specified."

  (setf (return-code request) status-code)
  (let ((stream (send-response-headers request nil content-type charset)))
    (if (text-type-p content-type)
        stream
        (make-flexi-stream stream :external-format (make-external-format charset)))))

(defun abort-request-handler (request response-status-code &optional body)
  "Abort the handling of a request, sending instead a response with
the given response-status-code. A request can only be aborted if
SEND-HEADERS has not been called."
  (setf (return-code request) response-status-code)
  (throw 'handler-done body))

(defun authorization (request)
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (header-in :authorization request))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (split ":" (base64:base64-string-to-string (subseq authorization start)))
        (values user password)))))

(defun host (request)
  "Returns the 'Host' incoming http header value."
  (header-in :host request))

(defun user-agent (request)
  "Returns the 'User-Agent' http header."
  (header-in :user-agent request))

(defun header-in (name request)
  "Returns the incoming header with name NAME. NAME can be a keyword
  \(recommended) or a string."
  (cdr (assoc* name (headers-in request))))

(defun cookie-in (name request)
  "Returns the cookie with the name NAME \(a string) as sent by the
browser - or NIL if there is none."
  (cdr (assoc name (cookies-in request) :test #'string=)))

(defun referer (request)
  "Returns the 'Referer' \(sic!) http header."
  (header-in :referer request))

(defun get-parameter (name request)
  "Returns the GET parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (get-parameters request) :test #'string=)))

(defun post-parameter (name request)
  "Returns the POST parameter with name NAME \(a string) - or NIL if
there is none.  Search is case-sensitive."
  (cdr (assoc name (post-parameters request) :test #'string=)))

(defun parameter (name request)
  "Returns the GET or the POST parameter with name NAME \(a string) -
or NIL if there is none.  If both a GET and a POST parameter with the
same name exist the GET parameter is returned.  Search is
case-sensitive."
  (or (get-parameter name request) (post-parameter name request)))


(defun real-remote-addr (request)
  "Returns the 'X-Forwarded-For' incoming http header as the
second value in the form of a list of IP addresses and the first
element of this list as the first value if this header exists.
Otherwise returns the value of REMOTE-ADDR as the only value."
  (let ((x-forwarded-for (header-in :x-forwarded-for request)))
    (cond (x-forwarded-for (let ((addresses (split "\\s*,\\s*" x-forwarded-for)))
                             (values (first addresses) addresses)))
          (t (remote-addr request)))))

;; FIXME: the caller needs to know the charset of the file. Not sure
;; if there's anything better to do.
(defun serve-file (request pathname &optional content-type (charset *default-charset*))
  "Serve the file denoted by PATHNAME. Sends a content type header
corresponding to CONTENT-TYPE or \(if that is NIL) tries to determine
the content type via the file's suffix. Aborts the request with 404:
Not found if the file does not exist. Also handles an
if-modified-since request appropriately."
  (when (or (wild-pathname-p pathname)
            (not (fad:file-exists-p pathname))
            (fad:directory-exists-p pathname))
    (abort-request-handler request +http-not-found+))

  (let ((time (or (file-write-date pathname) (get-universal-time))))

    ;; FIXME: do we really need to set the headers even if we might
    ;; send a not modified response? If not, let's check that first.
    (setf (header-out :last-modified request) (rfc-1123-date time))
    (setf (header-out :accept-ranges request) "bytes")

    (handle-if-modified-since time request)

    (with-open-file (file pathname :direction :input :element-type 'octet :if-does-not-exist nil)
      (let ((bytes-to-send (maybe-handle-range-header request file)))
        (setf (return-code request) +http-ok+)
        (let ((out (send-response-headers request
                                          bytes-to-send 
                                          (or content-type (guess-mime-type (pathname-type pathname)))
                                          charset))
              (buf (make-array +buffer-length+ :element-type 'octet)))
          ;; FIXME: is this necessary? We shouldn't have a
          ;; flexi-stream at this point. In fact, this should probably
          ;; blow up because of that.
          #+:clisp
          (setf (flexi-stream-element-type (content-stream (acceptor request))) 'octet)
          (loop
             (when (zerop bytes-to-send)
               (return))
             (let ((chunk-size (min +buffer-length+ bytes-to-send)))
               (unless (eql chunk-size (read-sequence buf file :end chunk-size))
                 (error "can't read from input file"))
               (write-sequence buf out :end chunk-size)
               (decf bytes-to-send chunk-size)))
          (finish-output out))))))

(defun no-cache (request)
  "Adds appropriate headers to completely prevent caching on most browsers."
  (setf
   ;; WTF is this date?! (Some cargo cult thing from PHP or maybe MSDN, it seems.)
   (header-out :expires request) "Mon, 26 Jul 1997 05:00:00 GMT"
   (header-out :cache-control request) "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
   (header-out :pragma request) "no-cache"
   (header-out :last-modified request) (rfc-1123-date))
  (values))

(defun redirect (request target &key
                 (host (host request))
                 port
                 (protocol (server-protocol request))
                 (code +http-moved-temporarily+))
  "Redirects the browser to TARGET which should be a string. If TARGET
is a full URL starting with a scheme, HOST, PORT and PROTOCOL are
ignored. Otherwise, TARGET should denote the path part of a URL,
PROTOCOL must be one of the keywords :HTTP or :HTTPS, and the URL to
redirect to will be constructed from HOST, PORT, PROTOCOL, and TARGET.
If CODE is a 3xx redirection code, it will be sent as status code."
  (check-type code (integer 300 399))
  (let ((url (if (starts-with-scheme-p target)
               target
               (format nil "~A://~A~@[:~A~]~A"
                       (ecase protocol
                         ((:http) "http")
                         ((:https) "https"))
                       (if port
                         (first (ppcre:split ":" (or host "")))
                         host)
                       port target))))
    (setf (header-out :location request) url)
    (abort-request-handler request code)))

(defun require-authorization (request &optional (realm "Toot"))
  "Sends back appropriate headers to require basic HTTP authentication
\(see RFC 2617) for the realm REALM."
  (setf (header-out :www-authenticate request)
        (format nil "Basic realm=\"~A\"" (quote-string realm)))
  (abort-request-handler request +http-authorization-required+))

(defun handle-if-modified-since (time request)
  "Handles the 'If-Modified-Since' header of REQUEST.  The date string
is compared to the one generated from the supplied universal time
TIME."
  (let ((if-modified-since (header-in :if-modified-since request))
        (time-string (rfc-1123-date time)))
    ;; simple string comparison is sufficient; see RFC 2616 14.25
    (when (and if-modified-since
               (equal if-modified-since time-string))
      (abort-request-handler request +http-not-modified+))
    (values)))

;; FIXME: (enough-url "/foo/bar/baz.html" "/www/") =>
;; "foo/bar/baz.html" which is probably not right.
(defun enough-url (url url-prefix)
  "Returns the relative portion of URL relative to URL-PREFIX, similar
to what ENOUGH-NAMESTRING does for pathnames."
  (subseq url (or (mismatch url url-prefix) (length url-prefix))))

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
     (cl-ppcre:register-groups-bind (start end)
        ("^bytes (\\d+)-(\\d+)$" (header-in :range request) :sharedp t)
      ;; body won't be executed if regular expression does not match
      (setf start (parse-integer start))
      (setf end (parse-integer end))
      (when (or (< start 0) (>= end bytes-available))
        (setf (header-out :content-range request) (format nil "bytes 0-~D/*" (1- bytes-available)))
        (abort-request-handler request +http-requested-range-not-satisfiable+
                               (format nil "invalid request range (requested ~D-~D, accepted 0-~D)"
                                       start end (1- bytes-available))))
      (file-position file start)
      (setf (return-code request) +http-partial-content+)
      (setf (header-out :content-range request) (format nil "bytes ~D-~D/*" start end))
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
        else return (and scheme-char-seen-p
                         (char= c #\:))))
