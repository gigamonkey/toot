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

;; FIXME: if binary is nil, should we set external-format on the
;; request so the content-type will get adjusted.
(defun send-headers (request &key binary (external-format :utf-8))
  "Send the headers and return a stream to which the body of the reply can be written."
  (let ((stream (start-output request)))
    (cond
      (binary stream)
      (t (flexi-streams:make-flexi-stream stream :external-format external-format)))))


(defun serve-file (request pathname &optional content-type)
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
    (setf (content-type request) (or content-type (guess-mime-type (pathname-type pathname))))
    (setf (header-out :last-modified request) (rfc-1123-date time))
    (setf (header-out :accept-ranges request) "bytes")

    (handle-if-modified-since time request)

    (with-open-file (file pathname :direction :input :element-type 'octet :if-does-not-exist nil)
      (let ((bytes-to-send (maybe-handle-range-header request file)))
        (setf (content-length request) bytes-to-send)

        (let ((out (send-headers request :binary t))
              (buf (make-array +buffer-length+ :element-type 'octet)))
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

(defun enough-url (url url-prefix)
  "Returns the relative portion of URL relative to URL-PREFIX, similar
to what ENOUGH-NAMESTRING does for pathnames."
  (subseq url (or (mismatch url url-prefix) (length url-prefix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal

(defun maybe-handle-range-header (request file)
  "Helper function for serve-file. Determines whether the requests
  specifies a Range header. If so, parses the header and position the
  already opened file to the location specified. Returns the number of
  bytes to transfer from the file. Invalid specified ranges are
  reported to the client with a HTTP 416 status code."
  (let ((bytes-to-send (file-length file)))
    (cl-ppcre:register-groups-bind
        (start end)
        ("^bytes (\\d+)-(\\d+)$" (header-in :range request) :sharedp t)
      ;; body won't be executed if regular expression does not match
      (setf start (parse-integer start)
            end (parse-integer end))
      (when (or (< start 0)
                (>= end (file-length file)))
        (setf (header-out :content-range request) (format nil "bytes 0-~D/*" (1- (file-length file))))
        (abort-request-handler
         request
         +http-requested-range-not-satisfiable+
         (format nil "invalid request range (requested ~D-~D, accepted 0-~D)"
                 start end (1- (file-length file)))))
      (file-position file start)
      (setf (return-code request) +http-partial-content+
            bytes-to-send (1+ (- end start))
            (header-out :content-range request) (format nil "bytes ~D-~D/*" start end)))
    bytes-to-send))

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
