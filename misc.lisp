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

(defun create-prefix-dispatcher (prefix handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
starts with the string PREFIX."
  (lambda (request)
    (let ((mismatch (mismatch (script-name request) prefix
                              :test #'char=)))
      (and (or (null mismatch)
               (>= mismatch (length prefix)))
           handler))))

(defun create-regex-dispatcher (regex handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches the CL-PPCRE regular expression REGEX."
  (let ((scanner (create-scanner regex)))
    (lambda (request)
      (and (scan scanner (script-name request))
           handler))))

(defun abort-request-handler (request response-status-code)
  "This function can be called by a request handler at any time to
immediately abort handling the request.  This works as if the handler
had returned RESULT.  See the source code of REDIRECT for an example."
  (setf (return-code (reply request)) response-status-code)
  (throw 'handler-done nil))

(defun maybe-handle-range-header (request reply file)
  "Helper function for handle-static-file.  Determines whether the
  requests specifies a Range header.  If so, parses the header and
  position the already opened file to the location specified.  Returns
  the number of bytes to transfer from the file.  Invalid specified
  ranges are reported to the client with a HTTP 416 status code."
  (let ((bytes-to-send (file-length file)))
    (cl-ppcre:register-groups-bind
        (start end)
        ("^bytes (\\d+)-(\\d+)$" (header-in :range request) :sharedp t)
      ;; body won't be executed if regular expression does not match
      (setf start (parse-integer start)
            end (parse-integer end))
      (when (or (< start 0)
                (>= end (file-length file)))
        (setf (return-code reply) +http-requested-range-not-satisfiable+
              (header-out :content-range reply) (format nil "bytes 0-~D/*" (1- (file-length file))))
        (throw 'handler-done
          (format nil "invalid request range (requested ~D-~D, accepted 0-~D)"
                  start end (1- (file-length file)))))
      (file-position file start)
      (setf (return-code reply) +http-partial-content+
            bytes-to-send (1+ (- end start))
            (header-out :content-range reply) (format nil "bytes ~D-~D/*" start end)))
    bytes-to-send))

(defun handle-static-file (request reply pathname &optional content-type)
  "A function which acts like a Toot handler for the file
denoted by PATHNAME.  Sends a content type header corresponding to
CONTENT-TYPE or \(if that is NIL) tries to determine the content type
via the file's suffix."
  (when (or (wild-pathname-p pathname)
            (not (fad:file-exists-p pathname))
            (fad:directory-exists-p pathname))
    (abort-request-handler request +http-not-found+))

  (let ((time (or (file-write-date pathname)
                  (get-universal-time)))
        bytes-to-send)
    (setf (content-type reply) (or content-type
                                   (mime-type pathname)
                                   "application/octet-stream")
          (header-out :last-modified reply) (rfc-1123-date time)
          (header-out :accept-ranges reply) "bytes")
    (handle-if-modified-since time request)
    (with-open-file (file pathname
                          :direction :input
                          :element-type 'octet
                          :if-does-not-exist nil)
      (setf bytes-to-send (maybe-handle-range-header request reply file))
      (setf (content-length reply) bytes-to-send)

      (let ((out (send-headers request))
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
        (finish-output out)))))

(defun create-static-file-dispatcher-and-handler (uri path &optional content-type)
  "Creates and returns a request dispatch function which will dispatch
to a handler function which emits the file denoted by the pathname
designator PATH with content type CONTENT-TYPE if the SCRIPT-NAME of
the request matches the string URI.  If CONTENT-TYPE is NIL, tries to
determine the content type via the file's suffix."
  ;; the dispatcher
  (lambda (request)
    (when (equal (script-name request) uri)
      ;; the handler
      (lambda (request reply)
        (declare (ignore request))
        (handle-static-file reply path content-type)))))

(defun enough-url (url url-prefix)
  "Returns the relative portion of URL relative to URL-PREFIX, similar
to what ENOUGH-NAMESTRING does for pathnames."
  (subseq url (or (mismatch url url-prefix) (length url-prefix))))

(defun create-folder-dispatcher-and-handler (uri-prefix base-path &optional content-type)
  "Creates and returns a dispatch function which will dispatch to a
handler function which emits the file relative to BASE-PATH that is
denoted by the URI of the request relative to URI-PREFIX.  URI-PREFIX
must be a string ending with a slash, BASE-PATH must be a pathname
designator for an existing directory.  If CONTENT-TYPE is not NIL,
it'll be the content type used for all files in the folder."
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (parameter-error "~S must be string ending with a slash." uri-prefix))
  (let ((name (pathname-name base-path))
        (type (pathname-type base-path)))
    (when (or (and name (not (eq name :unspecific)))
              (and type (not (eq type :unspecific))))
      (parameter-error "~S is supposed to denote a directory." base-path)))
  (flet ((handler (request reply)
           (let* ((script-name (url-decode (script-name request)))
                  (script-path (enough-url (regex-replace-all "\\\\" script-name "/")
                                           uri-prefix))
                  (script-path-directory (pathname-directory script-path)))
             (unless (or (stringp script-path-directory)
                         (null script-path-directory)
                         (and (listp script-path-directory)
                              (eq (first script-path-directory) :relative)
                              (loop for component in (rest script-path-directory)
                                    always (stringp component))))
               (abort-request-handler request +http-forbidden+))
             (handle-static-file reply (merge-pathnames script-path base-path) content-type))))
    (create-prefix-dispatcher uri-prefix #'handler)))

(defun no-cache (reply)
  "Adds appropriate headers to completely prevent caching on most browsers."
  (setf
   (header-out :expires reply) "Mon, 26 Jul 1997 05:00:00 GMT"
   (header-out :cache-control reply) "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
   (header-out :pragma reply) "no-cache"
   (header-out :last-modified reply) (rfc-1123-date))
  (values))

(defun redirect (request reply target &key (host (host request))
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
    (setf (header-out :location reply) url)
    (abort-request-handler request code)))

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

(defun require-authorization (request &optional (realm "Toot"))
  "Sends back appropriate headers to require basic HTTP authentication
\(see RFC 2617) for the realm REALM."
  (setf (header-out :www-authenticate (reply request))
        (format nil "Basic realm=\"~A\"" (quote-string realm)))
  (abort-request-handler request +http-authorization-required+))
