;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

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

(in-package :hunchentoot)

(defun write-header-line (key value stream)
  (let ((string (princ-to-string value)))
    (write-string key stream)
    (write-char #\: stream)
    (write-char #\Space stream)
    (let ((start 0))
      (loop
         (let ((end (or (position #\Newline string :start start)
                        (length string))))
           ;; skip empty lines, as they confuse certain HTTP clients
           (unless (eql start end)
             (unless (zerop start)
               (write-char #\Tab stream))
             (write-string string stream :start start :end end)
             (write-char #\Return stream)
             (write-char #\Linefeed stream))
           (setf start (1+ end))
           (when (<= (length string) start)
             (return)))))))

(defun maybe-add-charset-to-content-type-header (content-type external-format)
  (if (and (cl-ppcre:scan "(?i)^text" content-type)
           (not (cl-ppcre:scan "(?i);\\s*charset=" content-type)))
      (format nil "~A; charset=~(~A~)" content-type (flex:external-format-name external-format))
      content-type))

(defun start-output (request reply return-code &optional (content nil content-provided-p))
  (let* ((acceptor (acceptor request))
         (chunkedp (and (acceptor-output-chunking-p acceptor)
                        (eq (server-protocol request) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length reply) content-provided-p))))
         (request-method (request-method request))
         (head-request-p (eq request-method :head)))

    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (keep-alive-p request)
      (when keep-alive-p
        (setf keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (or if there
              ;; is no content)
              (or chunkedp
                  head-request-p
                  (eql (return-code reply) +http-not-modified+)
                  (content-length reply)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (header-out :transfer-encoding reply) "chunked"))
      (cond (keep-alive-p
             (setf *close-hunchentoot-stream* nil)
             (when (and (acceptor-read-timeout acceptor)
                        (or (not (eq (server-protocol request) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (setf (header-out :connection reply) "Keep-Alive"
                     (header-out :keep-alive reply)
                     (format nil "timeout=~D" (acceptor-read-timeout acceptor)))))
            (t (setf (header-out :connection reply) "Close"))))
    (unless (and (header-out-set-p :server reply)
                 (null (header-out :server reply)))
      (setf (header-out :server reply) (or (header-out :server reply)
                                     (acceptor-server-name acceptor))))
    (setf (header-out :date reply) (rfc-1123-date))

    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (string-to-octets content :external-format (reply-external-format reply))
            (content-type reply) (maybe-add-charset-to-content-type-header 
                                  (content-type reply)
                                  (reply-external-format reply))))
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the Content-Length header properly; maybe the user specified
      ;; a different content length, but that will wrong anyway
      (setf (header-out :content-length reply) (length content)))
    ;; send headers only once
    (when *headers-sent*
      (return-from start-output))
    (setf *headers-sent* t)
    (send-response 
     request
     (content-stream request)
     return-code
     :headers (headers-out reply)
     :cookies (cookies-out reply)
     :content (unless head-request-p content))
    ;; when processing a HEAD request, exit to return from PROCESS-REQUEST
    (when head-request-p
      (throw 'request-processed nil))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (unless (typep (content-stream request) 'chunked-stream)
        (setf (content-stream request) (make-chunked-stream (content-stream request))))
      (setf (chunked-stream-output-chunking-p (content-stream request)) t))
    (content-stream request)))

(defun send-response (request stream status-code &key headers cookies content)
  (let ((reply (reply request)))
    (when content
      (setf (content-length reply) (length content)))
    (when (content-length reply)
      (if (assoc :content-length headers)
          (setf (cdr (assoc :content-length headers)) (content-length reply))
          (push (cons :content-length (content-length reply)) headers)))
    ;; access log message
    (acceptor-log-access request :return-code status-code)
    ;; Read post data to clear stream - Force binary mode to avoid OCTETS-TO-STRING overhead.
    (raw-post-data request :force-binary t)
    (let* ((client-header-stream (flex:make-flexi-stream stream :external-format :iso-8859-1))
           (header-stream (if *header-stream*
                              (make-broadcast-stream *header-stream* client-header-stream)
                              client-header-stream)))
      ;; start with status line
      (format header-stream "HTTP/1.1 ~D ~A~C~C" status-code (reason-phrase status-code) #\Return #\Linefeed)
      ;; write all headers from the REPLY object
      (loop for (key . value) in headers
         when value
         do (write-header-line (as-capitalized-string key) value header-stream))
      ;; now the cookies
      (loop for (nil . cookie) in cookies
         do (write-header-line "Set-Cookie" (stringify-cookie cookie) header-stream))
      (format header-stream "~C~C" #\Return #\Linefeed))
    ;; now optional content
    (when content
      (write-sequence content stream)
      (finish-output stream))
    stream))

(defun send-headers (request reply)
  (start-output request reply (return-code reply)))

(defun read-initial-request-line (stream)
  (handler-case
      (let ((*current-error-message* "While reading initial request line:"))
        (with-mapped-conditions ()
          (read-line* stream)))
    ((or end-of-file usocket:timeout-error) ())))

(defun send-bad-request-response (stream &optional additional-info)
  (write-sequence (flex:string-to-octets
                   (format nil "HTTP/1.0 ~D ~A~C~CConnection: close~C~C~C~CYour request could not be interpreted by this HTTP server~C~C~@[~A~]~C~C"
                           +http-bad-request+ (reason-phrase +http-bad-request+) #\Return #\Linefeed
                           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed additional-info #\Return #\Linefeed))
                  stream))

(defun printable-ascii-char-p (char)
  (<= 32 (char-code char) 126))
  
(defun get-request-data (stream)
  "Reads incoming headers from the client via STREAM.  Returns as
multiple values the headers as an alist, the method, the URI, and the
protocol of the request."
  (with-character-stream-semantics
   (let ((first-line (read-initial-request-line stream)))
     (when first-line
       (unless (every #'printable-ascii-char-p first-line)
         (send-bad-request-response stream "Non-ASCII character in request line")
         (return-from get-request-data nil))
       (destructuring-bind (&optional method url-string protocol)
           (split "\\s+" first-line :limit 3)
         (unless url-string
           (send-bad-request-response stream)
           (return-from get-request-data nil))
         (when *header-stream*
           (format *header-stream* "~A~%" first-line))
         (let ((headers (and protocol (read-http-headers stream *header-stream*))))
           (unless protocol (setf protocol "HTTP/0.9"))
           ;; maybe handle 'Expect: 100-continue' header
           (when-let (expectations (cdr (assoc* :expect headers)))
             (when (member "100-continue" (split "\\s*,\\s*" expectations) :test #'equalp)
               ;; according to 14.20 in the RFC - we should actually
               ;; check if we have to respond with 417 here
               (let ((continue-line
                      (format nil "HTTP/1.1 ~D ~A"
                              +http-continue+
                              (reason-phrase +http-continue+))))
                 (write-sequence (map 'list #'char-code continue-line) stream)
                 (write-sequence +crlf+ stream)
                 (write-sequence +crlf+ stream)
                 (force-output stream)
                 (when *header-stream*
                   (format *header-stream* "~A~%" continue-line)))))
           (values headers
                   (as-keyword method)
                   url-string
                   (as-keyword (trim-whitespace protocol)))))))))
