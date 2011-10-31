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

(in-package :toot)

(defun maybe-add-charset-to-content-type-header (content-type external-format)
  (if (and (cl-ppcre:scan "(?i)^text" content-type)
           (not (cl-ppcre:scan "(?i);\\s*charset=" content-type)))
      (format nil "~A; charset=~(~A~)" content-type (flex:external-format-name external-format))
      content-type))

(defun start-output (request &optional (content nil content-provided-p))
  (let* ((return-code (return-code request))
         (acceptor (acceptor request))
         (chunkedp (and (output-chunking-p acceptor)
                        (eql (server-protocol request) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (content-length request) content-provided-p))))
         (head-request-p (eql (request-method request) :head)))

    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (keep-alive-p request)
      (when keep-alive-p
        (setf keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (or if there
              ;; is no content)
              (or chunkedp
                  head-request-p
                  (eql (return-code request) +http-not-modified+)
                  (content-length request)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (header-out :transfer-encoding request) "chunked"))

      (cond 
        (keep-alive-p
         (setf (close-stream-p request) nil)
         (when (and (read-timeout acceptor)
                    (or (not (eql (server-protocol request) :http/1.1))
                        keep-alive-requested-p))
           ;; persistent connections are implicitly assumed for
           ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
           ;; client has explicitly asked for one
           (setf (header-out :connection request) "Keep-Alive")
           (setf (header-out :keep-alive request) (format nil "timeout=~D" (read-timeout acceptor)))))

        (t (setf (header-out :connection request) "Close"))))

    (unless (and (header-out-set-p :server request) 
                 (null (header-out :server request)))
      (setf (header-out :server request) (or (header-out :server request)
                                             (format nil "Toot ~A" *toot-version*))))

    (setf (header-out :date request) (rfc-1123-date))

    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (string-to-octets content :external-format (external-format request)))
      (setf (content-type request) (maybe-add-charset-to-content-type-header 
                                    (content-type request)
                                    (external-format request))))

    ;; FIXME: this should probably be within the preceeding WHEN.
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the Content-Length header properly; maybe the user specified
      ;; a different content length, but that will be wrong anyway
      (setf (header-out :content-length request) (length content)))


    ;; If the handler calls send-headers, then we will get here with
    ;; no content and need to send the headers. Then, after the
    ;; handler returns, the acceptor will call start-output again and
    ;; we'll end up here with still no content but with the headers
    ;; sent, in which case we do nothing (and the return value is
    ;; ignored).
    ;;
    ;; If, however, the handler didn't call send-headers, then
    ;; start-output will be called for the first time from
    ;; process-request after the handler returns with (presumably) a
    ;; non-nil content, in which case we need to send the headers and
    ;; then send the content.
    ;;
    ;; Finally, there's the wrinkle that either of those scenarios
    ;; could also occur in response to a HEAD request in which case we
    ;; want to skip sending the content anyway.
    (unless (headers-sent-p request)
      (let ((stream (content-stream request)))

        (when content
          (unless head-request-p
            (setf (content-length request) (length content))))
        
        (%send-headers stream request return-code)

        (when content
          (unless head-request-p
            (write-sequence content stream)
            (finish-output stream)))
      
        ;; when processing a HEAD request, exit to return from
        ;; PROCESS-REQUEST. In the case of a handler that calls
        ;; send-header this will cause the handler to exit suddenly,
        ;; before it outputs any content.
        (when head-request-p (throw 'request-processed nil))
        
        ;;; This case is mostly (only?) interesting in the
        ;;; send-headers case (and perhaps should be moved there)
        ;;; since if we were called with content, it was already
        ;;; written and the request has been processed and after
        ;;; process-request returns the request whose content-stream
        ;;; we are setting here will become garbage.
        (when chunkedp
          ;; turn chunking on after the headers have been sent
          (unless (typep (content-stream request) 'chunked-stream)
            (setf (content-stream request) (make-chunked-stream (content-stream request))))
          (setf (chunked-stream-output-chunking-p (content-stream request)) t))
      
        (content-stream request)))))

(defun %send-headers (stream request status-code)
  (log-access (access-logger (acceptor request)) request)

  ;; Read post data to clear stream - Force binary mode to avoid
  ;; OCTETS-TO-STRING overhead.
  (raw-post-data request :force-binary t)

  (let ((header-stream (make-header-stream stream)))
    (write-status-line header-stream status-code)
    (write-headers header-stream (headers-out request))
    (write-cookies header-stream (cookies-out request))
    (format header-stream "~C~C" #\Return #\Linefeed))

  (setf (headers-sent-p request) t))

(defun quick-send-response (stream status-code)
  "Send a response to the client before we've created a request
object. At the moment used only to send a service unavailable
response. This happens so early that we can't even log it to the
access log. Which is perhaps unfortunate."
  ;; We don't read post data to clear stream since we're just going to
  ;; close it anyway.
  (with-open-stream (stream (make-header-stream stream))
    (write-status-line stream status-code)
    (write-headers stream '((:content . 0)))
    (format stream "~C~C" #\Return #\Linefeed)))

(defun make-header-stream (stream)
  (let* ((client-header-stream (flex:make-flexi-stream stream :external-format :iso-8859-1)))
    (if *header-stream*
        (make-broadcast-stream *header-stream* client-header-stream)
        client-header-stream)))

(defun write-status-line (stream status-code)
  (format stream "HTTP/1.1 ~D ~A~C~C" status-code (reason-phrase status-code) #\Return #\Linefeed))

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

(defun read-initial-request-line (stream)
  (handler-case
      (let ((*current-error-message* "While reading initial request line:"))
        (usocket:with-mapped-conditions ()
          (read-line* stream)))
    ((or end-of-file usocket:timeout-error) ())))
  
(defun printable-ascii-char-p (char)
  (<= 32 (char-code char) 126))

(defun send-bad-request-response (stream &optional additional-info)
  (write-sequence 
   (flex:string-to-octets
    (format nil "HTTP/1.0 ~D ~A~C~CConnection: close~C~C~C~CYour request could not be interpreted by this HTTP server~C~C~@[~A~]~C~C"
            +http-bad-request+ (reason-phrase +http-bad-request+) #\Return #\Linefeed
            #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed additional-info #\Return #\Linefeed))
   stream))
