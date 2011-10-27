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

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun default-document-directory (&optional sub-directory)
    (asdf:system-relative-pathname :hunchentoot (format nil "www/~@[~A~]" sub-directory))))

(defclass acceptor ()
  ((port :initarg :port :reader acceptor-port)
   (address :initarg :address :reader acceptor-address)
   (name :initarg :name :accessor acceptor-name)
   (taskmaster :initarg :taskmaster :reader acceptor-taskmaster)
   (output-chunking-p :initarg :output-chunking-p :accessor acceptor-output-chunking-p)
   (input-chunking-p :initarg :input-chunking-p :accessor acceptor-input-chunking-p)
   (persistent-connections-p :initarg :persistent-connections-p :accessor acceptor-persistent-connections-p)
   (read-timeout :initarg :read-timeout :reader acceptor-read-timeout)
   (write-timeout :initarg :write-timeout :reader acceptor-write-timeout)
   (listen-socket :initform nil :accessor acceptor-listen-socket)
   (listen-backlog :initarg :listen-backlog :reader acceptor-listen-backlog)
   (acceptor-shutdown-p :initform t :accessor acceptor-shutdown-p)
   (requests-in-progress :initform 0 :accessor accessor-requests-in-progress)
   (shutdown-queue :initform (make-condition-variable) :accessor acceptor-shutdown-queue)
   (shutdown-lock :initform (make-lock "hunchentoot-acceptor-shutdown") :accessor acceptor-shutdown-lock)
   (access-log-destination :initarg :access-log-destination :accessor acceptor-access-log-destination)
   (message-log-destination :initarg :message-log-destination :accessor acceptor-message-log-destination)
   (error-template-directory :initarg :error-template-directory :accessor acceptor-error-template-directory)
   (document-root :initarg :document-root :accessor acceptor-document-root)
   (ssl-adapter :initarg :ssl-adapter :accessor ssl-adapter))

  (:default-initargs
    :address nil
    :port 80
    :name (gensym)
    :listen-backlog 50
    :taskmaster (make-instance *default-taskmaster-class*)
    :output-chunking-p t
    :input-chunking-p t
    :persistent-connections-p t
    :read-timeout *default-connection-timeout*
    :write-timeout *default-connection-timeout*
    :access-log-destination *error-output*
    :message-log-destination *error-output*
    :document-root (load-time-value (default-document-directory))
    :ssl-adapter nil
    :error-template-directory (load-time-value (default-document-directory "errors/"))))

(defmethod print-object ((acceptor acceptor) stream)
  (print-unreadable-object (acceptor stream :type t)
    (format stream "\(host ~A, port ~A)"
            (or (acceptor-address acceptor) "*") (acceptor-port acceptor))))

;; SSL

(defclass ssl-adapter ()
  ((ssl-certificate-file :initarg :ssl-certificate-file :reader ssl-certificate-file)
   (ssl-private-key-file :initarg :ssl-private-key-file :reader ssl-private-key-file)
   (ssl-private-key-password :initform nil :initarg :ssl-private-key-password :reader ssl-private-key-password)))

(defmethod initialize-instance :after ((adapter ssl-adapter) &key &allow-other-keys)
  ;; OpenSSL doesn't know much about Lisp pathnames...
  (with-slots (ssl-private-key-file ssl-certificate-file) adapter
    (setf ssl-private-key-file (namestring (truename ssl-private-key-file)))
    (setf ssl-certificate-file (namestring (truename ssl-certificate-file)))))

(defun setup-ssl-stream (adapter stream)
  ;; attach SSL to the stream if necessary
  (with-slots (ssl-certificate-file ssl-private-key-file ssl-private-key-password) adapter
    (cl+ssl:make-ssl-server-stream 
     stream
     :certificate ssl-certificate-file
     :key ssl-private-key-file
    :password ssl-private-key-password)))

(defun start (acceptor)
  (setf (acceptor-shutdown-p acceptor) nil)
  (start-listening acceptor)
  (let ((taskmaster (acceptor-taskmaster acceptor)))
    (setf (taskmaster-acceptor taskmaster) acceptor)
    (execute-acceptor taskmaster))
  acceptor)

(defun stop (acceptor &key soft)
  (setf (acceptor-shutdown-p acceptor) t)
  (shutdown (acceptor-taskmaster acceptor))
  (when soft
    (with-lock-held ((acceptor-shutdown-lock acceptor))
      (when (plusp (accessor-requests-in-progress acceptor))
        (condition-variable-wait (acceptor-shutdown-queue acceptor) 
                                 (acceptor-shutdown-lock acceptor)))))
  (usocket:socket-close (acceptor-listen-socket acceptor))
  (setf (acceptor-listen-socket acceptor) nil)
  acceptor)

(defun reset-connection-stream (acceptor stream)
  (declare (ignore acceptor))
  ;; turn chunking off at this point
  (cond ((typep stream 'chunked-stream)
         ;; flush the stream first and check if there's unread input
         ;; which would be an error
         (setf (chunked-stream-output-chunking-p stream) nil
               (chunked-stream-input-chunking-p stream) nil)
         ;; switch back to bare socket stream
         (chunked-stream-stream stream))
        (t stream)))

(defun do-with-acceptor-request-count-incremented (acceptor function)
  (with-lock-held ((acceptor-shutdown-lock acceptor))
    (incf (accessor-requests-in-progress acceptor)))
  (unwind-protect
       (funcall function)
    (with-lock-held ((acceptor-shutdown-lock acceptor))
      (decf (accessor-requests-in-progress acceptor))
      (when (acceptor-shutdown-p acceptor)
        (condition-variable-signal (acceptor-shutdown-queue acceptor))))))

(defmacro with-acceptor-request-count-incremented ((acceptor) &body body)
  "Execute BODY with ACCEPTOR-REQUESTS-IN-PROGRESS of ACCEPTOR
  incremented by one.  If the ACCEPTOR-SHUTDOWN-P returns true after
  the BODY has been executed, the ACCEPTOR-SHUTDOWN-QUEUE condition
  variable of the ACCEPTOR is signalled in order to finish shutdown
  processing."
  `(do-with-acceptor-request-count-incremented ,acceptor (lambda () ,@body)))

(defun process-connection (acceptor socket)
  (handler-bind ((error
                  ;; abort if there's an error which isn't caught inside
                  (lambda (cond)
                    (acceptor-log-message acceptor *lisp-errors-log-level* "Error while processing connection: ~A" cond)
                    (return-from process-connection)))
                 (warning
                  ;; log all warnings which aren't caught inside
                  (lambda (cond)
                    (acceptor-log-message acceptor *lisp-warnings-log-level* "Warning while processing connection: ~A" cond))))
    (with-mapped-conditions ()
      (let ((content-stream (make-socket-stream socket acceptor)))
        (unwind-protect
             ;; process requests until either the acceptor is shut
             ;; down, close-stream-p on the reply is T, or the peer
             ;; fails to send a request
             (loop
                (when (acceptor-shutdown-p acceptor)
                  (return))
                (multiple-value-bind (headers-in method url-string protocol)
                    (get-request-data content-stream)
                  ;; check if there was a request at all
                  (unless method (return))
                  (let ((reply (make-instance 'reply :acceptor acceptor))
                        (transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))

                    (when transfer-encodings
                      (setf transfer-encodings
                            (split "\\s*,\\s*" transfer-encodings))

                      (when (member "chunked" transfer-encodings :test #'equalp)
                        (cond ((acceptor-input-chunking-p acceptor)
                               ;; turn chunking on before we read the request body
                               (setf content-stream (make-chunked-stream content-stream)
                                     (chunked-stream-input-chunking-p content-stream) t))
                              (t (hunchentoot-error "Client tried to use ~
chunked encoding, but acceptor is configured to not use it.")))))

                    (multiple-value-bind (remote-addr remote-port)
                        (get-peer-address-and-port socket)
                      (with-acceptor-request-count-incremented (acceptor)
                        (process-request (make-instance 'request
                                           :acceptor acceptor
                                           :reply reply
                                           :remote-addr remote-addr
                                           :remote-port remote-port
                                           :headers-in headers-in
                                           :content-stream content-stream
                                           :method method
                                           :uri url-string
                                           :server-protocol protocol)
                                         reply)))
                    (force-output content-stream)
                    (setf content-stream (reset-connection-stream acceptor content-stream))
                    (when (close-stream-p reply) (return)))))

          (when content-stream
            ;; as we are at the end of the request here, we ignore all
            ;; errors that may occur while flushing and/or closing the
            ;; stream.
            (ignore-errors* (force-output content-stream))
            (ignore-errors* (close content-stream :abort t))))))))

(defun acceptor-log-access (request &key return-code)
  (let ((acceptor (acceptor request))
        (reply (reply request)))
    (with-log-stream (stream (acceptor-access-log-destination acceptor) *access-log-lock*)
      (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
              (remote-addr request)
              (header-in :x-forwarded-for request)
              (authorization request)
              (iso-time)
              (request-method request)
              (script-name request)
              (query-string request)
              (server-protocol request)
              return-code
              (content-length reply)
              (referer request)
              (user-agent request)))))

(defun acceptor-log-message (acceptor log-level format-string &rest format-arguments)
  (with-log-stream (stream (acceptor-message-log-destination acceptor) *message-log-lock*)
    (format stream "[~A~@[ [~A]~]] ~?~%"
            (iso-time) log-level
            format-string format-arguments)))

;; usocket implementation

(defun start-listening (acceptor)
  (when (acceptor-listen-socket acceptor)
    (hunchentoot-error "acceptor ~A is already listening" acceptor))
  (setf (acceptor-listen-socket acceptor)
        (usocket:socket-listen (or (acceptor-address acceptor)
                                   usocket:*wildcard-host*)
                               (acceptor-port acceptor)
                               :reuseaddress t
			       :backlog (acceptor-listen-backlog acceptor)
                               :element-type '(unsigned-byte 8)))
  (values))

(defun accept-connections (acceptor)
  (usocket:with-server-socket (listener (acceptor-listen-socket acceptor))
    (loop
     (when (acceptor-shutdown-p acceptor)
       (return))
     (when (usocket:wait-for-input listener :ready-only t :timeout +new-connection-wait-time+)
       (when-let (client-connection
                  (handler-case (usocket:socket-accept listener)                               
                    ;; ignore condition
                    (usocket:connection-aborted-error ())))
         (set-timeouts client-connection
                       (acceptor-read-timeout acceptor)
                       (acceptor-write-timeout acceptor))
         (handle-incoming-connection (acceptor-taskmaster acceptor)
                                     client-connection))))))


(defun acceptor-dispatch-request (acceptor request reply)
  (cond
    ((acceptor-document-root acceptor)
     (handle-static-file request reply 
                         (merge-pathnames (if (equal (script-name request) "/")
                                              "index.html"
                                              (subseq (script-name request) 1))
                                          (acceptor-document-root acceptor))))
    (t (setf (return-code reply) +http-not-found+))))

(defun handle-request (acceptor request reply)
  (handler-bind 
      ((error
        (lambda (cond)
          ;; if the headers were already sent, the error happened
          ;; within the body and we have to close the stream
          (when (headers-sent-p reply) (setf (close-stream-p reply) t))
          (throw 'handler-done (values nil cond (get-backtrace)))))
       (warning
        (lambda (cond)
          (when *log-lisp-warnings-p*
            (acceptor-log-message acceptor *lisp-warnings-log-level* "~A" cond)))))
    (with-debugger
      (acceptor-dispatch-request acceptor request reply))))

(defun acceptor-status-message (request http-status-code &rest properties &key &allow-other-keys)
  "This function is called after the request's handler has been
   invoked to convert the HTTP-STATUS-CODE to a HTML message to be
   displayed to the user.  If this function returns a string, that
   string is sent to the client instead of the content produced by the
   handler, if any.

   If an ERROR-TEMPLATE-DIRECTORY is set in the current acceptor and
   the directory contains a file corresponding to HTTP-STATUS-CODE
   named <code>.html, that file is sent to the client after variable
   substitution.  Variables are referenced by ${<variable-name>}.

   Additional keyword arguments may be provided which are made
   available to the templating logic as substitution variables.  These
   variables can be interpolated into error message templates in,
   which contains the current URL relative to the server and without
   GET parameters.

   In addition to the variables corresponding to keyword arguments,
   the script-name, lisp-implementation-type,
   lisp-implementation-version and hunchentoot-version variables are
   available."
  (let ((acceptor (acceptor request))
        (reply (reply request)))
    (handler-case
        (labels
            ((substitute-request-context-variables (string)
               (let ((properties (append `(:script-name ,(script-name request)
                                                        :lisp-implementation-type ,(lisp-implementation-type)
                                                        :lisp-implementation-version ,(lisp-implementation-version)
                                                        :hunchentoot-version ,*hunchentoot-version*)
                                         properties)))
                 (cl-ppcre:regex-replace-all "(?i)\\$\\{([a-z0-9-_]+)\\}"
                                             string
                                             (lambda (target-string start end match-start match-end reg-starts reg-ends)
                                               (declare (ignore start end match-start match-end))
                                               (let ((variable-name (string-as-keyword (subseq target-string
                                                                                               (aref reg-starts 0)
                                                                                               (aref reg-ends 0)))))
                                                 (escape-for-html (princ-to-string (getf properties variable-name variable-name))))))))
             (file-contents (file)
               (let ((buf (make-string (file-length file))))
                 (read-sequence buf file)
                 buf))
             (error-contents-from-template ()
               (let ((error-file-template-pathname (and (acceptor-error-template-directory acceptor)
                                                        (probe-file (make-pathname :name (princ-to-string http-status-code)
                                                                                   :type "html"
                                                                                   :defaults (acceptor-error-template-directory acceptor))))))
                 (when error-file-template-pathname
                   (with-open-file (file error-file-template-pathname :if-does-not-exist nil :element-type 'character)
                     (when file
                       (setf (content-type reply) "text/html")
                       (substitute-request-context-variables (file-contents file))))))))
          (or (unless (< 300 http-status-code)
                (apply 'make-cooked-message request reply http-status-code properties)) ; don't ever try template for positive return codes
              (error-contents-from-template) ; try template
              (apply 'make-cooked-message request reply http-status-code properties))) ; fall back to cooked message
      (error (e)
        (acceptor-log-message acceptor :error "error ~A during error processing, sending cooked message to client" e)
        (apply 'make-cooked-message request reply http-status-code properties)))))

(defun make-cooked-message (request reply http-status-code &key error backtrace)
  (labels ((cooked-message (format &rest arguments)
             (setf (content-type reply) "text/html; charset=iso-8859-1")
             (format nil "<html><head><title>~D ~A</title></head><body><h1>~:*~A</h1>~?<p><hr>~A</p></body></html>"
                     http-status-code (reason-phrase http-status-code)
                     format (mapcar (lambda (arg)
                                      (if (stringp arg)
                                          (escape-for-html arg)
                                          arg))
                                    arguments)
                     (address-string request))))
    (case http-status-code
      ((#.+http-moved-temporarily+
        #.+http-moved-permanently+)
       (cooked-message "The document has moved <a href='~A'>here</a>" (header-out :location reply)))
      ((#.+http-authorization-required+)
       (cooked-message "The server could not verify that you are authorized to access the document requested.  ~
                        Either you supplied the wrong credentials \(e.g., bad password), or your browser doesn't ~
                        understand how to supply the credentials required."))
      ((#.+http-forbidden+)
       (cooked-message "You don't have permission to access ~A on this server."
                       (script-name request)))
      ((#.+http-not-found+)
       (cooked-message "The requested URL ~A was not found on this server."
                       (script-name request)))
      ((#.+http-bad-request+)
       (cooked-message "Your browser sent a request that this server could not understand."))
      ((#.+http-internal-server-error+)
       (if *show-lisp-errors-p*
           (cooked-message "<pre>~A~@[~%~%Backtrace:~%~%~A~]</pre>"
                           (escape-for-html (princ-to-string error))
                           (when *show-lisp-backtraces-p*
                             (escape-for-html (princ-to-string backtrace))))
           (cooked-message "An error has occured")))))) 

(defun acceptor-server-name (acceptor)
  (format nil "Hunchentoot ~A (~A)" *hunchentoot-version* (acceptor-name acceptor)))

