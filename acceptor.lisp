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
   (document-root :initarg :document-root :accessor acceptor-document-root))

  (:default-initargs
    :address nil
    :port 80
    :name (gensym)
    :listen-backlog 50
    :taskmaster (make-instance (cond (*supports-threads-p* 'thread-per-connection-taskmaster)
                                     (t 'single-threaded-taskmaster)))
    :output-chunking-p t
    :input-chunking-p t
    :persistent-connections-p t
    :read-timeout *default-connection-timeout*
    :write-timeout *default-connection-timeout*
    :access-log-destination *error-output*
    :message-log-destination *error-output*
    :document-root (load-time-value (default-document-directory))
    :error-template-directory (load-time-value (default-document-directory "errors/"))))

(defmethod print-object ((acceptor acceptor) stream)
  (print-unreadable-object (acceptor stream :type t)
    (format stream "\(host ~A, port ~A)"
            (or (acceptor-address acceptor) "*") (acceptor-port acceptor))))

;; Only generic for SSL-ACCEPTOR
(defgeneric initialize-connection-stream (acceptor stream))

;; Only generic for SSL-ACCEPTOR
(defgeneric acceptor-ssl-p (acceptor))

;; general implementation

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

(defmethod initialize-connection-stream ((acceptor acceptor) stream)
 (declare (ignore acceptor))
 ;; default method does nothing
 stream)

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

(defun do-with-acceptor-request-count-incremented (*acceptor* function)
  (with-lock-held ((acceptor-shutdown-lock *acceptor*))
    (incf (accessor-requests-in-progress *acceptor*)))
  (unwind-protect
       (funcall function)
    (with-lock-held ((acceptor-shutdown-lock *acceptor*))
      (decf (accessor-requests-in-progress *acceptor*))
      (when (acceptor-shutdown-p *acceptor*)
        (condition-variable-signal (acceptor-shutdown-queue *acceptor*))))))

(defmacro with-acceptor-request-count-incremented ((acceptor) &body body)
  "Execute BODY with ACCEPTOR-REQUESTS-IN-PROGRESS of ACCEPTOR
  incremented by one.  If the ACCEPTOR-SHUTDOWN-P returns true after
  the BODY has been executed, the ACCEPTOR-SHUTDOWN-QUEUE condition
  variable of the ACCEPTOR is signalled in order to finish shutdown
  processing."
  `(do-with-acceptor-request-count-incremented ,acceptor (lambda () ,@body)))

(defun process-connection (*acceptor* socket)
  (handler-bind ((error
                  ;; abort if there's an error which isn't caught inside
                  (lambda (cond)
                    (log-message* *lisp-errors-log-level* "Error while processing connection: ~A" cond)
                    (return-from process-connection)))
                 (warning
                  ;; log all warnings which aren't caught inside
                  (lambda (cond)
                    (log-message* *lisp-warnings-log-level* "Warning while processing connection: ~A" cond))))
    (with-mapped-conditions ()
      (let ((*hunchentoot-stream*
             (initialize-connection-stream *acceptor* (make-socket-stream socket *acceptor*))))
        (unwind-protect
             ;; process requests until either the acceptor is shut down,
             ;; *CLOSE-HUNCHENTOOT-STREAM* has been set to T by the
             ;; handler, or the peer fails to send a request
             (loop
                (let ((*close-hunchentoot-stream* t))
                  (when (acceptor-shutdown-p *acceptor*)
                    (return))
                  (multiple-value-bind (headers-in method url-string protocol)
                      (get-request-data *hunchentoot-stream*)
                    ;; check if there was a request at all
                    (unless method
                      (return))
                    ;; bind per-request special variables, then process the
                    ;; request - note that *ACCEPTOR* was bound above already
                    (let ((*reply* (make-instance 'reply))
                          (transfer-encodings (cdr (assoc* :transfer-encoding headers-in))))
                      (when transfer-encodings
                        (setq transfer-encodings
                              (split "\\s*,\\s*" transfer-encodings))
                        (when (member "chunked" transfer-encodings :test #'equalp)
                          (cond ((acceptor-input-chunking-p *acceptor*)
                                 ;; turn chunking on before we read the request body
                                 (setf *hunchentoot-stream* (make-chunked-stream *hunchentoot-stream*)
                                       (chunked-stream-input-chunking-p *hunchentoot-stream*) t))
                                (t (hunchentoot-error "Client tried to use ~
chunked encoding, but acceptor is configured to not use it.")))))
                      (multiple-value-bind (remote-addr remote-port)
                          (get-peer-address-and-port socket)
                        (with-acceptor-request-count-incremented (*acceptor*)
                          (process-request (make-instance 'request
                                             :acceptor *acceptor*
                                             :remote-addr remote-addr
                                             :remote-port remote-port
                                             :headers-in headers-in
                                             :content-stream *hunchentoot-stream*
                                             :method method
                                             :uri url-string
                                             :server-protocol protocol)))))
                    (force-output *hunchentoot-stream*)
                    (setq *hunchentoot-stream* (reset-connection-stream *acceptor* *hunchentoot-stream*))
                    (when *close-hunchentoot-stream*
                      (return)))))
          (when *hunchentoot-stream*
            ;; as we are at the end of the request here, we ignore all
            ;; errors that may occur while flushing and/or closing the
            ;; stream.
            (ignore-errors*
              (force-output *hunchentoot-stream*))
            (ignore-errors*
              (close *hunchentoot-stream* :abort t))))))))




  
(defmethod acceptor-ssl-p ((acceptor t)) nil)

(defun acceptor-log-access (acceptor &key return-code)

  (with-log-stream (stream (acceptor-access-log-destination acceptor) *access-log-lock*)
    (format stream "~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] [~A] \"~A ~A~@[?~A~] ~
                    ~A\" ~D ~:[-~;~:*~D~] \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\"~%"
            (remote-addr*)
            (header-in* :x-forwarded-for)
            (authorization)
            (iso-time)
            (request-method*)
            (script-name*)
            (query-string*)
            (server-protocol*)
            return-code
            (content-length*)
            (referer)
            (user-agent))))

(defun acceptor-log-message (acceptor log-level format-string &rest format-arguments)
  (with-log-stream (stream (acceptor-message-log-destination acceptor) *message-log-lock*)
    (format stream "[~A~@[ [~A]~]] ~?~%"
            (iso-time) log-level
            format-string format-arguments)))

(defun log-message* (log-level format-string &rest format-arguments)
  (apply 'acceptor-log-message *acceptor* log-level format-string format-arguments))

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


(defmethod acceptor-dispatch-request ((acceptor acceptor) request)
  (declare (ignore request))
  (if (acceptor-document-root acceptor)
      (handle-static-file (merge-pathnames (if (equal (script-name*) "/")
                                               "index.html"
                                               (subseq (script-name*) 1))
                                           (acceptor-document-root acceptor)))
      (setf (return-code *reply*) +http-not-found+)))

(defun handle-request (*acceptor* *request*)
  "Standard method for request handling.  Calls the request dispatcher
of *ACCEPTOR* to determine how the request should be handled.  Also
sets up standard error handling which catches any errors within the
handler."
  (handler-bind ((error
                  (lambda (cond)
                    ;; if the headers were already sent, the error
                    ;; happened within the body and we have to close
                    ;; the stream
                    (when *headers-sent*
                      (setq *close-hunchentoot-stream* t))
                    (throw 'handler-done
                      (values nil cond (get-backtrace)))))
                 (warning
                  (lambda (cond)
                    (when *log-lisp-warnings-p*
                      (log-message* *lisp-warnings-log-level* "~A" cond)))))
    (with-debugger
      (acceptor-dispatch-request *acceptor* *request*))))

(defgeneric acceptor-status-message (acceptor http-status-code &key &allow-other-keys)
  (:documentation
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
   available."))

(defun make-cooked-message (http-status-code &key error backtrace)
  (labels ((cooked-message (format &rest arguments)
             (setf (content-type*) "text/html; charset=iso-8859-1")
             (format nil "<html><head><title>~D ~A</title></head><body><h1>~:*~A</h1>~?<p><hr>~A</p></body></html>"
                     http-status-code (reason-phrase http-status-code)
                     format (mapcar (lambda (arg)
                                      (if (stringp arg)
                                          (escape-for-html arg)
                                          arg))
                                    arguments)
                     (address-string))))
    (case http-status-code
      ((#.+http-moved-temporarily+
        #.+http-moved-permanently+)
       (cooked-message "The document has moved <a href='~A'>here</a>" (header-out :location)))
      ((#.+http-authorization-required+)
       (cooked-message "The server could not verify that you are authorized to access the document requested.  ~
                        Either you supplied the wrong credentials \(e.g., bad password), or your browser doesn't ~
                        understand how to supply the credentials required."))
      ((#.+http-forbidden+)
       (cooked-message "You don't have permission to access ~A on this server."
                       (script-name *request*)))
      ((#.+http-not-found+)
       (cooked-message "The requested URL ~A was not found on this server."
                       (script-name *request*)))
      ((#.+http-bad-request+)
       (cooked-message "Your browser sent a request that this server could not understand."))
      ((#.+http-internal-server-error+)
       (if *show-lisp-errors-p*
           (cooked-message "<pre>~A~@[~%~%Backtrace:~%~%~A~]</pre>"
                           (escape-for-html (princ-to-string error))
                           (when *show-lisp-backtraces-p*
                             (escape-for-html (princ-to-string backtrace))))
           (cooked-message "An error has occured"))))))

(defmethod acceptor-status-message ((acceptor t) http-status-code &rest args &key &allow-other-keys)
  (apply 'make-cooked-message http-status-code args))

(defmethod acceptor-status-message :around ((acceptor acceptor) http-status-code &rest args &key &allow-other-keys)
  (handler-case
      (call-next-method)
    (error (e)
      (log-message* :error "error ~A during error processing, sending cooked message to client" e)
      (apply 'make-cooked-message http-status-code args))))

(defun string-as-keyword (string)
  "Intern STRING as keyword using the reader so that case conversion is done with the reader defaults."
  (let ((*package* (find-package :keyword)))
    (read-from-string string)))

(defmethod acceptor-status-message ((acceptor acceptor) http-status-code &rest properties &key &allow-other-keys)
  "Default function to generate error message sent to the client."
  (labels
      ((substitute-request-context-variables (string)
         (let ((properties (append `(:script-name ,(script-name*)
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
                 (setf (content-type*) "text/html")
                 (substitute-request-context-variables (file-contents file))))))))
    (or (unless (< 300 http-status-code)
          (call-next-method))           ; don't ever try template for positive return codes
        (error-contents-from-template)  ; try template
        (call-next-method))))           ; fall back to cooked message


(defun acceptor-server-name (acceptor)
  (format nil "Hunchentoot ~A (~A)" *hunchentoot-version* (acceptor-name acceptor)))

