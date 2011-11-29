;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
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


(defgeneric execute-acceptor (taskmaster acceptor)
  (:documentation "Called on a taskmaster which should call accept-connections on the acceptor."))

(defgeneric handle-incoming-connection (taskmaster acceptor socket)
  (:documentation "Called on a taskmaster to handle a new connection by calling process-connection on acceptor."))

(defgeneric shutdown (taskmaster)
  (:documentation "Shutdown the taskmaster, cleaning up any threads it created."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple minded, single-threaded taskmaster implemenetation

(defclass single-threaded-taskmaster () ())

(defmethod execute-acceptor ((taskmaster single-threaded-taskmaster) acceptor)
  (accept-connections acceptor))

(defmethod handle-incoming-connection ((taskmaster single-threaded-taskmaster) acceptor socket)
  (process-connection acceptor socket))

(defmethod shutdown ((taskmaster single-threaded-taskmaster)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thread-per-connection taskmaster implemenetation

(defvar *default-max-thread-count* 100)
(defvar *default-max-accept-count* (+ *default-max-thread-count* 20))

;; You might think it would be nice to provide a taskmaster that takes
;; threads out of a thread pool.  There are two things to consider:
;;  - On a 2010-ish Linux box, thread creation takes less than 250 microseconds.
;;  - Bordeaux Threads doesn't provide a way to "reset" and restart a thread,
;;    and it's not clear how many Lisp implementations can do this.
;; So for now, we leave this out of the mix.
(defclass thread-per-connection-taskmaster ()
  ((acceptor-process :accessor acceptor-process)
   (max-thread-count
    :type (or integer null)
    :initarg :max-thread-count
    :initform nil
    :accessor taskmaster-max-thread-count)
   (max-accept-count
    :type (or integer null)
    :initarg :max-accept-count
    :initform nil
    :accessor taskmaster-max-accept-count)
   (request-count
    :type integer
    :initform 0
    :accessor taskmaster-request-count)
   (request-count-lock
    :initform (make-lock "taskmaster-request-count")
    :reader taskmaster-request-count-lock)
   (wait-queue
    :initform (make-condition-variable)
    :reader taskmaster-wait-queue)
   (wait-lock
    :initform (make-lock "taskmaster-thread-lock")
    :reader taskmaster-wait-lock)
   (worker-thread-name-format
    :type (or string null)
    :initarg :worker-thread-name-format
    :initform "toot-worker-~A"
    :accessor taskmaster-worker-thread-name-format))
  (:default-initargs
   :max-thread-count *default-max-thread-count*
   :max-accept-count *default-max-accept-count*)
  (:documentation "A taskmaster that starts one thread for listening
to incoming requests and one new thread for each incoming connection.

If MAX-THREAD-COUNT is null, a new thread will always be created for
each request.

If MAX-THREAD-COUNT is supplied, the number of request threads is
limited to that.  Furthermore, if MAX-ACCEPT-COUNT is not supplied, an
HTTP 503 will be sent if the thread limit is exceeded.  Otherwise, if
MAX-ACCEPT-COUNT is supplied, it must be greater than MAX-THREAD-COUNT;
in this case, requests are accepted up to MAX-ACCEPT-COUNT, and only
then is HTTP 503 sent.

In a load-balanced environment with multiple Toot servers, it's
reasonable to provide MAX-THREAD-COUNT but leave MAX-ACCEPT-COUNT null.
This will immediately result in HTTP 503 when one server is out of
resources, so the load balancer can try to find another server.

In an environment with a single Toot server, it's reasonable
to provide both MAX-THREAD-COUNT and a somewhat larger value for
MAX-ACCEPT-COUNT.  This will cause a server that's almost out of
resources to wait a bit; if the server is completely out of resources,
then the reply will be HTTP 503.

This is the default taskmaster implementation for multi-threaded Lisp
implementations."))

(defmethod initialize-instance :after ((taskmaster thread-per-connection-taskmaster) &rest init-args)
  "Ensure the if MAX-ACCEPT-COUNT is supplied, that it is greater than MAX-THREAD-COUNT."
  (declare (ignore init-args))
  (when (taskmaster-max-accept-count taskmaster)
    (unless (taskmaster-max-thread-count taskmaster)
      (parameter-error "MAX-THREAD-COUNT must be supplied if MAX-ACCEPT-COUNT is supplied"))
    (unless (> (taskmaster-max-accept-count taskmaster) (taskmaster-max-thread-count taskmaster))
      (parameter-error "MAX-ACCEPT-COUNT must be greater than MAX-THREAD-COUNT"))))

;; Taskmaster implementation

(defmethod execute-acceptor ((taskmaster thread-per-connection-taskmaster) acceptor)
  (setf (acceptor-process taskmaster)
        (make-thread
         (lambda () (accept-connections acceptor))
         :name (listen-thread-name acceptor))))

(defmethod handle-incoming-connection ((taskmaster thread-per-connection-taskmaster) acceptor socket)
  ;; Here's the idea, with the stipulations given in THREAD-PER-CONNECTION-TASKMASTER
  ;;  - If MAX-THREAD-COUNT is null, just start a taskmaster
  ;;  - If the connection count will exceed MAX-ACCEPT-COUNT or if MAX-ACCEPT-COUNT
  ;;    is null and the connection count will exceed MAX-THREAD-COUNT,
  ;;    return an HTTP 503 error to the client
  ;;  - Otherwise if we're between MAX-THREAD-COUNT and MAX-ACCEPT-COUNT,
  ;;    wait until the connection count drops, then handle the request
  ;;  - Otherwise, increment REQUEST-COUNT and start a taskmaster
  (cond
    ((null (taskmaster-max-thread-count taskmaster))
     ;; No limit on number of requests, just start a thread to handle the connection
     (create-connection-handler-thread taskmaster acceptor socket))
    ((if (taskmaster-max-accept-count taskmaster)
         (>= (taskmaster-request-count taskmaster) (taskmaster-max-accept-count taskmaster))
         (>= (taskmaster-request-count taskmaster) (taskmaster-max-thread-count taskmaster)))
     ;; Send HTTP 503 to indicate that we can't handle the request right now
     (log-message acceptor :warning "Can't handle a new request, too many request threads already")
     (send-service-unavailable-response acceptor socket))

    ((and (taskmaster-max-accept-count taskmaster)
          (>= (taskmaster-request-count taskmaster) (taskmaster-max-thread-count taskmaster)))
     ;; Wait for a request to finish, then carry on
     (wait-for-free-connection taskmaster)
     (create-connection-handler-thread taskmaster acceptor socket))

    (t
     (create-connection-handler-thread taskmaster acceptor socket))))

(defmethod shutdown ((taskmaster thread-per-connection-taskmaster))
  (loop while (thread-alive-p (acceptor-process taskmaster)) do (sleep 1)))

(defun increment-taskmaster-request-count (taskmaster)
  (when (taskmaster-max-thread-count taskmaster)
    (with-lock-held ((taskmaster-request-count-lock taskmaster))
      (incf (taskmaster-request-count taskmaster)))))

(defun decrement-taskmaster-request-count (taskmaster)
  (when (taskmaster-max-thread-count taskmaster)
    (prog1
        (with-lock-held ((taskmaster-request-count-lock taskmaster))
          (decf (taskmaster-request-count taskmaster)))
      (when (and (taskmaster-max-accept-count taskmaster)
                 (< (taskmaster-request-count taskmaster) (taskmaster-max-accept-count taskmaster)))
        (notify-free-connection taskmaster)))))

(defun notify-free-connection (taskmaster)
  (with-lock-held ((taskmaster-wait-lock taskmaster))
    (condition-notify (taskmaster-wait-queue taskmaster))))

(defun wait-for-free-connection (taskmaster)
  (with-lock-held ((taskmaster-wait-lock taskmaster))
    (loop until (< (taskmaster-request-count taskmaster) (taskmaster-max-thread-count taskmaster))
          do (condition-wait (taskmaster-wait-queue taskmaster) (taskmaster-wait-lock taskmaster)))))

(defun create-connection-handler-thread (taskmaster acceptor socket)
  "Create a thread for handling a single request"
  ;; we are handling all conditions here as we want to make sure that
  ;; the acceptor process never crashes while trying to create a
  ;; worker thread; one such problem exists in
  ;; GET-PEER-ADDRESS-AND-PORT which can signal socket conditions on
  ;; some platforms in certain situations.
  (handler-case*
   (make-thread
    (lambda ()
      (increment-taskmaster-request-count taskmaster)
      (unwind-protect
           (process-connection acceptor socket)
        (decrement-taskmaster-request-count taskmaster)))
    :name (connection-handler-thread-name taskmaster socket))
   (error (cond)
          ;; need to bind *ACCEPTOR* so that LOG-MESSAGE* can do its work.
          (log-message
           acceptor *lisp-errors-log-level*
           "Error while creating worker thread for new incoming connection: ~A" cond))))

(defun listen-thread-name (acceptor)
  (format nil "toot-listener-~A:~A" (or (address acceptor) "*") (port acceptor)))

(defun connection-handler-thread-name (taskmaster socket)
  (let ((address (usocket:get-peer-address socket))
        (port (usocket:get-peer-port socket)))
    (format nil (taskmaster-worker-thread-name-format taskmaster)
            (cond
              ((and address port)
               (format nil "~A:~A" (usocket:vector-quad-to-dotted-quad address) port))
              (t "Unknown endpoint")))))
