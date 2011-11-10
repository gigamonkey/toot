;;; Copyright (c) 2011, Peter Seibel.  All rights reserved.

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

(defvar *test-acceptor* nil)

(defun start-test-server (&key port)
  (setf *test-acceptor* (make-instance 'acceptor :port port :handler (test-handler)))
  (start *test-acceptor*))

(defun reset-test-handler ()
  (setf (handler *test-acceptor*) (test-handler)))

(defun test-handler ()
  (make-search-handler 
   (make-exact-path-handler "/form-test-params" 'form-test-params)
   (make-exact-path-handler "/form-test-octets" 'form-test-octets)
   (make-exact-path-handler "/form-test-stream" 'form-test-stream)
   (make-static-file-handler (test-document-directory))))

(defun form-test-params (request)
  (with-output-to-string (s)
    (format s "~&<html><head><title>Form test params</title></head><body>")
    (format s "~&<h1>Form results via <code>post-parameters</code></h1>")
    (loop for (k . v) in (post-parameters request)
       do
         (cond
           ((listp v)
            (format s "~&<p>~a: ~a</p><p><pre>" k v)
            (with-open-file (in (first v))
              (loop for char = (read-char in nil nil)
                 while char do (write-string (escape-for-html (string char)) s)))
            (format s "</pre></p>"))
           (t (format s "~&<p>~a: ~a</p>" k v))))
    (format s "~&</body></html>")))

(defun form-test-octets (request)
  (with-output-to-string (s)
    (format s "~&<html><head><title>Form test octets</title></head><body>")
    (format s "~&<h1>Form results via <code>body-octets</code></h1>")
    (format s "~&<p><pre>~a</pre></p>" (escape-for-html (octets-to-string (body-octets request))))
    (format s "~&</body></html>")))

(defun form-test-stream (request)
  (with-output-to-string (s)
    (format s "~&<html><head><title>Form test stream</title></head><body>")
    (format s "~&<h1>Form results via <code>body-stream</code></h1>")
    (format s "~&<p><pre>")
    (loop with in = (body-stream request)
       for char = (read-char in nil nil)
       while char do (write-string (escape-for-html (string char)) s))
    (format s "</pre></p>")
    (format s "~&</body></html>")))



