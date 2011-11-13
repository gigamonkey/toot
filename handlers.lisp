;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

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

(defmacro maybe-handle (test &body body)
  "Handle the request with BODY if TEST is true. Otherwise return
'NOT-HANDLED."
  `(cond
     (,test ,@body)
     (t 'not-handled)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function and symbols as handlers.

(defmethod handle-request ((handler function) request)
  (funcall handler request))

(defmethod handle-request ((handler symbol) request)
  (funcall handler request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static file handler

(defun test-document-directory (&optional sub-directory)
  (asdf:system-relative-pathname :toot (format nil "www/~@[~A~]" sub-directory)))

;;; FIXME: this is perhaps not as correct as it should be. For
;;; instance, it may not work on windows beacuse of \ vs /. See the
;;; old create-folder-dispatcher-and-handler to see if there's any
;;; goodness that needs to be brought over.
(defun make-static-file-handler (document-root &optional uri-prefix)
  "Make a handler that maps the requested URI to a file under
DOCUMENT-ROOT and serves it if it exists. Does a basic sanity check to
dissalow requests for things like ../../../etc/passwd. Also maps
directory names to index.html in that directory. If URI-PREFIX is
supplied, it will strip that from the URI before mapping it to a
file."
  (lambda (request)
    (let ((path (url-decode (request-uri request))))
      (when-let (? (position #\? path))
        (setf path (subseq path 0 ?)))
      (unless (safe-filename-p path)
        (abort-request-handler request +http-forbidden+))
      (let ((file (resolve-file (enough-url path uri-prefix) document-root)))
        (serve-file request file)))))

(defun safe-filename-p (path)
  "Verify that a path, translated to a file doesn't contain any tricky
bits such as '..'"
  (let ((directory (pathname-directory (subseq path 1))))
    (or (stringp directory)
        (null directory)
        (and (consp directory)
             (eql (first directory) :relative)
             (every #'stringp (rest directory))))))

(defun resolve-file (path document-root)
  (merge-pathnames (subseq (add-index path) 1) document-root))
  
(defun add-index (filename &key (extension "html"))
  (format nil "~a~@[index~*~@[.~a~]~]" filename (ends-with #\/ filename) extension))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error page generation

(defmethod generate-error-page ((generator function) request &key error backtrace)
  (funcall generator request error backtrace))

(defmethod generate-error-page ((generator symbol) request &key error backtrace)
  (funcall generator request error backtrace))

(defun default-error-message-generator (request error backtrace)
  "A function that generates a bare-bones error page to be used as an error page generator."
  (let ((status-code (status-code request)))
    (with-output-to-string (s)
      (format s "<html><head><title>~d: ~a</title></head><body><h1>~2:*~d: ~a</h1></body></html>"
              status-code (reason-phrase status-code))
      (if (and error *show-lisp-errors-p*)
          (format s "<pre>~a~@[~%~%Backtrace:~%~%~a~]</pre>"
                  (escape-for-html (princ-to-string error))
                  (when (and backtrace *show-lisp-backtraces-p*)
                    (escape-for-html (princ-to-string backtrace))))))))

