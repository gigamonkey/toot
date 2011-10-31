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

(defgeneric dispatch (dispatcher request)
  (:documentation "This function is used by the acceptor to dispatch a
request."))

(defgeneric generate-error-page (generator request &key error backtrace))

(defmethod dispatch ((dispatcher function) request)
  (funcall dispatcher request))


(defmethod generate-error-page ((generator function) request &key error backtrace)
  (funcall generator request error backtrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static file dispatcher

(defun test-document-directory (&optional sub-directory)
  (asdf:system-relative-pathname :toot (format nil "www/~@[~A~]" sub-directory)))

(defun make-static-file-dispatcher (document-root)
  (lambda (request)
    (let ((script-name (script-name request)))
      (when (string= script-name "/fail")
        (error "Boo!")) 
      (unless (safe-filename-p script-name)
        (abort-request-handler request +http-forbidden+))
      (serve-file request (resolve-file script-name document-root)))))

(defun safe-filename-p (script-name)
  "Verify that a script-name, translated to a file doesn't contain any
tricky bits such as '..'"
  (let ((directory (pathname-directory (subseq script-name 1))))
    (or (stringp directory)
        (null directory)
        (and (consp directory)
             (eql (first directory) :relative)
             (every #'stringp (rest directory))))))

(defun resolve-file (script-name document-root)
  (merge-pathnames (script-name-to-filename script-name) document-root))

(defun script-name-to-filename (script-name)
  (if (equal script-name "/") "index.html" (subseq script-name 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error page generation

(defun default-error-message-generator (request error backtrace)
  "Generate a bare-bones error page."
  (let ((status-code (return-code request)))
    (with-output-to-string (s)
      (format s "<html><head><title>~d: ~a</title></head><body><h1>~2:*~d: ~a</h1></body></html>"
              status-code (reason-phrase status-code))
      (if (and error *show-lisp-errors-p*)
          (format s "<pre>~a~@[~%~%Backtrace:~%~%~a~]</pre>"
                  (escape-for-html (princ-to-string error))
                  (when (and backtrace *show-lisp-backtraces-p*)
                    (escape-for-html (princ-to-string backtrace))))))))
