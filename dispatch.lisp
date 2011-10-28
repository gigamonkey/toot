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

(defgeneric dispatch (dispatcher request reply)
  (:documentation "An object that can be used by an acceptor to dispatch a request."))

(defmethod dispatch ((dispatcher function) request reply)
  (funcall dispatcher request reply))

(defgeneric generate-error-page (generator request &key error backtrace))

(defmethod generate-error-page ((generator function) request &key error backtrace)
  (funcall generator request error backtrace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static file dispatcher

(defun make-static-file-dispatcher (document-root)
  (lambda (request reply)
    (let ((script-name (script-name request)))
      (when (string= script-name "/fail")
        (error "Boo!")) 
      (unless (safe-filename-p script-name)
        (abort-request-handler request +http-forbidden+))
      (handle-static-file request reply (resolve-file script-name document-root)))))

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