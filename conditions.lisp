;;; Copyright (c) 2008-2009, Dr. Edmund Weitz.  All rights reserved.
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

(define-condition toot-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to Toot."))

(define-condition toot-error (toot-condition error)
  ()
  (:documentation "Superclass for all errors related to Toot."))

(define-condition toot-simple-error (toot-error simple-condition)
  ()
  (:documentation "Like TOOT-ERROR but with formatting capabilities."))

(defun internal-error (format-control &rest format-arguments)
  "Signals an error of type TOOT-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'toot-simple-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition toot-warning (toot-condition warning)
  ()
  (:documentation "Superclass for all warnings related to Toot."))

(define-condition toot-simple-warning (toot-warning simple-condition)
  ()
  (:documentation "Like TOOT-WARNING but with formatting capabilities."))

(defun toot-warn (format-control &rest format-arguments)
  "Signals a warning of type TOOT-SIMPLE-WARNING with the
provided format control and arguments."
  (warn 'toot-simple-warning
        :format-control format-control
        :format-arguments format-arguments))

(define-condition parameter-error (toot-simple-error)
  ()
  (:documentation "Signalled if a function was called with incosistent or illegal parameters."))

(defun parameter-error (format-control &rest format-arguments)
  "Signals an error of type PARAMETER-ERROR with the provided
format control and arguments."
  (error 'parameter-error
         :format-control format-control
         :format-arguments format-arguments))

(define-condition operation-not-implemented (toot-error)
  ((operation :initarg :operation
              :reader operation
              :documentation "The name of the unimplemented operation."))
  (:report (lambda (condition stream)
             (format stream "The operation ~A is not yet implemented for the implementation ~A.
Consider sending a patch..."
                     (operation condition)
                     (lisp-implementation-type))))
  (:documentation "This warning is signalled when an operation \(like
SETUID for example) is not implemented for a specific Lisp."))

(define-condition request-aborted (toot-condition)
  ((response-status-code :initarg :response-status-code :reader response-status-code)
   (body :initarg :body :initform nil :reader body))
  (:documentation "Signaled internally to cause handling of a request to be aborted and a response sent by Toot itself."))

(defun not-implemented (name)
  "Used to signal an error if an operation named NAME is not implemented."
  (error 'operation-not-implemented :operation name))

(defgeneric maybe-invoke-debugger (condition)
  (:documentation "This generic function is called whenever a
condition CONDITION is signaled in Toot. You might want to specialize
it on specific condition classes for debugging purposes. The default
method invokes the debugger with CONDITION if *CATCH-ERRORS-P* is
NIL.")
  (:method (condition)
    (when *debug-errors-p* (invoke-debugger condition))))

(defmacro with-debugger (&body body)
  "Executes BODY and invokes the debugger if an error is signaled and
*CATCH-ERRORS-P* is NIL."
  `(handler-bind ((error #'maybe-invoke-debugger))
     ,@body))

(defmacro ignore-errors* (&body body)
  "Like IGNORE-ERRORS, but observes *CATCH-ERRORS-P*."
  `(ignore-errors (with-debugger ,@body)))

(defmacro handler-case* (expression &rest clauses)
  "Like HANDLER-CASE, but observes *CATCH-ERRORS-P*."
  `(handler-case (with-debugger ,expression)
     ,@clauses))

(defun get-backtrace ()
  "Returns a string with a backtrace of what the Lisp system thinks is
the \"current\" error."
  (handler-case
      (with-output-to-string (s)
        (trivial-backtrace:print-backtrace-to-stream s))
    (error (condition)
      (format nil "Could not generate backtrace: ~A." condition))))
