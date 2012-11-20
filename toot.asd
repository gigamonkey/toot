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

(in-package :cl-user)

(defpackage :toot-asd (:use :cl :asdf))

(in-package :toot-asd)

(defvar *toot-version* "0.0.1"
  "A string denoting the current version of Toot. Used for diagnostic
output.")

;(export '*toot-version*)

(defsystem :toot
  :description "A minimal web server originally built by stripping down Edi Weitz's Hunchentoot"
  :serial t
  :version #.*toot-version*
  :depends-on (:alexandria
               :chunga
               :cl-base64
               :cl-fad
               :cl-ppcre
               :flexi-streams
               :cl+ssl
               :md5
               :trivial-backtrace
               :usocket
               :bordeaux-threads
               :puri
               #+ecl :sb-bsd-sockets
               )
  :components ((:file "packages")
               (:file "rfc2388")
               (:file "specials")
               (:file "conditions")
               (:file "mime-types")
               (:file "util")
               (:file "cookie")
               (:file "set-timeouts")
               (:file "taskmaster")
               (:file "log")
               (:file "http")
               (:file "api")
               (:file "tests")
               (:file "documentation")))
