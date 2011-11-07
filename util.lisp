
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz. All rights reserved.

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

(defun starts-with-p (seq subseq &key (test 'eql))
  "Tests whether the sequence SEQ starts with the sequence
SUBSEQ.  Individual elements are compared with TEST."
  (let* ((length (length subseq))
         (mismatch (mismatch subseq seq
                             :test test)))
    (or (null mismatch)
        (<= length mismatch))))

(defun starts-with-one-of-p (seq subseq-list &key (test 'eql))
  "Tests whether the sequence SEQ starts with one of the
sequences in SUBSEQ-LIST.  Individual elements are compared with
TEST."
  (some (lambda (subseq)
          (starts-with-p seq subseq :test test))
        subseq-list))

(defun create-random-string (&optional (n 10) (base 16))
  "Returns a random number \(as a string) with base BASE and N
digits."
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base
              (random base *the-random-state*)))))

(defun string-as-keyword (string)
  "Intern STRING as keyword using the reader so that case conversion is done with the reader defaults."
  (let ((*package* (find-package :keyword)))
    (read-from-string string)))

(defun reason-phrase (status-code)
  "Returns a reason phrase for the HTTP return code STATUS-CODE
\(which should be an integer) or NIL for return codes Toot
doesn't know."
  (gethash status-code *http-reason-phrase-map* "No reason phrase known"))

(defgeneric assoc* (thing alist)
  (:documentation "Similar to CL:ASSOC, but 'does the right thing' if
THING is a string or a symbol.")
  (:method ((thing symbol) alist)
   (assoc thing alist :test #'eq))
  (:method ((thing string) alist)
   (assoc thing alist :test #'string-equal))
  (:method (thing alist)
   (assoc thing alist :test #'eql)))

(defun md5-hex (string)
  "Calculates the md5 sum of the string STRING and returns it as a hex string."
  (with-output-to-string (s)
    (loop for code across (md5:md5sum-sequence (coerce string 'simple-string))
	  do (format s "~2,'0x" code))))

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun http-token-p (token)
  "Tests whether TOKEN is a string which is a valid 'token'
according to HTTP/1.1 \(RFC 2068)."
  (and (stringp token)
       (plusp (length token))
       (every (lambda (char)
                (and ;; CHAR is US-ASCII but not control character or ESC
                     (< 31 (char-code char) 127)
                     ;; CHAR is not 'tspecial'
                     (not (find char "()<>@,;:\\\"/[]?={} " :test #'char=))))
              token)))

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123.  Default is current time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))

(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun make-tmp-filename-generator (request)
  (lambda ()
    (let ((tmp-filename
           #+:allegro (pathname (system:make-temp-file-name prefix *tmp-directory*))
           #-:allegro 
           (loop for pathname = (make-pathname 
                                 :name (format nil "toot-~A" (incf *tmp-counter*))
                                 :type nil
                                 :defaults *tmp-directory*)
              unless (probe-file pathname) return pathname)))
      (push tmp-filename (tmp-files request))
      ;; maybe call hook for file uploads
      (when *file-upload-hook*
        (funcall *file-upload-hook* tmp-filename))
      tmp-filename)))

(defun quote-string (string)
  "Quotes string according to RFC 2616's definition of `quoted-string'."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            unless (or (char< char #\Space)
                       (char= char #\Rubout))
              do (case char
                   ((#\\) (write-string "\\\\" out))
                   ((#\") (write-string "\\\"" out))
                   (otherwise (write-char char out)))))))

(defmacro upgrade-vector (vector new-type &key converter)
  "Returns a vector with the same length and the same elements as
VECTOR \(a variable holding a vector) but having element type
NEW-TYPE.  If CONVERTER is not NIL, it should designate a function
which will be applied to each element of VECTOR before the result is
stored in the new vector.  The resulting vector will have a fill
pointer set to its end.

The macro also uses SETF to store the new vector in VECTOR."
  `(setf ,vector
         (loop with length = (length ,vector)
               with new-vector = (make-array length
                                             :element-type ,new-type
                                             :fill-pointer length)
               for i below length
               do (setf (aref new-vector i) ,(if converter
                                               `(funcall ,converter (aref ,vector i))
                                               `(aref ,vector i)))
               finally (return new-vector))))

(defun url-decode (string &optional (external-format *default-external-format*))
  "Decodes a URL-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type 'octet :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
      (unless (< i (length string))
        (return))
      (let ((char (aref string i)))
       (labels ((decode-hex (length)
                  (prog1
                      (parse-integer string :start i :end (+ i length) :radix 16)
                    (incf i length)))
                (push-integer (integer)
                  (vector-push integer vector))
                (peek ()
                  (aref string i))
                (advance ()
                  (setf char (peek))
                  (incf i)))
         (cond
          ((char= #\% char)
           (advance)
           (cond
            ((char= #\u (peek))
             (unless unicodep
               (setf unicodep t)
               (upgrade-vector vector '(integer 0 65535)))
             (advance)
             (push-integer (decode-hex 4)))
            (t
             (push-integer (decode-hex 2)))))
          (t
           (push-integer (char-code (case char
                                      ((#\+) #\Space)
                                      (otherwise char))))
           (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (octets-to-string vector :external-format external-format)))))

(defun form-url-encoded-list-to-alist (form-url-encoded-list
                                       &optional (external-format *default-external-format*))
  "Converts a list FORM-URL-ENCODED-LIST of name/value pairs into an
alist.  Both names and values are url-decoded while doing this."
  (mapcar #'(lambda (entry)
              (destructuring-bind (name &optional value)
                  (split "=" entry :limit 2)
                (cons (string-trim " " (url-decode name external-format))
                      (url-decode (or value "") external-format))))
          form-url-encoded-list))

(defun url-encode (string &optional (external-format *default-external-format*))
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "$-_.!*'()" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (string-to-octets string
                                                               :start index
                                                               :end (1+ index)
                                                               :external-format external-format)
                            do (format s "%~2,'0x" octet)))))))

(defun parse-content-type-header (content-type-header)
  "Reads and parses a `Content-Type' header and returns it as three
values - the type, the subtype, and the requests' character set as
specified in the 'charset' parameter in the header, if there is one
and if the content type is \"text\".  CONTENT-TYPE-HEADER is supposed
to be the corresponding header value as a string."
  (with-input-from-sequence (stream (map 'list 'char-code content-type-header))
    (with-character-stream-semantics
     (let* ((*current-error-message* "Corrupted Content-Type header:")
            (type (read-token stream))
            (subtype (if (eql #\/ (read-char* stream nil))
                       (read-token stream)
                       (return-from parse-content-type-header
                         ;; try to return something meaningful
                         (values "application" "octet-stream" nil))))
            (parameters (read-name-value-pairs stream))
            (charset (cdr (assoc "charset" parameters :test #'string=)))
            (charset
             (when (string-equal type "text")
               charset)))
       (values type subtype charset)))))

(defun keep-alive-p (request)
  "Returns a true value unless the incoming request's headers or the
server's PERSISTENT-CONNECTIONS-P setting obviate a keep-alive reply.
The second return value denotes whether the client has explicitly
asked for a persistent connection."
  (let ((connection-values
         ;; the header might consist of different values separated by commas
         (when-let (connection-header (header-in :connection request))
           (split "\\s*,\\s*" connection-header))))
    (flet ((connection-value-p (value)
             (member value connection-values :test #'string-equal)))
      (let ((keep-alive-requested-p (connection-value-p "keep-alive")))
        (values (and (persistent-connections-p (acceptor request))
                     (or (and (eql (server-protocol request) :http/1.1)
                              (not (connection-value-p "close")))
                         (and (eql (server-protocol request) :http/1.0)
                              keep-alive-requested-p)))
                keep-alive-requested-p)))))

(defun address-string (request)
  "Returns a string with information about Toot suitable for
inclusion in HTML output."
  (flet ((escape-for-html (arg)
           (if arg
               (escape-for-html arg)
               arg)))
    (format nil "<address><a href='http://www.gigamonkeys.com/toot/'>Toot ~A</a> <a href='~A'>(~A ~A)</a>~@[ at ~A~:[ (port ~D)~;~]~]</address>"
            *toot-version*
            +implementation-link+
            (escape-for-html (lisp-implementation-type))
            (escape-for-html (lisp-implementation-version))
            (escape-for-html (or (host request) (address (acceptor request))))
            (scan ":\\d+$" (or (host request) ""))
            (port (acceptor request)))))

(defun chunking-input-p (request)
  "Whether input chunking is currently switched on for the acceptors
content stream. N.B. This is different than whether the "
  (chunked-stream-input-chunking-p (content-stream request)))
