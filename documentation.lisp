;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;; Copyright (c) 2011, Peter Seibel. All rights reserved.

;;; See LICENSE for details.

(in-package :toot)

(setf (documentation #'acceptor t) "acceptor that accepted this request.")
(setf (documentation #'access-logger t) "logger object that can log HTTP accesses.")
(setf (documentation #'address t) "IP address to which the acceptor binds.")
(setf (documentation #'body-octets t) "body of the request as octets.")
(setf (documentation #'body-stream t) "body of the request as a stream.")
(setf (documentation #'content-length t) "length of the response about to be written.")
(setf (documentation #'content-type t) "the MIME type of the response about to be written.")
(setf (documentation #'error-generator t) "object responsible for generating error pages.")
(setf (documentation #'handler t) "object responsible for generating a response to each request.")
(setf (documentation #'message-logger t) "object that logs miscelaneous messages to the Toot message log.")
(setf (documentation #'name t) "name of the server, used in to set the Server response header.")
(setf (documentation #'persistent-connections-p t) "flag controlling whether acceptor will allow persistent connections.")
(setf (documentation #'port t) "port the acceptor will listen on.")
(setf (documentation #'read-timeout t) "timeout for reading from the client.")
(setf (documentation #'remote-addr t) "IP address of the client side of the socket making the request.")
(setf (documentation #'remote-port t) "port of the client side of the socket making the request.")
(setf (documentation #'request-method t) "HTTP method (e.g. GET, POST, HEAD) of the request.")
(setf (documentation #'request-uri t) "URI of the request as a puri:uri object.")
(setf (documentation #'server-protocol t) "server protocol of the request as a keyword.")
(setf (documentation #'status-code t) "HTTP status code of the response being generated.")
(setf (documentation #'taskmaster t) "object responsible for running the acceptor.")
(setf (documentation #'write-timeout t) "timeout for writing to the client.")
(setf (documentation #'cookies-in t) "cookies sent by the client as an alist.")
(setf (documentation #'get-parameters t) "parameters sent in the query string.")
(setf (documentation #'request-headers t) "complete set of headers sent in the request as an alist.")