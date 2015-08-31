;;;; connection.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp)

(defvar *default-hostname* "localhost")
(defvar *default-port* 5222)

(define-condition connection-error (proxy-error)
  ())

(defclass connection ()
  ((hostname :accessor hostname :initarg :hostname :initform *default-hostname*)
   (port     :accessor port     :initarg :port     :initform *default-port*)
   (adapter  :accessor adapter  :initarg :adapter  :initform nil)))

#+nil
(defmethod print-object ((obj connection) stream)
  "Just print a human readable representation of connection object."
  (print-unreadable-object (obj stream :type t :identity t)
    (let ((connected (connectedp obj)))
      (when connected
        (format stream "localhost:~A -> " (usocket:get-local-port (socket obj))))
      (format stream "~A:~A" (hostname obj) (port obj))
      (let ((status (if connected " (opened)" " (closed)")))
        (format stream status)))))

(defmethod connectedp ((connection connection))
  "Returns t if `connection' is connected to a server."
  (adapter-connectedp (adapter connection)))

(defmethod close-connection ((connection connection))
  "Close TCP connection."
  (adapter-close-connection (adapter connection)))

(defmethod open-connection ((connection connection))
  "Open TCP connection to port on hostname, create and open socket, and returns connection."
  (adapter-open-connection (adapter connection)
                           (hostname connection)
                           (port connection)))

