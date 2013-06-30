
(in-package :cl-ngxmpp)

(defvar *default-hostname* "localhost")

(defvar *default-port* 5222)

(defclass connection ()
  ((hostname
    :accessor hostname
    :initarg :hostname
    :initform *default-hostname*)
   (port
    :accessor port
    :initarg :port
    :initform *default-port*)
   (socket
    :accessor socket
    :initarg :socket
    :initform nil)
   (socket-stream
    :accessor socket-stream
    :initarg :socket-stream
    :initform nil)))

(defcreate connection
  ((:hostname hostname)
   (:port port)))
  
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
  (let ((stream (socket-stream connection)))
    (and (streamp stream)
         (open-stream-p stream))))

(defmethod disconnect ((connection connection))
  "Close TCP connection."
  (close (socket-stream connection))
  connection)

(defmethod connect ((connection connection) &key (hostname *default-hostname*) (port *default-port*))
  "Open TCP connection to port on hostname, create and open socket, and returns connection."
  (let* ((socket (usocket:socket-connect hostname port :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream socket)))
    (progn
      (setf (socket connection) socket)
      (setf (socket-stream connection) stream))
    connection))
    
    
