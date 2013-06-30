
(in-package :cl-ngxmpp-client)

(defclass client ()
  ((username
    :accessor username
    :initarg :username
    :initform "")
   (password
    :accessor password
    :initarg :password
    :initform "")
   (resource
    :accessor resource
    :initarg :resource
    :initform "cl-ngxmpp")
   (server-hostname
    :accessor server-hostname
    :initarg :server-hostname
    :initform cl-ngxmpp:*default-hostname*)
   (server-port
    :accessor server-port
    :initarg :server-port
    :initform cl-ngxmpp:*default-port*)
   (xml-stream
    :accessor xml-stream
    :initarg :xml-stream
    :initform nil)
   (session
    :accessor session
    :initarg :session
    :initform nil)
   (debuggable
    :accessor debuggable
    :initarg :debuggable
    :initform nil)))

(cl-ngxmpp:defcreate client
  ((:username username)
   (:password password)
   (:resource resource)
   (:server-hostname server-hostname)
   (:server-port server-port)
   (:debuggable debuggable)))

(defmethod jid ((client client))
  "Returns full jid, i.e. username@server.com/resource."
  (concatenate
   'string
   (username client)
   "@"
   (server-hostname client)
   "/"
   (resource client)))

(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A " (jid obj))
    (when (xml-stream obj)
      (print (cl-ngxmpp:connection (xml-stream obj)) stream))))

(defmethod connect ((client client))
  (let ((connection (cl-ngxmpp:create-connection
                     :hostname (server-hostname client)
                     :port     (server-port     client))))
    (when (cl-ngxmpp:connectedp (cl-ngxmpp:connect connection))
      (let ((xml-stream (cl-ngxmpp:create-xml-stream
                         :connection connection
                         :debuggable (debuggable client))))
        (setf (xml-stream client) xml-stream)))
        ;(cl-ngxmpp:open-stream xml-stream)))
    client))
  

