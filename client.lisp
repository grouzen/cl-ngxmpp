
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
  ((:resource (resource "cl-ngxmpp"))
   (:server-hostname (server-hostname "localhost"))
   (:server-port (server-port 5222))
   (:debuggable (debuggable t))))

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
      (print (cl-ngxmpp::connection (xml-stream obj)) stream))))

(defmethod disconnect ((client client))
  (let* ((xml-stream (xml-stream client))
         (connection (cl-ngxmpp::connection xml-stream)))
    (when (and (not (null xml-stream))
               (cl-ngxmpp:openedp xml-stream))
      (cl-ngxmpp:close-stream xml-stream))
    (when (and (not (null connection))
               (cl-ngxmpp:connectedp connection))
      (cl-ngxmpp:disconnect connection))))

(defmethod connect ((client client))
  (let ((connection (cl-ngxmpp:create-connection
                     :hostname (server-hostname client)
                     :port     (server-port     client))))
    (when (cl-ngxmpp:connectedp (cl-ngxmpp:connect connection))
      (let ((xml-stream (cl-ngxmpp:create-xml-stream
                         :connection connection
                         :debuggable (debuggable client))))
          (setf (xml-stream client) xml-stream)
          (cl-ngxmpp:open-stream xml-stream)
          (cl-ngxmpp:negotiate-tls xml-stream)))))

(defmethod authorize ((client client) &key username password)
  "Calls SASL authorization over TLS connection."
  (let ((xml-stream (xml-stream client)))
    (setf (username client) username)
    (setf (password client) password)
    (cl-ngxmpp:negotiate-sasl xml-stream :username username :password password)
    ;; TODO: hide into cl-ngxmpp package
    (cl-ngxmpp:with-stanza-output (xml-stream) ;; Send iq bind set stanza
      (make-instance 'cl-ngxmpp::iq-set-bind-stanza
                     :id       "bind_2"
                     :resource (resource client)))
    (cl-ngxmpp:with-stanza-input (xml-stream iq-bind) ;; Receive id bind result
      iq-bind)
    (cl-ngxmpp:with-stanza-output (xml-stream) ;; Send presence
      (make-instance 'presence-stanza))
    (cl-ngxmpp:with-stanza-input (xml-stream presence) ;; Receive self-presence stanza
      presence)))

(defmethod proceed-stanza-loop ((client client))
  (let ((xml-stream (xml-stream client)))
    (loop
       until (cl-ngxmpp:closedp xml-stream)
       do (proceed-stanza client))))

(defmethod proceed-stanza ((client client))
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-input (xml-stream stanza)
      (cl-ngxmpp:handle-stanza stanza))))

(defmethod send-message ((client client) &key to body)
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp::message-stanza
                     :from (jid client)
                     :to   to
                     :body body))))
