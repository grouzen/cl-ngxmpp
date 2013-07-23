
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

(defmethod authorize ((client client) &key username password mechanism)
  "Calls SASL authorization over TLS connection."
  (let ((xml-stream (xml-stream client)))
    (setf (username client) username
          (password client) password)
    ;; SASL negotiation
    (cl-ngxmpp:negotiate-sasl xml-stream
                              :username username
                              :password password
                              :mechanism mechanism)
    (%bind% client) ;; Bind resource
    (%session% client) ;; Open session
    (send-presence client :show "online") ;; Send presence
    (proceed-stanza client))) ;; Receive self-presence stanza, ignore it.

(defmethod %bind% ((client client))
  (let ((xml-stream (xml-stream client))
        (resource   (resource client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:iq-set-bind-stanza
                     :id       "bind"
                     :resource (resource client)))
    (proceed-stanza client)))

(defmethod %session% ((client client))
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:iq-set-session-stanza
                     :to (server-hostname client)
                     :id "sess"))
    (proceed-stanza client)))

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
      (make-instance 'cl-ngxmpp:message-stanza
                     :from (jid client)
                     :to   to
                     :body body))))

(defmethod send-presence ((client client) &key to from show)
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:presence-stanza
                     :to to
                     :from from
                     :show show))))
        
