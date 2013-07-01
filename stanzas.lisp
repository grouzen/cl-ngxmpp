
(in-package #:cl-ngxmpp)

(defclass stanza ()
  ((node
    :accessor node
    :initarg :node
    :initform nil)))

(defclass message (stanza)
  ())

(defclass message-error (message)
  ())

(defclass presence (stanza)
  ())

(defclass presence-error (presence)
  ())

(defclass iq (stanza)
  ())

(defclass iq-get (iq)
  ())

(defclass iq-set (iq)
  ())

(defclass iq-result (iq)
  ())

(defclass iq-error (iq)
  ())

(defmethod stanza-to-xml ((stanza message))
  t)

(defmethod stanza-to-xml ((stanza presence))
  t)

(defmethod stanza-to-xml ((stanza iq))
  t)

(defmethod stanza-to-xml ((iq-set iq))
  t)

(defmethod stanza-to-xml ((iq-result iq))
  t)

                          
