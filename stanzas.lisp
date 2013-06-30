
(in-package #:cl-ngxmpp)

(defclass stanza ()
  ((element
    :accessor element
    :initarg :element
    :initform nil)))

(defclass message (stanza)
  ())

(defclass presence (stanza)
  ())

(defclass iq (stanza)
  ())

(defclass iq-get (iq)
  ())

(defclass iq-set (iq)
  ())

(defclass iq-result (iq)
  ())
