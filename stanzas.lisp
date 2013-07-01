
(in-package #:cl-ngxmpp)

(defmacro with-stanza-output ((xml-stream) &body body)
  `(with-stream-xml-output (,xml-stream)
     (stanza-to-xml ,@body)))

(defmacro with-stanza-input ((xml-stream stanza-input) &body body)
  (let ((xml-input (gensym "xml-input")))
    `(with-stream-xml-input (,xml-stream ,xml-input)
       (let ((,stanza-input (xml-to-stanza (make-instance 'stanza :xml-node ,xml-input))))
         ,@body))))


(defclass stanza ()
  ((xml-node
    :accessor xml-node
    :initarg :xml-node
    :initform nil)))

(defmethod xml-to-stanza ((stanza stanza))
  stanza)


(defclass stream-stanza (stanza)
  ((to
    :accessor to
    :initarg :to
    :initform "")
   (xmlns
    :accessor xmlns
    :initarg :xmlns
    :initform "")
   (xmlns-stream
    :accessor xmlns-stream
    :initarg :xmlns-stream
    :initform "http://etherx.jabber.org/streams")
   (version
    :accessor version
    :initarg :version
    :initform "1.0")))

;; TODO: read from (xml-node stanza) and fill fields.
(defmethod xml-to-stanza ((stanza stream-stanza))
  (make-instance 'stream-stanza))

(defmethod stanza-to-xml ((stanza stream-stanza))
  (cxml:with-element "stream:stream"
    (cxml:attribute "to" (to stanza))
    (cxml:attribute "xmlns" (xmlns stanza))
    (cxml:attribute "xmlns:stream" (xmlns-stream stanza))
    (cxml:attribute "version" (version stanza))))


(defclass stream-stanza-close (stream-stanza) ())

(defmethod xml-to-stanza ((stanza stream-stanza-close))
  (make-instance 'stream-stanza-close))

(defmethod stanza-to-xml ((stanza stream-stanza-close))
  (cxml:with-element "stream:stream"))

(defclass stream-stanza-error (stream-stanza)
  ((error-node
   :accessor error-node
   :initarg :error-node
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

                          
