
(in-package #:cl-ngxmpp)

(defmacro with-stanza-output ((xml-stream) &body body)
  `(with-stream-xml-output (,xml-stream)
     (stanza-to-xml ,@body)))

(defmacro with-stanza-input ((xml-stream stanza-input) &body body)
  (let ((xml-input (gensym "xml-input")))
    `(with-stream-xml-input (,xml-stream ,xml-input)
       (let ((,stanza-input (xml-to-stanza (make-instance 'stanza :xml-node ,xml-input))))
         ,@body))))


(defgeneric make-stanza (stanza class-name)
  (:documentation
   "This method makes new instance of `class-name' stanza,
fill it with the necessary fields taken from the parent,
and calls `xml-to-stanza' with this new instance. This method
needs to be implemented only for parental classes"))

(defclass stanza ()
  ((xml-node
    :accessor xml-node
    :initarg :xml-node
    :initform nil)))


(defmethod make-stanza ((stanza stanza) class-name)
  (xml-to-stanza (make-instance class-name :xml-node (xml-node stanza))))

;; TODO: dispatch over all heirs of stanza.
(defmethod xml-to-stanza ((stanza stanza))
  (let ((qname (dom:node-name (dom:first-child (xml-node stanza)))))
    (string-case qname
      ("stream:stream" (make-stanza stanza 'stream-stanza))
      ("message"       (make-stanza stanza 'message)))))
        

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

(defmethod make-stanza ((stanza stream-stanza) class-name)
  (xml-to-stanza (make-instance class-name
                                :xml-node     (xml-node stanza)
                                :to           (to stanza)
                                :xmlns        (xmlns stanza)
                                :xmlns-stream (xmlns-stream stanza)
                                :version      (version stanza))))

;; TODO: read from (xml-node stanza) and fill fields.
(defmethod xml-to-stanza ((stanza stream-stanza))
  (let* ((xml-node (xml-node stanza))
         (child (dom:first-child (dom:first-child xml-node)))
         (child-qname (dom:node-name child)))
    ;(write-string child-qname *debug-io*)))
    (string-case child-qname
      ("stream:features" (make-stanza stanza 'stream-stanza-features))
      ("stream:error"    (make-stanza stanza 'stream-stanza-error)))))

(defmethod stanza-to-xml ((stanza stream-stanza))
  (cxml:with-element "stream:stream"
    (cxml:attribute "to" (to stanza))
    (cxml:attribute "xmlns" (xmlns stanza))
    (cxml:attribute "xmlns:stream" (xmlns-stream stanza))
    (cxml:attribute "version" (version stanza))))


(defclass stream-stanza-features (stream-stanza)
  ((features
   :accessor features
   :initarg :features
   :initform nil)))

(defmethod xml-to-stanza ((stanza stream-stanza-features))
  (setf (features stanza) (dom:first-child (dom:first-child (xml-node stanza))))
  stanza)

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

                          
