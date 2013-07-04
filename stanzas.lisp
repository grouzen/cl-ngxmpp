
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

(defgeneric xml-to-stanza (stanza)
  (:documentation
   "Method for serialization xml to one of the stanza's classes."))

(defgeneric stanza-to-xml (stanza)
  (:documentation
   "Returns xml object which is transformed further to string."))


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
   (from
    :accessor from
    :initarg :from
    :initform "")
   (id
    :accessor id
    :initarg :id
    :initform nil)
   (xml-lang
    :accessor xml-lang
    :initarg :xml-lang
    :initform "en")
   (xmlns
    :accessor xmlns
    :initarg :xmlns
    :initform "jabber:client")
   (xmlns-stream
    :accessor xmlns-stream
    :initarg :xmlns-stream
    :initform "http://etherx.jabber.org/streams")
   (version
    :accessor version
    :initarg :version
    :initform "1.0"))
  (:documentation
   "Actually is not a stanza, but it's xml chunk, so I called it stanza ;).
This class describes <stream:stream/> - XML entity which RFC 6120 calls `stream'."))

(defmethod print-object ((obj stream-stanza) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "to: ~A, from: ~A, id: ~A, xml-lang: ~A, xmlns: ~A, xmlns-stream: ~A, version: ~A"
            (to obj) (from obj) (id obj) (xml-lang obj) (xmlns obj) (xmlns-stream obj) (version obj))))

(defmethod make-stanza ((stanza stream-stanza) class-name)
  (let* ((xml-node (xml-node stanza))
         (stream-node (dom:first-child xml-node)))
    (xml-to-stanza (make-instance class-name
                                  :xml-node     xml-node
                                  :to           (dom:get-attribute stream-node "to")
                                  :from         (dom:get-attribute stream-node "from")
                                  :id           (dom:get-attribute stream-node "id")
                                  :xml-lang     (dom:get-attribute stream-node "xml:lang")
                                  :xmlns        (dom:get-attribute stream-node "xmlns")
                                  :xmlns-stream (dom:get-attribute stream-node "xmlns:stream")
                                  :version      (dom:get-attribute stream-node "version")))))

(defmethod xml-to-stanza ((stanza stream-stanza))
  (let* ((xml-node (xml-node stanza))
         (child (dom:first-child (dom:first-child xml-node)))
         (child-qname (dom:node-name child)))
    (string-case child-qname
      ("stream:features" (make-stanza stanza 'stream-stanza-features))
      ("stream:error"    (make-stanza stanza 'stream-stanza-error)))))

(defmethod stanza-to-xml ((stanza stream-stanza))
  (cxml:with-element "stream:stream"
    (cxml:attribute "to" (to stanza))
    (cxml:attribute "id" (id stanza))
    (cxml:attribute "xmlns" (xmlns stanza))
    (cxml:attribute "xmlns:stream" (xmlns-stream stanza))
    (cxml:attribute "version" (version stanza))))

(defconstant +supported-features+
  (list
   'starttls ; mandatory-to-negotiate (TLS negotiation)
   'mechanisms ; mandatory-to-negotiate (SASL negotiation)
   ))

(defclass stream-stanza-features (stream-stanza)
  ((features
   :accessor features
   :initarg :features
   :initform nil
   :documentation
   "List of cons' features, i.e. ((\"startls\" . t) (\"mechanisms\" . nil) ...)."))
  (:documentation
   "Subclass of `stream-stanza' which describes <stream:stream><stream:features/><stream:stream/>
entity. It is returned by a receiving entity (e.g. on client-to-server communication is a server)."))

(defmethod xml-to-stanza ((stanza stream-stanza-features))
  (let ((features (features stanza)))
    (dom:map-node-list
     #'(lambda (node)
         (let* ((feature-name (dom:node-name node))
                (feature-required (cond ((string= feature-name "starttls") t)
                                        ((string= feature-name "mechanisms") t)
                                        (t nil)))) ; TODO: check on <requred/> element.
           (setf features (cons (cons feature-name feature-required) features))))
     (dom:child-nodes (dom:first-child (dom:first-child (xml-node stanza)))))
    (setf (features stanza) features)
    stanza))


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

                          
