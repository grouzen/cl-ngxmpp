
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
      ("stream:stream"   (make-stanza stanza 'stream-stanza))
      ("message"         (make-stanza stanza 'message))
      ("failure"         (make-stanza stanza 'failure-stanza))
      ("proceed"         (make-stanza stanza 'proceed-stanza))
      (:default          (make-stanza stanza 'unknown-stanza)))))


(defclass unknown-stanza (stanza)
  ())

(defmethod print-object ((obj unknown-stanza) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format "Unknown type of stanza: ~A" (dom:node-name (dom:first-child (xml-node obj))))))
        
(defmethod xml-to-stanza ((stanza unknown-stanza))
  stanza)


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
      ("stream:features" (make-stanza stanza 'stream-features-stanza))
      ("stream:error"    (make-stanza stanza 'stream-error-stanza)))))

(defmethod stanza-to-xml ((stanza stream-stanza))
  (cxml:with-element "stream:stream"
    (cxml:attribute "to" (to stanza))
    (cxml:attribute "id" (id stanza))
    (cxml:attribute "xmlns" (xmlns stanza))
    (cxml:attribute "xmlns:stream" (xmlns-stream stanza))
    (cxml:attribute "version" (version stanza))))


(defclass stream-features-stanza (stream-stanza)
  ((features
   :accessor features
   :initarg :features
   :initform nil
   :documentation
   "List of cons' features, i.e. ((\"startls\" . t) (\"mechanisms\" . nil) ...)."))
  (:documentation
   "Subclass of `stream-stanza' which describes <stream:stream><stream:features/><stream:stream/>
entity. It is returned by a receiving entity (e.g. on client-to-server communication is a server)."))

(defmethod print-object ((obj stream-features-stanza) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (mapcar #'(lambda (feature)
                (let ((feature-name (car feature))
                      (feature-required (cdr feature)))
                  (format stream "{~A -> ~A} " feature-name
                          (if feature-required
                              "required"
                              "not required"))))
            (features obj)))
  (call-next-method obj stream))

(defmethod xml-to-stanza ((stanza stream-features-stanza))
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


(defclass stream-close-stanza (stream-stanza) ())

(defmethod xml-to-stanza ((stanza stream-close-stanza))
  stanza)

(defmethod stanza-to-xml ((stanza stream-close-stanza))
  (cxml:with-element "stream:stream"))


(defclass stream-error-stanza (stream-stanza)
  ((error-node
   :accessor error-node
   :initarg :error-node
   :initform nil)))


(defclass message-stanza (stanza)
  ())

(defclass message-error-stanza (message-stanza)
  ())

(defclass presence-stanza (stanza)
  ())

(defclass presence-error-stanza (presence-stanza)
  ())

(defclass iq-stanza (stanza)
  ())

(defclass iq-get-stanza (iq-stanza)
  ())

(defclass iq-set-stanza (iq-stanza)
  ())

(defclass iq-result-stanza (iq-stanza)
  ())

(defclass iq-error-stanza (iq-stanza)
  ())


(defclass starttls-stanza (stanza)
  ((xmlns
    :accessor xmlns
    :initarg :xmlns
    :initform "urn:ietf:params:xml:ns:xmpp-tls")))

(defmethod stanza-to-xml ((stanza starttls-stanza))
  (cxml:with-element "starttls"
    (cxml:attribute "xmlns" (xmlns stanza))))

(defmethod xml-to-stanza ((stanza starttls-stanza))
  stanza)


(defclass proceed-stanza (stanza)
  ())

(defmethod xml-to-stanza ((stanza proceed-stanza))
  stanza)


(defclass failure-stanza (stanza)
  ((xmlns
    :accessor xmlns
    :initarg :xmlns
    :initform "")))

(defmethod print-object ((obj failure-stanza) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "xmlns: ~A" (xmlns obj))))

(defmethod xml-to-stanza ((stanza failure-stanza))
  (setf (xmlns stanza) (dom:node-name (dom:first-child (xml-node stanza))))
  stanza)
         
