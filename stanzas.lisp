;;;; stanzas.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

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

(defgeneric handle-stanza (stanza)
  (:documentation
   "This handler must be overrided on client code."))

;;
;; Basic stanza class.
;;
;; TODO: export from cl-ngxmpp package.
;;
(defclass stanza ()
  ((xml-node
    :accessor xml-node
    :initarg :xml-node
    :initform nil)))

(defmethod handle-stanza ((stanza stanza))
  (error "Default stanza handler called. Please define handler for this type of stanza: ~A" stanza))

(defmethod make-stanza ((stanza stanza) class-name)
  (xml-to-stanza (make-instance class-name :xml-node (xml-node stanza))))

;; TODO: dispatch over all heirs of stanza.
(defmethod xml-to-stanza ((stanza stanza))
  (let ((qname (dom:node-name (dom:first-child (xml-node stanza)))))
    (string-case qname
      ("stream:stream"   (make-stanza stanza 'stream-stanza))
      ("message"         (make-stanza stanza 'message-stanza))
      ("failure"         (make-stanza stanza 'failure-stanza))
      ("success"         (make-stanza stanza 'success-stanza))
      ("proceed"         (make-stanza stanza 'proceed-stanza))
      ("challenge"       (make-stanza stanza 'sasl-challenge-stanza))
      ("iq"              (make-stanza stanza 'iq-stanza))
      ("presence"        (make-stanza stanza 'presence-stanza))
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
                (feature-required (string-case feature-name
                                    ("session"    t) ;; TODO: implement required checking.
                                    ("mechanisms" t) ;; These four features are 
                                    ("starttls"   t) ;; mandatory-to-negitiate for 
                                    ("bind"       t) ;; client and server, see RFC 6120 and RFC 3921.
                                    (:default     nil)))) ;; TODO: check on <required/> element
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

;;
;; Message stanzas
;;
(defclass message-stanza (stanza)
  ((from
    :accessor from
    :initarg :from
    :initform nil)
   (to
    :accessor to
    :initarg :to
    :initform nil)
   (body
    :accessor body
    :initarg :body
    :initform "")))

(defmethod stanza-to-xml ((stanza message-stanza))
  (cxml:with-element "message"
    (unless (null (from stanza))
      (cxml:attribute "from" (from stanza)))
    (unless (null (to stanza))
      (cxml:attribute "to" (to stanza)))
    (cxml:with-element "body"
      (cxml:text (body stanza)))))

(defmethod xml-to-stanza ((stanza message-stanza))
  (let* ((xml-node     (xml-node stanza))
         (message-node (dom:first-child xml-node))
         (to           (dom:get-attribute message-node "to"))
         (from         (dom:get-attribute message-node "from"))
         (body         (dom:data (dom:first-child (dom:first-child message-node)))))
    (setf (to stanza) to
          (from stanza) from
          (body stanza) body)
    stanza))
    

(defclass message-error-stanza (message-stanza)
  ())

;;
;; Presence stanzas
;;
(defclass presence-stanza (stanza)
  ((to
    :accessor to
    :initarg :to
    :initform nil)
   (from
    :accessor from
    :initarg :from
    :initform nil)
   (show
    :accessor show
    :initarg :show
    :initform nil)))

(defmethod xml-to-stanza ((stanza presence-stanza))
  stanza)

(defmethod stanza-to-xml ((stanza presence-stanza))
  (cxml:with-element "presence"
    (unless (null (to stanza))
      (cxml:attribute "to"   (to stanza)))
    (unless (null (from stanza))
      (cxml:attribute "from" (from stanza)))
    (cxml:with-element "show"
      (cxml:text (show stanza)))))

(defclass presence-error-stanza (presence-stanza)
  ())

;;
;; IQ stanzas
;;
(defclass iq-stanza (stanza)
  ((id
    :accessor id
    :initarg :id
    :initform nil)
   (iq-type
    :accessor iq-type
    :initarg :iq-type
    :initform "get")
   (to
    :accessor to
    :initarg :to
    :initform nil)
   (from
    :accessor from
    :initarg :from
    :initform nil)))

(defmacro with-iq-stanza ((iq-stanza) &body body)
  `(cxml:with-element "iq"
    (cxml:attribute "id" (id ,iq-stanza))
    (unless (null (to stanza))
      (cxml:attribute "to" (to ,iq-stanza)))
    (unless (null (from stanza))
      (cxml:attribute "from" (from ,iq-stanza)))
    ,@body))

(defmethod make-stanza ((stanza iq-stanza) class-name)
  (let* ((xml-node (xml-node stanza))
         (iq-node (dom:first-child xml-node)))
    (xml-to-stanza (make-instance class-name
                                  :xml-node     xml-node
                                  :to           (dom:get-attribute iq-node "to")
                                  :from         (dom:get-attribute iq-node "from")
                                  :id           (dom:get-attribute iq-node "id")
                                  :iq-type      (dom:get-attribute iq-node "type")))))

(defmethod xml-to-stanza ((stanza iq-stanza))
  (let* ((xml-node (xml-node stanza))
         (iq-type  (dom:get-attribute (dom:first-child xml-node) "type")))
    (string-case iq-type
      ("result" (make-stanza stanza 'iq-result-stanza))
      ("error"  (make-stanza stanza 'iq-error-stanza)))))
      

(defclass iq-get-stanza (iq-stanza) ())
    
(defclass iq-set-stanza (iq-stanza) ())

(defmacro with-iq-set-stanza ((iq-set-stanza) &body body)
  `(with-iq-stanza (,iq-set-stanza)
     (cxml:attribute "type" "set")
     ,@body))
                    

(defclass iq-set-bind-stanza (iq-set-stanza)
  ((xmlns
    :reader xmlns
    :initform "urn:ietf:params:xml:ns:xmpp-bind")
   (resource
    :accessor resource
    :initarg :resource
    :initform "cl-ngxmpp")))

(defmethod stanza-to-xml ((stanza iq-set-bind-stanza))
  (with-iq-set-stanza (stanza)
    (cxml:with-element "bind"
      (cxml:attribute "xmlns" (xmlns stanza))
      (cxml:with-element "resource"
        (cxml:text (resource stanza))))))


(defclass iq-set-session-stanza (iq-set-stanza)
  ((xmlns
    :reader xmlns
    :initform "urn:ietf:params:xml:ns:xmpp-session")))

(defmethod stanza-to-xml ((stanza iq-set-session-stanza))
  (with-iq-set-stanza (stanza)
    (cxml:with-element "session"
      (cxml:attribute "xmlns" (xmlns stanza)))))


(defclass iq-result-stanza (iq-stanza) ())

(defmethod xml-to-stanza ((stanza iq-result-stanza))
  stanza)


(defclass iq-error-stanza (iq-stanza) ())

(defmethod xml-to-stanza ((stanza iq-error-stanza))
  stanza)

;;
;; TODO: move starttls, proceed, sasl, etc
;;       to corresponding files.
;;
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


(defclass sasl-stanza (stanza)
  ((xmlns
    :reader xmlns
    :initarg :xmlns
    :initform "urn:ietf:params:xml:ns:xmpp-sasl")))

(defmethod xml-to-stanza ((stanza sasl-stanza))
  (let* ((xml-node (xml-node stanza))
         (response-node (dom:first-child xml-node)))
    (setf (xmlns stanza) (dom:get-attribute response-node "xmlns"))
    stanza))


(defclass sasl-auth-stanza (sasl-stanza)
  ((mechanism
    :accessor mechanism
    :initarg :mechanism
    :initform "DIGEST-MD5")
   (identity-string
    :accessor identity-string
    :initarg :identity-string
    :initform nil)))
   
(defmethod stanza-to-xml ((stanza sasl-auth-stanza))
  (cxml:with-element "auth"
    (cxml:attribute "xmlns"     (xmlns stanza))
    (cxml:attribute "mechanism" (mechanism stanza))
    (unless (null (identity-string stanza))
      (cxml:text (identity-string stanza)))))


(defclass sasl-response-stanza (sasl-stanza)
  ((identity-string
    :accessor identity-string
    :initarg :identity-string
    :initform nil)))

(defmethod stanza-to-xml ((stanza sasl-response-stanza))
  (cxml:with-element "response"
    (cxml:attribute "xmlns" (xmlns stanza))
    (unless (null (identity-string stanza))
      (cxml:text (identity-string stanza)))))

(defclass sasl-challenge-stanza (sasl-stanza)
  ((identity-string
    :accessor identity-string
    :initarg :identity-string
    :initform nil)))

(defmethod print-object ((obj sasl-challenge-stanza) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "identity-string: ~A" (identity-string obj))))

(defmethod xml-to-stanza ((stanza sasl-challenge-stanza))
  (let ((xml-node (xml-node stanza)))
    (setf (identity-string stanza)
          (base64:base64-string-to-string
           (dom:data (dom:first-child (dom:first-child xml-node)))))
    stanza))
         
    
(defclass failure-stanza (stanza)
  ((xmlns
    :accessor xmlns
    :initarg :xmlns
    :initform "")))

(defmethod print-object ((obj failure-stanza) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "xmlns: ~A" (xmlns obj))))

(defmethod xml-to-stanza ((stanza failure-stanza))
  (setf (xmlns stanza) (dom:get-attribute (dom:first-child (xml-node stanza)) "xmlns"))
  stanza)
         

(defclass success-stanza (stanza)
  ((xmlns
    :accessor xmlns
    :initarg :xmlns
    :initform "")))

(defmethod xml-to-stanza ((stanza success-stanza))
  (setf (xmlns stanza) (dom:get-attribute (dom:first-child (xml-node stanza)) "xmlns"))
  stanza)

(defmethod print-object ((obj success-stanza) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "xmlns: ~A" (xmlns obj))))
