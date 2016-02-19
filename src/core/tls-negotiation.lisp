;;;; tls-negotiation.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza starttls-element (meta-element)
    ((xmlns "urn:ietf:params:xml:ns:xmpp-tls"))
  
  (stanza-to-xml ((stanza))
    (cxml:with-element "starttls"
      (cxml:attribute "xmlns" (xmlns stanza))))

  (xml-to-stanza ((stanza) dispatchers)
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza proceed-element (meta-element)
    ()

  (xml-to-stanza ((stanza) dispatchers)
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition negotiate-tls-error (simple-condition)
  ((failure-stanza :accessor failure-stanza :initarg :failure-stanza :initform nil)))

(defun %tls-fail% (failure)
  (error (make-condition 'negotiate-tls-error
                         :failure-stanza failure
                         :format-control "TLS failied: ~A"
                         :format-arguments (list failure))))

(defmethod negotiate-tls ((xml-stream xml-stream))
  (send-tls-negotiation xml-stream)
  (receive-tls-negotiation xml-stream))

(defmethod send-tls-negotiation ((xml-stream xml-stream))
  (with-stanza-output (xml-stream)
    (make-instance 'starttls-element)))

(defmethod receive-tls-negotiation ((xml-stream xml-stream))
  ;; We can omit dispatchers parameter here, because the xmpp protocol defines that
  ;; it is an error when an incoming stanza is not a proceed-element stanza,
  ;; so we don't care about dispatching in this case.
  (with-stanza-input (xml-stream stanza-input nil)
    (cond ((typep stanza-input 'proceed-element)
           (proceed-tls-negotiation xml-stream))
          (t (%tls-fail% stanza-input)))))

(defmethod proceed-tls-negotiation ((xml-stream xml-stream))
  (let ((adapter (adapter (connection xml-stream))))
    (with-slots (socket-stream) adapter
      (setf socket-stream (cl+ssl:make-ssl-client-stream socket-stream :external-format '(:utf-8 :eol-style :crlf)))
      (restart-stream xml-stream)
      (setf (state xml-stream) 'tls-negotiated))))
