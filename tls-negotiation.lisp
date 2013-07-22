
(in-package #:cl-ngxmpp)

(defmethod negotiate-tls ((xml-stream xml-stream))
  (send-tls-negotiation xml-stream)
  (receive-tls-negotiation xml-stream))

(defmethod send-tls-negotiation ((xml-stream xml-stream))
  (with-stanza-output (xml-stream)
    (make-instance 'starttls-stanza)))

(defmethod receive-tls-negotiation ((xml-stream xml-stream))
  (with-stanza-input (xml-stream stanza-input)
    (cond ((typep stanza-input 'proceed-stanza) (proceed-tls-negotiation xml-stream))
          ((typep stanza-input 'failure-stanza) t)
          (t (error "Unexpected reply from TLS negotiation")))))

(defmethod proceed-tls-negotiation ((xml-stream xml-stream))
  (let ((connection (connection xml-stream)))
    (setf (socket-stream connection)
          (cl+ssl:make-ssl-client-stream
           (socket-stream connection)
           :external-format :iso-8859-1))
    (restart-stream xml-stream)))
