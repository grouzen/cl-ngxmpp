;;;; tls-negotiation.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

(define-condition negotiate-tls-condition (simple-condition) ())

(defmethod negotiate-tls ((xml-stream xml-stream))
  (send-tls-negotiation xml-stream)
  (receive-tls-negotiation xml-stream))

(defmethod send-tls-negotiation ((xml-stream xml-stream))
  (with-stanza-output (xml-stream)
    (make-instance 'starttls-stanza)))

(defmethod receive-tls-negotiation ((xml-stream xml-stream))
  (with-stanza-input (xml-stream stanza-input)
    (cond ((typep stanza-input 'proceed-stanza)
           (proceed-tls-negotiation xml-stream))
          ((typep stanza-input 'failure-stanza)
           (error 'negotiate-tls-condition
                  :format-control "TLS negotiation failed"))
          (t (error 'negotiate-tls-condition
                    :format-control "Unexpected reply from TLS negotiation")))))

(defmethod proceed-tls-negotiation ((xml-stream xml-stream))
  (let ((adapter (adapter (connection xml-stream))))
    (with-slots (socket-stream) adapter
      (setf socket-stream (cl+ssl:make-ssl-client-stream socket-stream :external-format '(:utf-8 :eol-style :crlf)))
      (restart-stream xml-stream))))
