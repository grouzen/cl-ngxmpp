
(in-package :cl-ngxmpp)

;;
;; Because cl-sasl library doesn't support SCRAM-SHA-1 and SCRAM-SHA-1-PLUS
;; mechanisms we are forced to support old and week PLAIN and DIGEST-MD5
;;
;; Ordered list of SASL mechanisms.
;;
(defconstant +sasl-mandatory-mechanisms+
  '("DIGEST-MD5" "PLAIN"))

(defmethod negotiate-sasl ((xml-stream xml-stream) &key username password)
  (send-sasl-negotiation xml-stream)
  (receive-sasl-negotiation xml-stream :username username :password password))

(defmethod send-sasl-negotiation ((xml-stream xml-stream))
  (with-stanza-output (xml-stream)
    (make-instance 'sasl-auth-stanza)))

(defmethod receive-sasl-negotiation ((xml-stream xml-stream) &key username password)
  (let ((sasl-client (make-instance (sasl:get-mechanism (car +sasl-mandatory-mechanisms+))
                                    :authentication-id username
                                    :password password
                                    :service "xmpp"
                                    :host (hostname (connection xml-stream)))))
    (flet ((sasl-fail (failure) (error "SASL failed: ~A" failure)))
      (with-stanza-input (xml-stream first-challenge)
        (cond ((typep first-challenge 'sasl-challenge-stanza)
               (let ((response (base64:usb8-array-to-base64-string
                                (sasl:client-step
                                 sasl-client
                                 (babel:string-to-octets (identity-string first-challenge))))))
                 (with-stanza-output (xml-stream) ;; Send <response/>
                   (make-instance 'sasl-response-stanza :identity-string response))
                 (with-stanza-input (xml-stream second-challenge) ;; Receive second <challenge/>
                   (cond ((typep second-challenge 'sasl-challenge-stanza)
                          (with-stanza-output (xml-stream) ;; Send second and last <response/>
                            (make-instance 'sasl-response-stanza))
                          (with-stanza-input (xml-stream auth-success) ;; Receive <success/>
                            (restart-stream xml-stream)
                            auth-success))
                         ((typep second-challenge 'failure-stanza) (sasl-fail second-challenge))
                         (t nil)))))
              ((typep first-challenge 'failure-stanza) (sasl-fail first-challenge))
              (t nil))))))
        
