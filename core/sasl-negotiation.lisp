;;;; sasl-negotiation.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package :cl-ngxmpp)

;;
;; Because cl-sasl library doesn't support SCRAM-SHA-1 and SCRAM-SHA-1-PLUS
;; mechanisms we are forced to support old and week PLAIN and DIGEST-MD5
;;
;; Ordered list of SASL mechanisms.
;;
(defparameter *sasl-mandatory-mechanisms*
  '("DIGEST-MD5" "PLAIN"))

(define-condition negotiate-sasl-error (simple-condition)
  ((failure-stanza :accessor failure-stanza :initarg :failure-stanza :initform nil)))

(defmethod negotiate-sasl ((xml-stream xml-stream) &key username password mechanism)
  (let* ((sasl-mechanism (sasl:get-mechanism (%choose-mechanism% mechanism)))
         (sasl-client (make-instance sasl-mechanism
                                     :authentication-id username
                                     :password password
                                     :service "xmpp"
                                     :host (hostname (connection xml-stream))))
         (negotiation-result (string-case sasl-mechanism
                               ("PLAIN" (%sasl-plain-negotiation% xml-stream sasl-client))
                               ("DIGEST-MD5" (%sasl-digest-md5-negotiation% xml-stream sasl-client)))))
    (cond ((typep negotiation-result 'success-stanza)
           (restart-stream xml-stream)
           (setf (state xml-stream) 'sasl-negotiated))
          ((typep negotiation-result 'failure-stanza)
           (%sasl-fail% negotiation-result)))))

;; TODO: improve algorithm, now it's so dumb.
(defun %choose-mechanism% (mechanism)
  (if (null mechanism)
      (car *sasl-mandatory-mechanisms*)
      mechanism))

(defun %sasl-fail% (failure)
  (error (make-condition 'negotiate-sasl-error
                         :failure-stanza failure
                         :format-control "SASL failed: ~A"
                         :format-arguments (list failure))))

(defmethod %sasl-plain-negotiation% ((xml-stream xml-stream) sasl-client)
  (print (cl-sasl::password sasl-client))
  (let ((step-response (base64:usb8-array-to-base64-string
                        ;; BUG: I found a bug in cl-sasl.
                        ;;      client-step throws error about server-input is nil,
                        ;;      but in client-step server-input argument is ignored.
                        (sasl:client-step sasl-client nil))))
    (with-stanza-output (xml-stream)
      (make-instance 'sasl-auth-stanza
                     :mechanism "PLAIN"
                     :identity-string step-response))
    (with-stanza-input (xml-stream success-stanza)
      success-stanza)))

(defmethod %sasl-digest-md5-negotiation% ((xml-stream xml-stream) sasl-client)
  (with-stanza-output (xml-stream)
    (make-instance 'sasl-auth-stanza :mechanism "DIGEST-MD5"))
  (with-stanza-input (xml-stream first-challenge-stanza)
    (cond ((typep first-challenge-stanza 'sasl-challenge-stanza)
           (let ((response (base64:usb8-array-to-base64-string
                            (sasl:client-step
                             sasl-client
                             (babel:string-to-octets (identity-string first-challenge-stanza))))))
             (with-stanza-output (xml-stream) ;; Send <response/>
               (make-instance 'sasl-response-stanza :identity-string response))
             (with-stanza-input (xml-stream second-challenge-stanza) ;; Receive second <challenge/>
               (cond ((typep second-challenge-stanza 'sasl-challenge-stanza)
                      (with-stanza-output (xml-stream) ;; Send second and last <response/>
                        (make-instance 'sasl-response-stanza))
                      (with-stanza-input (xml-stream success-stanza) ;; Receive <success/>
                        success-stanza))
                     ((typep second-challenge-stanza 'failure-stanza) (%sasl-fail% second-challenge-stanza))))))
          ((typep first-challenge-stanza 'failure-stanza) (%sasl-fail% first-challenge-stanza)))))
