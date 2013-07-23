
(defpackage #:cl-ngxmpp-client.examples
  (:use #:cl #:cl-ngxmpp-client))

(in-package #:cl-ngxmpp-client.examples)

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp::message-stanza))
  (write-line (format nil "~A -> ~A: ~A~%"
                      (cl-ngxmpp::from stanza)
                      (cl-ngxmpp::to   stanza)
                      (cl-ngxmpp::body stanza))))

(defun run (&key server-hostname username password to body)
  (let ((client (create-client :server-hostname server-hostname)))
    (connect client)
    (authorize client :username username :password password)
    (send-message client
                  :to to
                  :body body)
    (proceed-stanza client) ;; wait for message from opponent
    (disconnect client))) ;; close connection
