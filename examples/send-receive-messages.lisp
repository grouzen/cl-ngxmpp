;;;; send-receive-messages.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp-client.examples
  (:use #:cl #:cl-ngxmpp-client))

(in-package #:cl-ngxmpp-client.examples)

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp:presence-stanza))
  (write-line "Presence received."))

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp:iq-result-stanza))
  (write-line "Bind result stanza received."))

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp:message-stanza))
  (write-line (format nil "~A -> ~A: ~A"
                      (cl-ngxmpp::from stanza)
                      (cl-ngxmpp::to   stanza)
                      (cl-ngxmpp::body stanza))))

(defun run (&key server-hostname username password mechanism to body)
  (let ((client (create-client :server-hostname server-hostname)))
    (connect client)
    (authorize client :username username :password password :mechanism mechanism)
    (send-message client
                  :to to
                  :body body)
    (proceed-stanza-loop client) ;; wait for messages from opponent
    (disconnect client))) ;; close connection
