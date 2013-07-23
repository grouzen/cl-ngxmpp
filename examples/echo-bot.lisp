;;;; echo-bot.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp-client.examples.echo-bot
  (:use #:cl))

(in-package #:cl-ngxmpp-client.examples.echo-bot)

(defvar *client* nil)

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp:presence-stanza))
  (write-line "Presence received."))

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp:iq-get-stanza))
  (write-line "Received iq get stanza"))

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp:iq-result-stanza))
  (write-line "Bind result stanza received."))

(defmethod cl-ngxmpp:handle-stanza ((stanza cl-ngxmpp:message-stanza))
  (write-line (format nil "~A -> ~A: ~A"
                      (cl-ngxmpp::from stanza)
                      (cl-ngxmpp::to   stanza)
                      (cl-ngxmpp::body stanza)))
  (cl-ngxmpp-client:send-message *client*
                                 :to (cl-ngxmpp::from stanza)
                                 :body (format nil "You said to me:~%> ~A" (cl-ngxmpp::body stanza))))

(defun run (&key server-hostname username password mechanism to body)
  (unless (null *client*)
    (cl-ngxmpp-client:disconnect *client*))
  (setf *client* (cl-ngxmpp-client:create-client :server-hostname server-hostname))
  (cl-ngxmpp-client:connect *client*)
  (cl-ngxmpp-client:authorize *client* :username username :password password :mechanism mechanism)
  (cl-ngxmpp-client:send-message *client*
                                 :to to
                                 :body body)
  (cl-ngxmpp-client:proceed-stanza-loop *client*)) ;; wait for messages from opponent
