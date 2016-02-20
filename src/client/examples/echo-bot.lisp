;;;; echo-bot.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(defpackage #:cl-ngxmpp-client.examples.echo-bot
  (:use #:cl)
  (:export #:run))

(in-package #:cl-ngxmpp-client.examples.echo-bot)

(defun run (&key server-hostname username password mechanism to message)
  (labels ((handle-stanzas (xmpp-client)
             (let ((stanza (xmpp:receive-stanza xmpp-client)))
               (when (typep stanza 'xmpp%:message-stanza)
                 (let ((to   (xmpp%::to   stanza))
                       (from (xmpp%::from stanza))
                       (body (xmpp%::body stanza)))
                   (if (string= body "stop talking")
                       (progn
                         (xmpp:send-stanza xmpp-client 'xmpp%:message-stanza
                                           :to from :body "Thanks for talking with me :)")
                         (xmpp:disconnect-client xmpp-client))
                       (xmpp:send-stanza xmpp-client 'xmpp%:message-stanza
                                         :to from
                                         :body (format nil ">> ~A" body)))))
               (when (xmpp:connectedp xmpp-client)
                 (handle-stanzas xmpp-client)))))
    
    (let ((xmpp-client (make-instance 'xmpp:client :debuggable t)))
      (xmpp:connect-client xmpp-client :server-hostname server-hostname)
      (when (xmpp:connectedp xmpp-client)
        (xmpp:login-client xmpp-client
                           :username username
                           :password password
                           :mechanism mechanism)
        (when (xmpp:loggedinp xmpp-client)
          (xmpp:send-stanza xmpp-client 'xmpp%:message-stanza :to to :body message)
          (xmpp:send-stanza xmpp-client 'xmpp%:message-stanza :to to
                            :body "To end up the session, send me a message 'stop talking'")
          (handle-stanzas xmpp-client))))))

