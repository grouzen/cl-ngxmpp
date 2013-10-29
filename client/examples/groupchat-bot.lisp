;;;; groupchat-bot.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp-client.examples.groupchat-bot
  (:use #:cl)
  (:import-from #:cl-ngxmpp-client #:define-stanza-handler)
  (:export #:connect #:join-room #:exit-room))

(in-package #:cl-ngxmpp-client.examples.groupchat-bot)

(defvar *client* nil)

(cl-ngxmpp-client:use-xeps "multi-user-chat")

(define-stanza-handler ((stanza presence-user-stanza) :xep multi-user-chat)
  (let ((affiliation (cl-ngxmpp:affiliation stanza))
        (role       (cl-ngxmpp:role stanza))
        (from       (cl-ngxmpp:from stanza))
        (to         (cl-ngxmpp:to   stanza)))
    (write-line (format nil "MUC User presence: ~A -> ~A, affil: ~A, role: ~A"
                        from to affiliation role))))
        
(defun connect (&key server-hostname username password)
  (unless (null *client*)
    (cl-ngxmpp-client:disconnect *client*))
  (setf *client* (make-instance 'cl-ngxmpp-client:client :server-hostname server-hostname))
  (cl-ngxmpp-client:connect *client*)
  (cl-ngxmpp-client:authorize *client* :username username :password password))
  

(defun join-room (&key conference nickname)
  (cl-ngxmpp-client:call-methods-with-xep (multi-user-chat)
    ((send-presence-join *client*
                         :conference conference
                         :nickname nickname)))
  (cl-ngxmpp-client:proceed-stanza-loop *client*))

(defun exit-room (&key conference nickname)
  (cl-ngxmpp-client:call-methods-with-xep (multi-user-chat)
    ((send-presence-exit *client*
                         :conference conference
                         :nickname nickname)))
  (cl-ngxmpp-client:proceed-stanza-loop *client*))
