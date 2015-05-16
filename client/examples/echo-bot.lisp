;;;; echo-bot.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp-client.examples.echo-bot
  (:use #:cl)
  (:import-from #:cl-ngxmpp-client #:define-stanza-handler)
  (:export #:run))

(in-package #:cl-ngxmpp-client.examples.echo-bot)

(defvar *client* nil)

(define-stanza-handler ((stanza stanza))
  (write-line "Default handler."))

(define-stanza-handler ((stanza presence-show-stanza))
  (let ((from (xmpp%::from stanza))
        (to   (xmpp%::to stanza))
        (show (xmpp%::show stanza)))
    (write-line (format nil "Presence ~A -> ~A: ~A" from to show))))

(define-stanza-handler ((stanza presence-subscribe-stanza))
  (let ((from   (xmpp%::from stanza))
        (status (xmpp%::status stanza)))
    (write-line (format nil "Presence ~A wants to subscribe to you, with status ~A"
                        from status))))
          
(define-stanza-handler ((stanza iq-get-stanza))
  (let ((from (xmpp%::from stanza))
        (to   (xmpp%::to   stanza))
        (id   (xmpp%::id   stanza))
        (iq-type (xmpp%::iq-type stanza)))
    (write-line (format nil "IQ ~A (~A) ~A -> ~A" id iq-type from to))))

(define-stanza-handler ((stanza iq-result-stanza))
  (let ((id (xmpp%::id stanza))
        (iq-type (xmpp%::iq-type stanza))
        (to (xmpp%::to stanza))
        (from (xmpp%::from stanza)))
    (write-line (format nil "IQ ~A (~A) ~A -> ~A" id iq-type from to))))

(define-stanza-handler ((stanza message-stanza))
  (let ((from (xmpp%::from stanza))
        (to   (xmpp%::to   stanza))
        (body (xmpp%::body stanza)))
    (write-line (format nil "~A -> ~A: ~A" from to body))
    (if (string= body "stop talking")
        (progn 
          (xmpp:send-message *client* :to from :body "Thanks for talking with me ;-)")
          (xmpp:disconnect *client*))
        (xmpp:send-message *client*
                                       :to from
                                       :body (format nil ">> ~A" body)))))

(defun run (&key server-hostname username password mechanism to body)
  (unless (null *client*)
    (xmpp:disconnect *client*))
  (setf *client* (make-instance 'xmpp:client
                                :server-hostname server-hostname))
  (xmpp:connect *client*)
  (xmpp:authorize *client* :username username :password password :mechanism mechanism)
  (xmpp:send-message *client* :to to :body body)
  (xmpp:send-message
   *client*
   :to to
   :body "To end up the session, send a message: 'stop talking'")
  ;; Wait for messages from your opponent
  (xmpp:proceed-stanza-loop *client*))
