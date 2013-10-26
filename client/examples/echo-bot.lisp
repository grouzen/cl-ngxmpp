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

(define-stanza-handler ((stanza presence-stanza))
  (let ((from (cl-ngxmpp:from stanza))
        (to   (cl-ngxmpp:to stanza))
        (show (cl-ngxmpp:show stanza)))
    (write-line (format nil "Presence ~A -> ~A: ~A" from to show))))

(define-stanza-handler ((stanza iq-get-stanza))
  (let ((from (cl-ngxmpp:from stanza))
        (to   (cl-ngxmpp:to   stanza))
        (id   (cl-ngxmpp:id   stanza))
        (iq-type (cl-ngxmpp:iq-type stanza)))
    (write-line (format nil "IQ ~A (~A) ~A -> ~A" id iq-type from to))))

(define-stanza-handler ((stanza iq-result-stanza))
  (let ((id (cl-ngxmpp:id stanza))
        (iq-type (cl-ngxmpp:iq-type stanza))
        (to (cl-ngxmpp:to stanza))
        (from (cl-ngxmpp:from stanza)))
    (write-line (format nil "IQ ~A (~A) ~A -> ~A" id iq-type from to))))

(define-stanza-handler ((stanza message-stanza))
  (let ((from (cl-ngxmpp:from stanza))
        (to   (cl-ngxmpp:to   stanza))
        (body (cl-ngxmpp:body stanza)))
    (write-line (format nil "~A -> ~A: ~A" from to body))
    (cl-ngxmpp-client:send-message *client*
                  :to from
                  :body (format nil "Are you talking to me? Are you talking to me?! Walk on home boy!"))))

(defun run (&key server-hostname username password mechanism to body)
  (unless (null *client*)
    (cl-ngxmpp-client:disconnect *client*))
  (setf *client* (make-instance 'cl-ngxmpp-client:client :server-hostname server-hostname))
  (cl-ngxmpp-client:connect *client*)
  (cl-ngxmpp-client:authorize *client* :username username :password password :mechanism mechanism)
  (cl-ngxmpp-client:send-message *client* :to to :body body)
  ;; Wait for messages from opponent
  (cl-ngxmpp-client:proceed-stanza-loop *client*))
