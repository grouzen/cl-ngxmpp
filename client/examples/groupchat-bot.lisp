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

(cl-ngxmpp-client:use-xeps "delayed-delivery" "multi-user-chat")

;;
;; Standard set of handlers.
;;
(define-stanza-handler ((stanza stanza))
  (write-line "Default handler."))

(define-stanza-handler ((stanza presence-show-stanza))
  (let ((from (cl-ngxmpp:from stanza))
        (to   (cl-ngxmpp:to stanza))
        (show (cl-ngxmpp:show stanza)))
    (write-line (format nil "Presence ~A -> ~A: ~A" from to show))))

(define-stanza-handler ((stanza presence-subscribe-stanza))
  (let ((from   (cl-ngxmpp:from stanza))
        (status (cl-ngxmpp:status stanza)))
    (write-line (format nil "Presence ~A wants to subscribe to you, with status ~A"
                        from status))))
          
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

;;
;; XEP (Multi User Chat) related handlers.
;;
(define-stanza-handler ((stanza message-groupchat-stanza) :xep multi-user-chat)
  (let ((body (cl-ngxmpp:body stanza))
        (from (cl-ngxmpp:from stanza))
        (to   (cl-ngxmpp:to   stanza)))
    (write-line (format nil "MUC message: ~A -> ~A: ~A" from to body))))

(define-stanza-handler ((stanza message-groupchat-stanza) :xep delayed-delivery)
  (write-line (format nil "MUC delayed message: ~A: ~A"
                      (cl-ngxmpp:stamp stanza) (cl-ngxmpp:delay-from stanza))))

(define-stanza-handler ((stanza presence-user-stanza) :xep multi-user-chat)
  (let ((affiliation (cl-ngxmpp:affiliation stanza))
        (role        (cl-ngxmpp:role stanza))
        (from        (cl-ngxmpp:from stanza))
        (to          (cl-ngxmpp:to   stanza)))
    (write-line (format nil "MUC User presence: ~A -> ~A, affil: ~A, role: ~A"
                        from to affiliation role))))

(define-stanza-handler ((stanza presence-user-self-stanza) :xep multi-user-chat)
  (write-line (format nil "MUC user self presence, roster ends: ~A" (cl-ngxmpp:statuses stanza))))
     
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
