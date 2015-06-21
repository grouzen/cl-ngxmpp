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

(xmpp:use-xeps '("delayed-delivery"
                             "multi-user-chat"))

;;
;; Standard set of handlers.
;;
(define-stanza-handler ((stanza stanza))
  (write-line "Default handler."))

(define-stanza-handler ((stanza presence-show-stanza))
  (let ((from (xmpp%:from stanza))
        (to   (xmpp%:to stanza))
        (show (xmpp%:show stanza)))
    (write-line (format nil "Presence ~A -> ~A: ~A" from to show))))

(define-stanza-handler ((stanza presence-subscribe-stanza))
  (let ((from   (xmpp%:from stanza))
        (status (xmpp%:status stanza)))
    (write-line (format nil "Presence ~A wants to subscribe to you, with status ~A"
                        from status))))
          
(define-stanza-handler ((stanza iq-get-stanza))
  (let ((from (xmpp%:from stanza))
        (to   (xmpp%:to   stanza))
        (id   (xmpp%:id   stanza))
        (iq-type (xmpp%:iq-type stanza)))
    (write-line (format nil "IQ ~A (~A) ~A -> ~A" id iq-type from to))))

(define-stanza-handler ((stanza iq-result-stanza))
  (let ((id (xmpp%:id stanza))
        (iq-type (xmpp%:iq-type stanza))
        (to (xmpp%:to stanza))
        (from (xmpp%:from stanza)))
    (write-line (format nil "IQ ~A (~A) ~A -> ~A" id iq-type from to))))

;;
;; XEP (Multi User Chat) related handlers.
;;
(define-stanza-handler ((stanza message-groupchat-stanza) :xep multi-user-chat)
  (let ((body (xmpp%:body stanza))
        (from (xmpp%:from stanza))
        (to   (xmpp%:to   stanza)))
    (write-line (format nil "MUC message: ~A -> ~A: ~A" from to body))))

(define-stanza-handler ((stanza message-groupchat-stanza) :xep delayed-delivery)
  (write-line (format nil "MUC delayed message: ~A: ~A"
                      (xmpp%:stamp stanza) (xmpp%:delay-from stanza))))

(define-stanza-handler ((stanza presence-user-stanza) :xep multi-user-chat)
  (let ((affiliation (xmpp%:affiliation stanza))
        (role        (xmpp%:role stanza))
        (from        (xmpp%:from stanza))
        (to          (xmpp%:to   stanza)))
    (write-line (format nil "MUC User presence: ~A -> ~A, affil: ~A, role: ~A"
                        from to affiliation role))))

(define-stanza-handler ((stanza presence-user-self-stanza) :xep multi-user-chat)
  (write-line (format nil "MUC user self presence, roster ends: ~A" (xmpp%:statuses stanza))))
     
(defun connect (&key server-hostname username password)
  (unless (null *client*)
    (xmpp:disconnect *client*))
  (setf *client* (make-instance 'xmpp:client :server-hostname server-hostname))
  (xmpp:connect *client*)
  (xmpp:authorize *client* :username username :password password))
  

(defun join-room (&key conference nickname)
  (xmpp:call-methods-with-xep (multi-user-chat)
    ((send-presence-join *client*
                         :conference conference
                         :nickname nickname)))
  (xmpp:proceed-stanza-loop *client*))

(defun exit-room (&key conference nickname)
  (xmpp:call-methods-with-xep (multi-user-chat)
    ((send-presence-exit *client*
                         :conference conference
                         :nickname nickname)))
  (xmpp:proceed-stanza-loop *client*))
