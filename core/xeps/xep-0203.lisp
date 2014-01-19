;;;; xep-0203.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

(defun delayed-delivery-dispatcher (stanza)
  (get-element-by-name (dom:first-child (xml-node stanza)) "delay"))

(define-xep (delayed-delivery :order "0203"
                              :author "Nedokushev Michael <grouzen.hexy@gmail.com>"
                              :description "XEP 0203, Delayed Delivery"
                              :depends-on (multi-user-chat))

  ((root-stanza ()
     ((description :accessor description :initarg :description :initform nil)           
      (delay-from  :accessor delay-from  :initarg :delay-from  :initform "")
      (stamp       :accessor stamp       :initarg :stamp       :initform "")
      (delay-xmlns :accessor delay-xmlns :initarg :delay-xmlns :initform "urn:xmpp:delay"))

     (:methods
      ((xml-to-stanza (stanza)
         (let* ((delay-node  (get-element-by-name (dom:first-child (xml-node stanza)) "delay"))
                (from        (dom:get-attribute delay-node "from"))
                (stamp       (dom:get-attribute delay-node "stamp"))
                (description (get-element-data delay-node)))
           (setf (description stanza) description
                 (delay-from  stanza) from
                 (stamp       stanza) stamp)
           stanza)))))

   (message-stanza (delayed-delivery-root-stanza
                    message-stanza)
     ()

     (:methods
      ((xml-to-stanza (stanza)
         (call-next-method stanza)))

      :dispatcher ((stanza)
        (delayed-delivery-dispatcher stanza))))
   
   
   (message-groupchat-stanza (delayed-delivery-root-stanza
                              multi-user-chat-message-groupchat-stanza)
     ()

     (:methods
      ((xml-to-stanza (stanza)
         (call-next-method stanza)))

      :dispatcher ((stanza)
        (delayed-delivery-dispatcher stanza))))))
