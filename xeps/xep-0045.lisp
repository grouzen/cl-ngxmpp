;;;; xep-0045.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

(define-xep (multi-user-chat :order "0045"
                             :author "Nedokushev Michael <grouzen.hexy@gmail.com>"
                             :description "XEP 0045, Multi User Chat")
  
  ((presence-join-stanza (presence-stanza)
     ((x-xmlns :accessor x-xmlns :initarg :x-xmlns :initform "http://jabber.org/protocol/muc"))
     
     (:methods
      ((stanza-to-xml (stanza)
         (with-presence-stanza (stanza)
           (cxml:with-element "x"
             (cxml:attribute "xmlns" (x-xmlns stanza))))))))

   (presence-exit-stanza (presence-stanza)
     ()
     
     (:methods
      ((stanza-to-xml (stanza)
         (call-next-method stanza)))))

   (presence-user-stanza (presence-stanza)
     ((x-xmlns     :accessor x-xmlns     :initarg :x-xmlns     :initform "http://jabber.org/protocol/muc#user")
      (affiliation :accessor affiliation :initarg :affiliation :initform "member")
      (role        :accessor role        :initarg :role        :initform "participant"))

     (:methods
      ((xml-to-stanza (stanza)
         (let* ((xml-node    (xml-node stanza))
                (x-node      (get-element-by-name (dom:first-child xml-node) "x"))
                (item-node   (get-element-by-name x-node "item"))
                (affiliation (dom:get-attribute item-node "affiliation"))
                (role        (dom:get-attribute item-node "role"))
                (disp        (dispatch-stanza stanza 'multi-user-chat-presence-user-stanza)))
           (if (typep disp 'unknown-stanza)
               (progn
                 (setf (affiliation stanza) affiliation
                       (role        stanza) role)
                 stanza)
               disp)))

       (make-stanza (stanza class-name)
         (let* ((item-node   (get-element-by-name 
                              (get-element-by-name (dom:first-child (xml-node stanza)) "x")
                              "item"))
                (affiliation (dom:get-attribute item-node "affiliation"))
                (role        (dom:get-attribute item-node "role")))
           (xml-to-stanza (make-instance 'multi-user-chat-presence-user-self-stanza
                                         :xml-node    (xml-node stanza)
                                         :to          (to stanza)
                                         :from        (from stanza)
                                         :id          (id stanza)
                                         :affiliation affiliation
                                         :role        role)))))
      
      :dispatcher ((stanza)
        (let* ((presence-node (dom:first-child (xml-node stanza)))
               (x-node        (get-element-by-name presence-node "x")))
          (unless (null x-node)
            (equalp (dom:get-attribute x-node "xmlns") "http://jabber.org/protocol/muc#user"))))))

   (presence-user-self-stanza (multi-user-chat-presence-user-stanza)
     ((statuses :accessor statuses :initarg :statuses :initform nil))

     (:methods
      ((xml-to-stanza (stanza)
         (let* ((x-node       (get-element-by-name (dom:first-child (xml-node stanza)) "x"))
                (status-nodes (get-elements-by-name x-node "status")))
           (loop :for status-node :in status-nodes
              :do (push (dom:get-attribute status-node "code") (statuses stanza)))
           stanza)))

      :dispatcher ((stanza)
        (let ((x-node (get-element-by-name (dom:first-child (xml-node stanza)) "x")))
          (get-elements-by-name x-node "status")))))))

