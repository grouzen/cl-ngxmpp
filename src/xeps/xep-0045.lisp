;;;; xep-0045.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-xeps)

(define-xep (multi-user-chat :order "0045"
                             :description "XEP 0045, Multi User Chat")
    
  ((message-groupchat-stanza (message-stanza)
     ()

     (:methods
      ((xml-to-stanza ((stanza) dispatchers)
         (let ((disp (dispatch-stanza stanza 'multi-user-chat-message-groupchat-stanza dispatchers)))
           (if (typep disp 'unknown-stanza)
               stanza
               disp)))

       (stanza-to-xml ((stanza))
         (with-message-stanza (stanza))))

      :dispatcher ((stanza)
        (let* ((message-node (dom:first-child (xml-node stanza)))
               (stanza-type  (dom:get-attribute message-node "type")))
          (equalp stanza-type "groupchat")))))
               

   (presence-join-stanza (presence-stanza)
     ((x-xmlns "http://jabber.org/protocol/muc"))
     
     (:methods
      ((stanza-to-xml ((stanza))
         (with-presence-stanza (stanza)
           (cxml:with-element "x"
             (cxml:attribute "xmlns" (x-xmlns stanza))))))))

   
   (presence-exit-stanza (presence-stanza)
     ((stanza-type "unavailable"))
     
     (:methods
      ((stanza-to-xml ((stanza))
         (call-next-method stanza)))))

   (presence-user-stanza (presence-stanza)
     ((x-xmlns     "http://jabber.org/protocol/muc#user")
      (affiliation "member")
      (role        "participant")
      jid)
     
     (:methods
      ((xml-to-stanza ((stanza) dispatchers)
         (let* ((x-node      (get-x-node stanza))
                (item-node   (get-element-by-name x-node "item"))
                (affiliation (dom:get-attribute item-node "affiliation"))
                (role        (dom:get-attribute item-node "role"))
                (disp        (dispatch-stanza stanza 'multi-user-chat-presence-user-stanza dispatchers)))
           (if (typep disp 'unknown-stanza)
               (progn
                 (setf (affiliation stanza) affiliation
                       (role        stanza) role)
                 stanza)
               disp)))

       (make-stanza ((stanza) class-name)
         (let* ((item-node   (get-element-by-name (get-x-node stanza) "item"))
                (affiliation (dom:get-attribute item-node "affiliation"))
                (role        (dom:get-attribute item-node "role")))
           (xml-to-stanza (make-instance 'multi-user-chat-presence-user-self-stanza
                                         :xml-node    (xml-node stanza)
                                         :to          (to stanza)
                                         :from        (from stanza)
                                         :id          (id stanza)
                                         :affiliation affiliation
                                         :role        role)
                          dispatchers)))

       ;; Helper for searhing "x" element with particular xmlns attr.
       (get-x-node ((stanza))
         (let ((xs (remove-if #'(lambda (x-node)
                                  (not (equalp (dom:get-attribute x-node "xmlns")
                                               "http://jabber.org/protocol/muc#user")))
                              (get-elements-by-name (dom:first-child (xml-node stanza)) "x"))))
           (when xs
             (car xs)))))
      
      :dispatcher ((stanza)
        (get-x-node (make-instance 'multi-user-chat-presence-user-stanza
                                   :xml-node (xml-node stanza))))))
   
   (presence-user-self-stanza (multi-user-chat-presence-user-stanza)
     (statuses)

     (:methods
      ((xml-to-stanza ((stanza) dispatchers)
         (let* ((x-node       (get-element-by-name (dom:first-child (xml-node stanza)) "x"))
                (status-nodes (get-elements-by-name x-node "status")))
           (loop :for status-node :in status-nodes
              :do (push (dom:get-attribute status-node "code") (statuses stanza)))
           stanza)))

      :dispatcher ((stanza)
        (let ((x-node (get-element-by-name (dom:first-child (xml-node stanza)) "x")))
          (get-elements-by-name x-node "status")))))))
