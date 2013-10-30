;;;; xep-0045.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client)

(define-methods-with-xep (multi-user-chat)

  ((send-presence-join ((client client) &key conference nickname)
     (let ((xml-stream (xml-stream client))
           (to         (format nil "~A/~A" conference nickname))
           (from       (jid client)))
       (cl-ngxmpp:with-stanza-output (xml-stream)
         (make-instance-with-xep (multi-user-chat)
             'presence-join-stanza :to to :from from))))

   (send-presence-exit ((client client) &key conference nickname)
     (let ((to   (format nil "~A/~A" conference nickname))
           (from (jid client)))
       (cl-ngxmpp:with-stanza-output ((xml-stream client))
         (make-instance-with-xep (multi-user-chat)
             'presence-exit-stanza :presence-type "unavailable" :to to :from from))))))
             
