;;;; xep-0045.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client)

(define-methods-with-xep (muc)

  ((send-message ((client client) &key to body)
       (send-message client :to to :body))

   (send-invite ((client client) &key to)
       ())))

;;
;; Example, how to call send methods from xep
;; (call-methods-with-xep (muc)
;;   ((send-message client :to to :body body)
;;    (send-invite  clinet :to to)))
;;
