;;;; cl-ngxmpp-client.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-user)

(asdf:defsystem #:cl-ngxmpp-client
  :name "cl-ngxmpp-client"
  :author "Michael Nedokushev <grouzen.hexy@gmail.com>"
  :license "Lisp-LGPL"
  :depends-on (:cl-ngxmpp)
  :serial t
  :components ((:module "client"
                        :serial t
                        :components ((:file "package")
                                     (:file "client")
                                     (:module "xeps"
                                              :components ((:file "xeps")
                                                           (:file "xep-0045"))
                                              :serial t)
                                     (:module "high"
                                              :serial t
                                              :components ((:file "session")))))))


