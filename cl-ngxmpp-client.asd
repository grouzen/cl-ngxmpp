;;;; cl-ngxmpp-client.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-user)

(asdf:defsystem #:cl-ngxmpp-client
  :name "cl-ngxmpp-client"
  :author "Michael Nedokushev <michael.nedokushev@gmail.com>"
  :license "Lisp-LGPL"
  :depends-on (:cl-ngxmpp :cl-ngxmpp-xeps)
  :serial t
  :components ((:module "src/client"
                        :serial t
                        :components ((:file "package")
                                     (:file "client")
                                     (:file "session")
                                     (:file "domain")))))


