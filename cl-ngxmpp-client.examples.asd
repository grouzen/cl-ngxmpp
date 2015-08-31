;;;; cl-ngxmpp-client.examples.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-user)

(asdf:defsystem #:cl-ngxmpp-client.examples
  :name "cl-ngxmpp-client.examples"
  :author "Michael Nedokushev <michael.nedokushev@gmail.com>"
  :license "Lisp-LGPL"
  :depends-on (:cl-ngxmpp-client)
  :components ((:module "src/client/examples"
                        :components ((:file "echo-bot")))))

