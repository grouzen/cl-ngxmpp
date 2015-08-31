;;;; cl-ngxmpp-client-test.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-user)

(asdf:defsystem #:cl-ngxmpp-client-test
  :name "cl-ngxmpp-test"
  :author "Michael Nedokushev <michael.nedokushev@gmail.com>"
  :depends-on (:lift :cl-ngxmpp-client)
  :license "Lisp-LGPL"
  :serial t
  :components ((:module "tests/client"
                        :serial t
                        :components ((:file "package")
                                     (:file "suite")
                                     (:file "client-test")
                                     (:file "session-test")))))

