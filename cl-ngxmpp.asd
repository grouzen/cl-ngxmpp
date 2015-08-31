;;;; cl-ngxmpp.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-user)

(asdf:defsystem #:cl-ngxmpp
  :name "cl-ngxmpp"
  :author "Michael Nedokushev <michael.nedokushev@gmail.com>"
  :license "Lisp-LGPL"
  :depends-on (:blackbird :alexandria :usocket :cxml :babel :cl+ssl :cl-base64 :cl-sasl)
  :serial t
  :components ((:module "src/core"
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "connection")
                                     (:file "xml-stream")
                                     (:file "stanzas")
                                     (:file "tls-negotiation")
                                     (:file "sasl-negotiation")
                                     (:file "adapters")
                                     (:module "adapters/"
                                              :serial t
                                              :components ((:file "usocket-adapter")))))))
