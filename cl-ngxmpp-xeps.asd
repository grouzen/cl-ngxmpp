;;;; cl-ngxmpp-xeps.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-user)

(asdf:defsystem #:cl-ngxmpp-xeps
  :name "cl-ngxmpp-xeps"
  :author "Michael Nedokushev <michael.nedokushev@gmail.com>"
  :license "Lisp-LGPL"
  :depends-on (:cl-ngxmpp)
  :serial t
  :components ((:module "src/xeps"
                        :serial t
                        :components ((:file "package")
                                     (:file "xeps")
                                     (:file "xep-0004")
                                     (:file "xep-0045")
                                     (:file "xep-0203")))))

