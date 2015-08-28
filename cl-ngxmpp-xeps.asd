
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
                                     (:file "xep-0045")
                                     (:file "xep-0203")))))

