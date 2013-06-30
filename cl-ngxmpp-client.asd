
(in-package #:cl-user)

(defpackage #:cl-ngxmpp-client-system
  (:use #:cl #:asdf))

(in-package #:cl-ngxmpp-client-system)

(defsystem cl-ngxmpp-client
  :name "cl-ngxmpp-client"
  :author "Michael Nedokushev"
  :license "MIT"
  :depends-on (:cl-ngxmpp)
  :components ((:file "xmpp")))

