
(in-package #:cl-user)

(defpackage #:cl-ngxmpp-ext-system
  (:use #:cl #:asdf))

(in-package #:cl-ngxmpp-ext-system)

(defsystem cl-ngxmpp-ext
  :name "cl-ngxmpp-ext"
  :author "Michael Nedokushev"
  :license "MIT"
  :depends-on (:cl-ngxmpp)
  :components ((:file "xmpp")))

