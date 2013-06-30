
(in-package #:cl-user)

(defpackage #:cl-ngxmpp-system
  (:use #:cl #:asdf))

(in-package #:cl-ngxmpp-system)

(defsystem cl-ngxmpp
  :name "cl-ngxmpp"
  :author "Michael Nedokushev"
  :license "MIT"
  :depends-on (:usocket :cxml :babel)
  :components ((:file "package")
               (:file "utils"      :depends-on ("package"))
               (:file "connection" :depends-on ("utils"))
               (:file "xml-stream" :depends-on ("connection"))))
