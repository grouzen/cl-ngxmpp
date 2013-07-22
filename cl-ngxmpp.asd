
(in-package #:cl-user)

(defpackage #:cl-ngxmpp-system
  (:use #:cl #:asdf))

(in-package #:cl-ngxmpp-system)

(defsystem cl-ngxmpp
  :name "cl-ngxmpp"
  :author "Michael Nedokushev <grouzen.hexy@gmail.com>"
  :license "MIT"
  :depends-on (:usocket :cxml :babel :cl+ssl :cl-base64 :cl-sasl)
  :components ((:file "package")
               (:file "utils"            :depends-on ("package"))
               (:file "connection"       :depends-on ("utils"))
               (:file "xml-stream"       :depends-on ("connection"))
               (:file "stanzas"          :depends-on ("xml-stream"))
               (:file "tls-negotiation"  :depends-on ("stanzas"))
               (:file "sasl-negotiation" :depends-on ("stanzas"))))
