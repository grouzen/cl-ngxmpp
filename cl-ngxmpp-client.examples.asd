;;;; cl-ngxmpp-client.examples.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-user)

(defpackage #:cl-ngxmpp-client.examples-system
  (:use #:cl #:asdf))

(in-package #:cl-ngxmpp-client.examples-system)

(defsystem cl-ngxmpp-client.examples
  :name "cl-ngxmpp-client.examples"
  :author "Michael Nedokushev <grouzen.hexy@gmail.com>"
  :license "MIT"
  :depends-on (:cl-ngxmpp :cl-ngxmpp-client)
  :components ((:module "examples"
                        :components ((:file "echo-bot"))
                        :pathname "examples")))
  
    
