;;;; cl-ngxmpp-client.asd
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-user)

(defpackage #:cl-ngxmpp-client-system
  (:use #:cl #:asdf))

(in-package #:cl-ngxmpp-client-system)

(defsystem cl-ngxmpp-client
  :name "cl-ngxmpp-client"
  :author "Michael Nedokushev <grouzen.hexy@gmail.com>"
  :license "MIT"
  :depends-on (:cl-ngxmpp)
  :components ((:file "client")))

