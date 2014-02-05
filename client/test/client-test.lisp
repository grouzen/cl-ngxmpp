;;;; client-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client-test)

(deftestsuite client-test (cl-ngxmpp-client-test)
  ((client          nil)
   (server-hostname "ch3kr.net")
   (server-port     5222)
   (adapter         (make-instance 'cl-ngxmpp:usocket-adapter))))


(deftestsuite client-connect/disconnect-test (client-test)
  ()
  (:setup    (setf client
                   (make-instance 'cl-ngxmpp-client:client
                                  :server-hostname server-hostname
                                  :server-port     server-port
                                  :debuggable      nil
                                  :adapter         adapter)))
  (:teardown (cl-ngxmpp-client:disconnect client)))

(addtest (client-connect/disconnect-test)
  correct-open
  (progn
    (cl-ngxmpp-client:connect client)
    (ensure (cl-ngxmpp:openedp (cl-ngxmpp-client::xml-stream client)))))

(addtest (client-connect/disconnect-test)
  incorrect-hostname-open
  (progn
    (setf (cl-ngxmpp-client::server-hostname client) "incorrect-hostname")
    (ensure-condition cl-ngxmpp:connection-error
      (cl-ngxmpp-client:connect client))))

(addtest (client-connect/disconnect-test)
  incorrect-port-open
  (progn
    (setf (cl-ngxmpp-client::server-port client) 123)
    (ensure-condition cl-ngxmpp:connection-error
      (cl-ngxmpp-client:connect client))))

(addtest (client-connect/disconnect-test)
  disconnect-from-connected
  (progn
    (cl-ngxmpp-client:connect client)
    (ensure (cl-ngxmpp-client:disconnect client))))


(deftestsuite client-authorize-test (client-test)
  ((username "clngxmpp")
   (password "clngxmpp"))
  (:setup (progn
            (setf client
                   (make-instance 'cl-ngxmpp-client:client
                                  :server-hostname server-hostname
                                  :server-port     server-port
                                  :debuggable      nil
                                  :adapter         adapter))
            (cl-ngxmpp-client:connect client))))

(addtest (client-authorize-test)
  correct-authorize
  (progn
    (cl-ngxmpp-client:authorize client
                                :username username
                                :password password)
    (ensure
     (cl-ngxmpp:sasl-negotiatedp (cl-ngxmpp-client::xml-stream client)))))
