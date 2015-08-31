;;;; client-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-client-test)

(deftestsuite client-test (cl-ngxmpp-client-test)
  ((client          nil)
   (server-hostname "ch3kr.net")
   (server-port     5222)
   (adapter         (make-instance 'xmpp%:usocket-adapter))))


(deftestsuite client-connect/disconnect-test (client-test)
  ()
  (:setup    (setf client
                   (make-instance 'xmpp:client
                                  :server-hostname server-hostname
                                  :server-port     server-port
                                  :debuggable      nil
                                  :adapter         adapter)))
  (:teardown (xmpp:disconnect client)))

(addtest (client-connect/disconnect-test)
  correct-open
  (progn
    (xmpp:connect client)
    (ensure (xmpp%:openedp (xmpp::xml-stream client)))))

(addtest (client-connect/disconnect-test)
  incorrect-hostname-open
  (progn
    (setf (xmpp::server-hostname client) "incorrect-hostname")
    (ensure-condition xmpp%:connection-error
      (xmpp:connect client))))

(addtest (client-connect/disconnect-test)
  incorrect-port-open
  (progn
    (setf (xmpp::server-port client) 123)
    (ensure-condition xmpp%:connection-error
      (xmpp:connect client))))

(addtest (client-connect/disconnect-test)
  disconnect-from-connected
  (progn
    (xmpp:connect client)
    (ensure (xmpp:disconnect client))))


(deftestsuite client-authorize-test (client-test)
  ((username "clngxmpp")
   (password "clngxmpp"))
  (:setup (progn
            (setf client
                   (make-instance 'xmpp:client
                                  :server-hostname server-hostname
                                  :server-port     server-port
                                  :debuggable      nil
                                  :adapter         adapter))
            (xmpp:connect client))))

(addtest (client-authorize-test)
  correct-authorize
  (progn
    (xmpp:authorize client
                                :username username
                                :password password)
    (ensure
     (xmpp%:sasl-negotiatedp (xmpp::xml-stream client)))))
