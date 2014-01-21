;;;; session-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client-test)


(deftestsuite session-test (cl-ngxmpp-client-test)
  ((session nil)
   (server-hostname "ch3kr.net")
   (server-port     5222)
   (username        "clngxmpp")
   (password        "clngxmpp")
   (mechanism       nil))
  (:setup (setf session (cl-ngxmpp-client::create-session
                           :server-hostname server-hostname
                           :server-port     server-port
                           :username        username
                           :password        password
                           :mechanism       mechanism
                           :debuggable      t)))
  (:teardown (setf session nil)))


(deftestsuite session-open-test (session-test)
  ()
  (:teardown (cl-ngxmpp-client:close-session session)))

(addtest (session-open-test)
  correct-open
  (ensure (cl-ngxmpp:openedp
           (cl-ngxmpp-client::xml-stream (cl-ngxmpp-client:open-session session)))))

(addtest (session-open-test)
  incorrect-server-hostname-open
  (progn
    (setf (cl-ngxmpp-client::server-hostname session) "incorrect.hostname")
    (ensure (cl-ngxmpp:closedp
             (cl-ngxmpp-client::xml-stream (cl-ngxmpp-client:open-session session))))))

(addtest (session-open-test)
  incorrect-username-open
  (progn
    (setf (cl-ngxmpp-client::username session) "incorrect-username-unknown")
    (ensure-condition 'cl-ngxmpp:negotiate-sasl-condition
       (cl-ngxmpp-client::xml-stream (cl-ngxmpp-client:open-session session)))))
