;;;; session-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-client-test)


(deftestsuite session-test (cl-ngxmpp-client-test)
  ((session nil)
   (server-hostname "ch3kr.net")
   (server-port     5222)
   (username        "clngxmpp")
   (password        "clngxmpp")
   (mechanism       nil))
  (:setup (setf session (xmpp::create-session
                           :server-hostname server-hostname
                           :server-port     server-port
                           :username        username
                           :password        password
                           :mechanism       mechanism
                           :debuggable      nil)))
  (:teardown (setf session nil)))


(deftestsuite session-open-test (session-test)
  ()
  (:teardown (xmpp:close-session session)))

(addtest (session-open-test)
  correct-open
  (progn
    (xmpp:open-session session)
    (ensure (xmpp%:openedp
             (xmpp::xml-stream session)))))

(addtest (session-open-test)
  incorrect-server-hostname-open
  (progn
    (setf (xmpp::server-hostname session) "incorrect.hostname")
    (ensure-condition xmpp%:connection-error
      (xmpp:open-session session))))

(addtest (session-open-test)
  incorrect-username-open
  (progn
    (setf (xmpp::username session) "incorrect-username-unknown")
    (ensure-condition xmpp%:negotiate-sasl-error
      (xmpp:open-session session))))
      
