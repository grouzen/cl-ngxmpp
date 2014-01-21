;;;; usocket-adapter-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

(deftestsuite usocket-adapter-test (cl-ngxmpp-test)
  ((adapter (make-instance 'cl-ngxmpp:usocket-adapter))
   (hostname "ch3kr.net")
   (port     5222))
  (:teardown (when (cl-ngxmpp::adapter-connectedp adapter)
               (cl-ngxmpp::adapter-close-connection adapter))))


(deftestsuite usocket-adapter-connectedp-test (usocket-adapter-test)
  ())

(addtest (usocket-adapter-connectedp-test)
  connected-adapter
  (progn
    (cl-ngxmpp::adapter-open-connection adapter hostname port)
    (ensure (cl-ngxmpp::adapter-connectedp adapter))))

(addtest (usocket-adapter-connectedp-test)
  closed-adapter
  (ensure-null (cl-ngxmpp::adapter-connectedp adapter)))
  

(deftestsuite usocket-adapter-open-test (usocket-adapter-test)
  ())

(addtest (usocket-adapter-open-test)
  correct-open-test
  (ensure (cl-ngxmpp::adapter-connectedp
           (cl-ngxmpp::adapter-open-connection adapter hostname port))))

(addtest (usocket-adapter-open-test)
  incorrect-hostname-open-test
  (ensure-null (cl-ngxmpp::adapter-connectedp
                (cl-ngxmpp::adapter-open-connection adapter "unknown.unknown" port))))

(addtest (usocket-adapter-open-test)
  incorrect-port-open-test
  (ensure-null (cl-ngxmpp::adapter-connectedp
                (cl-ngxmpp::adapter-open-connection adapter hostname 123))))


(deftestsuite usocket-adapter-close-test (usocket-adapter-test)
  ()
  (:setup (cl-ngxmpp::adapter-open-connection adapter hostname port)))

(addtest (usocket-adapter-close-test)
  close-correct-opened-adapter
  (ensure-null (cl-ngxmpp::adapter-connectedp
                (cl-ngxmpp::adapter-close-connection adapter))))

(addtest (usocket-adapter-close-test)
  close-closed-adapter
  (progn
    (cl-ngxmpp::adapter-close-connection adapter)
    (ensure-null (cl-ngxmpp::adapter-connectedp
                  (cl-ngxmpp::adapter-close-connection adapter)))))
