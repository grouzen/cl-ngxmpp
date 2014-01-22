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
  (progn
    (cl-ngxmpp::adapter-open-connection adapter hostname port)
    (ensure (cl-ngxmpp::adapter-connectedp adapter))))

(addtest (usocket-adapter-open-test)
  incorrect-hostname-open-test
  (ensure-condition cl-ngxmpp:connection-error
    (cl-ngxmpp::adapter-open-connection adapter "unknown.unknown" port)))

(addtest (usocket-adapter-open-test)
  incorrect-port-open-test
  (ensure-condition cl-ngxmpp:connection-error
    (cl-ngxmpp::adapter-open-connection adapter hostname 123)))


(deftestsuite usocket-adapter-close-test (usocket-adapter-test)
  ()
  (:setup (cl-ngxmpp::adapter-open-connection adapter hostname port)))

(addtest (usocket-adapter-close-test)
  close-correct-opened-adapter
  (progn
    (cl-ngxmpp::adapter-close-connection adapter)
    (ensure-null (cl-ngxmpp::adapter-connectedp adapter))))

(addtest (usocket-adapter-close-test)
  close-closed-adapter
  (progn
    (cl-ngxmpp::adapter-close-connection adapter)
    (cl-ngxmpp::adapter-close-connection adapter)
    (ensure-null (cl-ngxmpp::adapter-connectedp adapter))))


(deftestsuite usocket-adapter-read/write-stream-test (usocket-adapter-test)
  ((stream-header "<?xml version='1.0'?>")
   (stream-open   "<stream:stream id='0'
                     to='ch3kr.net'
                     xmlns:stream='http://etherx.jabber.org/streams'
                     version='1.0'
                     xmlns='jabber:client'>")
   (stream-close  "</stream:stream>"))
  (:setup (cl-ngxmpp::adapter-open-connection adapter hostname port)))

(addtest (usocket-adapter-read/write-stream-test)
  write-to-opened-connection
  (ensure (cl-ngxmpp::adapter-write-to-stream adapter stream-header)))

#+sbcl
(addtest (usocket-adapter-read/write-stream-test)
  write-to-closed-connection
  (progn
    (cl-ngxmpp::adapter-close-connection adapter)
    (ensure-condition sb-int:closed-stream-error
                       (cl-ngxmpp::adapter-write-to-stream adapter "<?xml?>"))))
  
(addtest (usocket-adapter-read/write-stream-test)
  read-from-opened-stream
  (progn
    (cl-ngxmpp::adapter-write-to-stream adapter "<?xml?>")
    (ensure-same (cl-ngxmpp::future-value
                  (cl-ngxmpp::adapter-read-from-stream
                   adapter
                   :stanza-reader 'cl-ngxmpp:stanza-reader-header))
                 stream-header)))

(addtest (usocket-adapter-read/write-stream-test)
  read-from-closed-stream
  (progn
    (cl-ngxmpp::adapter-write-to-stream adapter "<?xml?>")
    (cl-ngxmpp::adapter-read-from-stream adapter :stanza-reader 'cl-ngxmpp:stanza-reader-header)
    (cl-ngxmpp::adapter-write-to-stream adapter stream-open)
    (cl-ngxmpp::adapter-read-from-stream adapter :stanza-reader 'cl-ngxmpp:stanza-reader-features)
    (cl-ngxmpp::adapter-write-to-stream adapter stream-close)
    (ensure-condition cl-ngxmpp:stanza-reader-error
      (cl-ngxmpp::future-value
       (cl-ngxmpp::adapter-read-from-stream
        adapter
        :stanza-reader 'cl-ngxmpp:stanza-reader))
      "</stream:stream>")))
