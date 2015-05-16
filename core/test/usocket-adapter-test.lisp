;;;; usocket-adapter-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

(deftestsuite usocket-adapter-test (cl-ngxmpp-test)
  ((adapter (make-instance 'xmpp%:usocket-adapter))
   (hostname "ch3kr.net")
   (port     5222))
  (:teardown (when (xmpp%::adapter-connectedp adapter)
               (xmpp%::adapter-close-connection adapter))))


(deftestsuite usocket-adapter-connectedp-test (usocket-adapter-test)
  ())

(addtest (usocket-adapter-connectedp-test)
  connected-adapter
  (progn
    (xmpp%::adapter-open-connection adapter hostname port)
    (ensure (xmpp%::adapter-connectedp adapter))))

(addtest (usocket-adapter-connectedp-test)
  closed-adapter
  (ensure-null (xmpp%::adapter-connectedp adapter)))
  

(deftestsuite usocket-adapter-open-test (usocket-adapter-test)
  ())

(addtest (usocket-adapter-open-test)
  correct-open-test
  (progn
    (xmpp%::adapter-open-connection adapter hostname port)
    (ensure (xmpp%::adapter-connectedp adapter))))

(addtest (usocket-adapter-open-test)
  incorrect-hostname-open-test
  (ensure-condition xmpp%:connection-error
    (xmpp%::adapter-open-connection adapter "unknown.unknown" port)))

(addtest (usocket-adapter-open-test)
  incorrect-port-open-test
  (ensure-condition xmpp%:connection-error
    (xmpp%::adapter-open-connection adapter hostname 123)))


(deftestsuite usocket-adapter-close-test (usocket-adapter-test)
  ()
  (:setup (xmpp%::adapter-open-connection adapter hostname port)))

(addtest (usocket-adapter-close-test)
  close-correct-opened-adapter
  (progn
    (xmpp%::adapter-close-connection adapter)
    (ensure-null (xmpp%::adapter-connectedp adapter))))

(addtest (usocket-adapter-close-test)
  close-closed-adapter
  (progn
    (xmpp%::adapter-close-connection adapter)
    (xmpp%::adapter-close-connection adapter)
    (ensure-null (xmpp%::adapter-connectedp adapter))))


(deftestsuite usocket-adapter-read/write-stream-test (usocket-adapter-test)
  ((stream-header "<?xml version='1.0'?>")
   (stream-open   "<stream:stream id='0'
                     to='ch3kr.net'
                     xmlns:stream='http://etherx.jabber.org/streams'
                     version='1.0'
                     xmlns='jabber:client'>")
   (stream-close  "</stream:stream>"))
  (:setup (xmpp%::adapter-open-connection adapter hostname port)))

(addtest (usocket-adapter-read/write-stream-test)
  write-to-opened-connection
  (ensure (xmpp%::adapter-write-to-stream adapter stream-header)))

#+sbcl
(addtest (usocket-adapter-read/write-stream-test)
  write-to-closed-connection
  (progn
    (xmpp%::adapter-close-connection adapter)
    (ensure-condition sb-int:closed-stream-error
                       (xmpp%::adapter-write-to-stream adapter "<?xml?>"))))
  
(addtest (usocket-adapter-read/write-stream-test)
  read-from-opened-stream
  (progn
    (xmpp%::adapter-write-to-stream adapter "<?xml?>")
    (ensure-same (xmpp%:resolve-async-value
                  (xmpp%::adapter-read-from-stream adapter
                                                       :stanza-reader 'xmpp%:stanza-reader-header))
                 stream-header)))

(addtest (usocket-adapter-read/write-stream-test)
  read-from-closed-stream
  (progn
    (xmpp%::adapter-write-to-stream adapter "<?xml?>")
    (xmpp%::adapter-read-from-stream adapter :stanza-reader 'xmpp%:stanza-reader-header)
    (xmpp%::adapter-write-to-stream adapter stream-open)
    (xmpp%::adapter-read-from-stream adapter :stanza-reader 'xmpp%:stanza-reader-features)
    (xmpp%::adapter-write-to-stream adapter stream-close)
    (ensure-condition xmpp%:stanza-reader-error
      (xmpp%:resolve-async-value
       (xmpp%::adapter-read-from-stream
        adapter
        :stanza-reader 'xmpp%:stanza-reader))
      "</stream:stream>")))
