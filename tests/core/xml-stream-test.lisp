;;;; xml-stream-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-test)

(defun write-data (str stream)
  (write str :stream stream)
  (force-output stream))

(defun stanza-reader-read (stream &key (reader 'xmpp%:stanza-reader))
  (xmpp%:stanza-reader-read-stream
   (make-instance reader :stanza-stream stream)))


(deftestsuite xml-stream-test (cl-ngxmpp-test)
  ())


(deftestsuite xml-stream-actions-test (xml-stream-test)
  ((xml-stream nil)
   (connection (make-instance 'xmpp%:connection
                              :adapter  (make-instance 'xmpp%:usocket-adapter)
                              :hostname "ch3kr.net"
                              :port     5222))
   (debuggable nil))
  (:setup (progn
            (xmpp%:open-connection connection)
            (when (xmpp%:connectedp connection)     
              (setf xml-stream (make-instance 'xmpp%:xml-stream
                                              :connection connection
                                              :debuggable debuggable))))))


(deftestsuite xml-stream-actions-open-stream-test (xml-stream-actions-test)
  ()
  (:teardown (when (xmpp%:openedp xml-stream)
               (xmpp%:close-stream xml-stream)
               (when (xmpp%:connectedp connection)
                 (xmpp%:close-connection connection)))))

(addtest (xml-stream-actions-open-stream-test)
  open-stream-with-opened-connection
  (progn
    (xmpp%:open-stream xml-stream)
    (ensure (xmpp%:openedp xml-stream))))

#+sbcl
(addtest (xml-stream-actions-open-stream-test)
  open-stream-with-closed-connection
  (progn
    (xmpp%:close-connection (xmpp%::connection xml-stream))
    (ensure-condition sb-int:closed-stream-error (xmpp%:open-stream xml-stream))))


(deftestsuite xml-stream-stanza-reader-test (xml-stream-test)
  ((filespec #P"reader-test")
   (stream-out nil)
   (stream-in  nil)
   (correct-xml "<foo><bar id='a'>dog</bar></foo>")
   (incorrect-xml "<foo><bar/>")
   (empty-xml ""))
  (:setup (progn
            (setf stream-out (open filespec
                                   :direction :output
                                   :element-type 'character
                                   :if-does-not-exist :create))
            (setf stream-in (open filespec :element-type 'character))))
  (:teardown (progn
               (close stream-out)
               (close stream-in)
               (delete-file filespec))))

(addtest (xml-stream-stanza-reader-test)
  read-correct-xml
  (progn
    (write-data correct-xml stream-out)
    (ensure-null (not (xmpp%::result (stanza-reader-read stream-in))))))

(addtest (xml-stream-stanza-reader-test)
  read-incorrect-xml
  (progn
    (write-data incorrect-xml stream-out)
    (ensure-condition xmpp%:stanza-reader-error (stanza-reader-read stream-in))))

(addtest (xml-stream-stanza-reader-test)
  read-empty-xml
  (progn
    (write-data empty-xml stream-out)
    (ensure-condition xmpp%:stanza-reader-error (stanza-reader-read stream-in))))


(deftestsuite xml-stream-stanza-reader-header-test (xml-stream-stanza-reader-test)
  ((correct-header "<?xml version='1.0'?>")))

(addtest (xml-stream-stanza-reader-header-test)
  read-correct-header-depth
  (progn
    (write-data correct-header stream-out)
    (let ((reader (stanza-reader-read stream-in
                                      :reader 'xmpp%:stanza-reader-header)))
      (ensure-same 1 (xmpp%::depth reader)))))

(addtest (xml-stream-stanza-reader-header-test)
  read-correct-header-state
  (progn
    (write-data correct-header stream-out)
    (ensure-same :node-opened
                 (xmpp%::state (stanza-reader-read
                                    stream-in
                                    :reader 'xmpp%:stanza-reader-header)))))


(deftestsuite xml-stream-stanza-reader-features-test (xml-stream-stanza-reader-test)
  ((correct-features "<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>
                        <stream:features><some_features/></stream:features>")
   (incorrect-features "<stream:stream><stream:features><some_features/>")))

(addtest (xml-stream-stanza-reader-features-test)
  read-correct-features-state
  (progn
    (write-data correct-features stream-out)
    (ensure-same :node-closed
                 (xmpp%::state (stanza-reader-read
                                    stream-in
                                    :reader 'xmpp%:stanza-reader-features)))))

(addtest (xml-stream-stanza-reader-features-test)
  read-incorrect-features
  (progn
    (write-data incorrect-features stream-out)
    (ensure-condition xmpp%:stanza-reader-error
      (stanza-reader-read stream-in
                          :reader 'xmpp%:stanza-reader-features))))

