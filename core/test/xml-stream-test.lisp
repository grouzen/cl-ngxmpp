;;;; xml-stream-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

(defun write-data (str stream)
  (write str :stream stream)
  (force-output stream))

(defun stanza-reader-read (stream &key (reader 'cl-ngxmpp:stanza-reader))
  (cl-ngxmpp:stanza-reader-read-stream
   (make-instance reader :stanza-stream stream)))


(deftestsuite xml-stream-test (cl-ngxmpp-test)
  ())


(deftestsuite xml-stream-actions-test (xml-stream-test)
  ((xml-stream      nil)
   (adapter         (make-instance 'cl-ngxmpp:usocket-adapter))
   (debuggable      nil)
   (server-hostname "ch3kr.net")
   (server-port     5222))
  (:setup (let ((c (make-instance 'cl-ngxmpp:connection
                                  :adapter  adapter
                                  :hostname server-hostname
                                  :port     server-port)))
            (when (cl-ngxmpp:connectedp (cl-ngxmpp:open-connection c))
              (setf xml-stream (make-instance 'cl-ngxmpp:xml-stream
                                              :connection c
                                              :debuggable debuggable))))))

(deftestsuite xml-stream-actions-open-stream-test (xml-stream-actions-test)
  ()
  (:teardown (when (cl-ngxmpp:openedp xml-stream)
               (cl-ngxmpp:close-stream xml-stream))))

(addtest (xml-stream-actions-open-stream-test)
  open-stream-with-opened-connection
  (ensure (cl-ngxmpp:openedp (cl-ngxmpp:open-stream xml-stream))))

#+sbcl
(addtest (xml-stream-actions-open-stream-test)
  open-stream-with-closed-connection
  (progn
    (cl-ngxmpp:close-connection (cl-ngxmpp::connection xml-stream))
    (ensure-condition sb-int:closed-stream-error (cl-ngxmpp:open-stream xml-stream))))


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
    (ensure-null (not (cl-ngxmpp::result (stanza-reader-read stream-in))))))

(addtest (xml-stream-stanza-reader-test)
  read-incorrect-xml
  (progn
    (write-data incorrect-xml stream-out)
    (ensure-condition type-error (stanza-reader-read stream-in))))

(addtest (xml-stream-stanza-reader-test)
  read-empty-xml
  (progn
    (write-data empty-xml stream-out)
    (ensure-condition type-error (stanza-reader-read stream-in))))


(deftestsuite xml-stream-stanza-reader-header-test (xml-stream-stanza-reader-test)
  ((correct-header "<?xml version='1.0'?>")))

(addtest (xml-stream-stanza-reader-header-test)
  read-correct-header-depth
  (progn
    (write-data correct-header stream-out)
    (let ((reader (stanza-reader-read stream-in
                                      :reader 'cl-ngxmpp:stanza-reader-header)))
      (ensure-same 1 (cl-ngxmpp::depth reader)))))

(addtest (xml-stream-stanza-reader-header-test)
  read-correct-header-state
  (progn
    (write-data correct-header stream-out)
    (ensure-same :node-opened
                 (cl-ngxmpp::state (stanza-reader-read
                                    stream-in
                                    :reader 'cl-ngxmpp:stanza-reader-header)))))


(deftestsuite xml-stream-stanza-reader-features-test (xml-stream-stanza-reader-test)
  ((correct-features "<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>
                        <stream:features><some_features/></stream:features>")
   (incorrect-features "<stream:stream><stream:features><some_features/>")))

(addtest (xml-stream-stanza-reader-features-test)
  read-correct-features-state
  (progn
    (write-data correct-features stream-out)
    (ensure-same :node-closed
                 (cl-ngxmpp::state (stanza-reader-read
                                    stream-in
                                    :reader 'cl-ngxmpp:stanza-reader-features)))))

(addtest (xml-stream-stanza-reader-features-test)
  read-incorrect-features
  (progn
    (write-data incorrect-features stream-out)
    (ensure-condition type-error
      (stanza-reader-read stream-in
                          :reader 'cl-ngxmpp:stanza-reader-features))))

