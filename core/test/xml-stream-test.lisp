;;;; stanzas-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

(deftestsuite xml-stream-test ()
  ())

(deftestsuite xml-stream-stanza-reader-test (xml-stream-test)
  ((correct-xml "<foo><bar id='a'>dog</bar></foo>")
   (incorrect-xml "<foo><bar/>")
   (filespec #P"reader-test")
   (stream-out nil)
   (stream-in  nil))
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
    (write correct-xml :stream stream-out)
    (force-output stream-out)
    (ensure-null (not (cl-ngxmpp::result (cl-ngxmpp:stanza-reader-read-stream
                                          (make-instance 'cl-ngxmpp:stanza-reader
                                                         :stanza-stream stream-in)))))))

(addtest (xml-stream-stanza-reader-test)
  read-incorrect-xml
  (progn
    (write incorrect-xml :stream stream-out)
    (force-output stream-out)
    (ensure-condition type-error (cl-ngxmpp:stanza-reader-read-stream
                                  (make-instance 'cl-ngxmpp:stanza-reader
                                                 :stanza-stream stream-in)))))

(describe (run-tests))
