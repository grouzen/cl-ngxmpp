;;;; usocket-adapter.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp)

(defclass usocket-adapter (adapter)
  ((socket :accessor socket :initarg :socket :initform nil)))

(defmethod adapter-close-connection ((adapter usocket-adapter))
  (close (socket-stream adapter)))

(defmethod adapter-open-connection ((adapter usocket-adapter) hostname port)
  (with-proxy-error connection-error
      (usocket:ns-host-not-found-error
       usocket:timeout-error
       usocket:connection-refused-error)
    (let* ((socket (usocket:socket-connect hostname port :element-type 'character))
           (stream (usocket:socket-stream socket)))
      (setf (socket        adapter) socket
            (socket-stream adapter) stream))))

(defmethod adapter-read-from-stream ((adapter usocket-adapter) &key stanza-reader)
  (bb:promisify (result (stanza-reader-read-stream
                         (make-instance stanza-reader
                                        :stanza-stream (socket-stream adapter))))))

(defmethod adapter-write-to-stream ((adapter usocket-adapter) string)
  (with-slots (socket-stream) adapter
    (write-string string socket-stream)
    (force-output socket-stream))
  (length string))
