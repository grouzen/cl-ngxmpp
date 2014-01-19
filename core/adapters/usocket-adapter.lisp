;;;; usocket-adapter.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

(defclass usocket-adapter (adapter)
  ((socket :accessor socket :initarg :socket :initform nil)))

(defmethod adapter-close-connection ((adapter usocket-adapter))
  (close (socket-stream adapter)))

(defmethod adapter-open-connection ((adapter usocket-adapter) hostname port)
  (let* ((socket (usocket:socket-connect hostname port :element-type 'character))
         (stream (usocket:socket-stream socket)))
    (setf (socket        adapter) socket
          (socket-stream adapter) stream)))

(defmethod adapter-read-from-stream ((adapter usocket-adapter) &key stanza-reader)
  (let ((future (cl-async-future:make-future)))
    (cl-async-future:finish future
                            (result (stanza-reader-read-stream
                                     (make-instance stanza-reader
                                                    :stanza-stream (socket-stream adapter)))))))

(defmethod adapter-write-to-stream ((adapter usocket-adapter) string)
  (let ((future (cl-async-future:make-future)))
    (with-slots (socket-stream) adapter
      (write-string string socket-stream)
      (force-output socket-stream))
    (cl-async-future:finish future)))

(defmethod adapter-connectedp ((adapter usocket-adapter))
  (with-slots (socket-stream) adapter
    (and (streamp socket-stream)
         (open-stream-p socket-stream))))
