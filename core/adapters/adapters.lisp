;;;; adapter.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

(defclass adapter ()
  ((socket-stream :accessor socket-stream :initarg :socket-stream :initform nil)))

(defgeneric adapter-close-connection (adapter)
  (:documentation "Close connection"))

(defgeneric adapter-open-connection (adapter hostname port)
  (:documentation "Open connection"))

(defgeneric adapter-read-from-stream (adapter &key stanza-reader)
  (:documentation "Read from XML stream"))

(defgeneric adapter-write-to-stream (adapter string)
  (:documentation "Write to XML stream"))

(defgeneric adapter-connectedp (adapter)
  (:documentation "Connected predicate")
  (:method ((adapter adapter))
    (with-slots (socket-stream) adapter
      (and (streamp socket-stream)
           (open-stream-p socket-stream)))))
