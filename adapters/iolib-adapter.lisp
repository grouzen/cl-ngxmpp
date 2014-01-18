;;;; usocket-adapter.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>


;; 
;; Actually this adapter doesn't work because of problem
;; with underlying fd which is returned from cl+ssl:make-ssl-client-stream
;; function. Discussion about this issue you can find on this url:
;; http://www.reddit.com/r/lisp/comments/1q3kvs/clssl_and_iolib_problem/
;;
;; But anyway I think problem can be solved somehow ;).
;;

(in-package #:cl-ngxmpp)

(defparameter *timeout* 5)

(defclass iolib-adapter (adapter)
  ((event-base :accessor event-base :initarg :event-base :initform nil)))

(defmethod adapter-open-connection ((adapter iolib-adapter) hostname port)
  (let ((socket-stream (iolib.sockets:make-socket :connect :active
                                                  :address-family :internet
                                                  :ipv6 nil
                                                  :type :stream
                                                  :external-format '(:utf-8 :eol-style :crlf))))
    (iolib.sockets:connect socket-stream
                           (iolib.sockets:lookup-hostname hostname)
                           :port port)
    (setf (socket-stream adapter) socket-stream
          (event-base    adapter) (make-instance 'iolib.multiplex:event-base
                                                 :exit-when-empty t))))

(defmethod adapter-close-connection ((adapter iolib-adapter))
  (with-slots (event-base socket-stream) adapter
    (close event-base)
    (close socket-stream :abort t)))

(defmethod async-read/write% ((adapter iolib-adapter) event-type function)
  (let* ((future        (cl-async-future:make-future))
         (event-base    (event-base    adapter))
         (socket-stream (socket-stream adapter))
         (cb            (funcall function future event-base socket-stream)))
    (iolib.multiplex:set-io-handler event-base
                                    (iolib.sockets:socket-os-fd socket-stream)
                                    event-type cb
                                    :timeout *timeout*)
    (iolib.multiplex:event-dispatch event-base :one-shot t)
    future))

(defmethod adapter-read-from-stream ((adapter iolib-adapter) &key stanza-reader)
  (async-read/write% adapter :read
                     #'(lambda (future event-base socket-stream)
                         (lambda (fd event exception)
                           (iolib.multiplex:remove-fd-handlers event-base fd :write t :read t :error t)
                           (cl-async-future:finish
                            future
                            (result (stanza-reader-read-stream (make-instance stanza-reader :stanza-stream socket-stream))))))))

(defmethod adapter-write-to-stream ((adapter iolib-adapter) string)
  (async-read/write% adapter :write
                     #'(lambda (future event-base socket-stream)
                         (lambda (fd event exception)
                           (iolib.multiplex:remove-fd-handlers event-base fd :write t :read t :error t)
                           (write-string string socket-stream)
                           (force-output socket-stream)
                           (cl-async-future:finish future)))))

(defmethod adapter-connectedp ((adapter iolib-adapter))
  (with-slots (socket-stream) adapter
    (and (streamp socket-stream)
         (open-stream-p socket-stream))))

