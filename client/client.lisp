;;;; client.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client)

(defclass client ()
  ((username        :accessor username        :initarg :username        :initform "")
   (password        :accessor password        :initarg :password        :initform "")
   (resource        :accessor resource        :initarg :resource        :initform "cl-ngxmpp")
   (server-hostname :accessor server-hostname :initarg :server-hostname :initform cl-ngxmpp:*default-hostname*)
   (server-port     :accessor server-port     :initarg :server-port     :initform cl-ngxmpp:*default-port*)
   (xml-stream      :accessor xml-stream      :initarg :xml-stream      :initform nil)
   (session         :accessor session         :initarg :session         :initform nil)
   (adapter         :accessor adapter         :initarg :adapter         :initform (make-instance 'cl-ngxmpp:usocket-adapter))
   (debuggable      :accessor debuggable      :initarg :debuggable      :initform t)))

(defmethod jid ((client client))
  "Returns full jid, i.e. username@server.com/resource."
  (concatenate
   'string
   (username client)
   "@"
   (server-hostname client)
   "/"
   (resource client)))

(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A " (jid obj))
    (when (xml-stream obj)
      (print (cl-ngxmpp::connection (xml-stream obj)) stream))))

(defmethod disconnect ((client client))
  (let* ((xml-stream (xml-stream client))
         (connection (when xml-stream
                       (cl-ngxmpp::connection xml-stream))))
    (when (and (not (null xml-stream))
               (cl-ngxmpp:openedp xml-stream))
      (cl-ngxmpp:close-stream xml-stream))
    (when (and (not (null connection))
               (cl-ngxmpp:connectedp connection))
      (cl-ngxmpp:close-connection connection))))

(defmethod connect ((client client))
  (let ((connection (make-instance 'cl-ngxmpp:connection
                                   :adapter  (adapter         client)
                                   :hostname (server-hostname client)
                                   :port     (server-port     client))))
    (cl-ngxmpp:open-connection connection)
    (when (cl-ngxmpp:connectedp connection)
      (let ((xml-stream (make-instance 'cl-ngxmpp:xml-stream
                                       :connection connection
                                       :debuggable (debuggable client))))
        (setf (xml-stream client) xml-stream)
        (cl-ngxmpp:open-stream xml-stream)
        (cl-ngxmpp:negotiate-tls xml-stream)))))

(defmethod authorize ((client client) &key username password mechanism)
  "SASL authorization over TLS connection, should be called after the connection
is established. In case of error signals negotiate-sasl-condition which should be
handled by the caller."
  (let ((xml-stream (xml-stream client)))
    (setf (username client) username
          (password client) password)
    ;; This hell is needed for suppression of errors.
    ;; Default cl-ngxmpp:handle-stanza signals a handle-stanza-error,
    ;; thus if client didn't define handle-stanza method for appropriate
    ;; type of stanza, authorization will fail.
    ;; There is another case about sasl negotiation, when authorization failed
    ;; and server sends <failure/> stanza, but client didn't manage to define
    ;; handle-stanza for failure stanza.
    (handler-bind
        ((cl-ngxmpp:handle-stanza-error
          #'(lambda (c)
              (declare (ignore c))
              (invoke-restart 'skip-handle-stanza))))
      (macrolet ((with-restarts ((&rest restarts) &body steps)
                   (let ((steps-restarts
                          (mapcar
                           #'(lambda (step)
                               `(restart-case ,step ,@restarts))
                           steps)))
                     `(progn ,@steps-restarts))))
        (with-restarts ((skip-handle-stanza () nil))
          (cl-ngxmpp:negotiate-sasl xml-stream
                                    :username username
                                    :password password
                                    :mechanism mechanism)
          (%bind% client)
          (%session% client)
          (send-presence-show client :show "online")
          (proceed-stanza client))))))

(defmethod %bind% ((client client))
  (let ((xml-stream (xml-stream client))
        (resource   (resource client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:iq-set-bind-stanza
                     :id       "bind"
                     :resource (resource client)))
    (proceed-stanza client)))

(defmethod %session% ((client client))
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:iq-set-session-stanza
                     :to (server-hostname client)
                     :id "sess"))
    (proceed-stanza client)))

;;
;; Blocking I/O methods, usefull for bots.
;; These methods call `cl-ngxmpp:handle-stanza' callback
;; after receiving a message from network.
;;
(defmethod proceed-stanza-loop ((client client))
  (let ((xml-stream (xml-stream client)))
    (handler-case
        (loop
           :until (cl-ngxmpp:closedp xml-stream)
           :do (proceed-stanza client))
      (cl-ngxmpp:handle-stanza-error (c) (format nil "~S" c)))))

(defmethod proceed-stanza ((client client))
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-input (xml-stream stanza)
      (cl-ngxmpp:handle-stanza stanza))))

;;
;; Method just receives an any stanza from network
;; and returns it. User can do anything with result
;; of these method (i.e call handle-stanza).
;; It's usefull for event-loop.
;;
(defmethod read-stanza ((client client))
  (with-slots (xml-stream) client
    (cl-ngxmpp:with-stanza-input (xml-stream stanza)
      stanza)))

;;
;; Methods for sending stanzas from core RFC.
;; For the rest of send-* methods, see client/xeps/xep-XXXX.lisp files.
;;
(defmethod send-message ((client client) &key to body)
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:message-stanza
                     :from (jid client)
                     :to   to
                     :body body))))

(defmethod send-presence-subscribe ((client client) &key to from status)
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:presence-subscribe-stanza
                     :to to :from from :status status))))

(defmethod send-presence-show ((client client) &key to from show)
  (let ((xml-stream (xml-stream client)))
    (cl-ngxmpp:with-stanza-output (xml-stream)
      (make-instance 'cl-ngxmpp:presence-show-stanza
                     :to to :from from :show show))))

