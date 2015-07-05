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
   (server-hostname :accessor server-hostname :initarg :server-hostname :initform xmpp%:*default-hostname*)
   (server-port     :accessor server-port     :initarg :server-port     :initform xmpp%:*default-port*)
   (xml-stream      :accessor xml-stream      :initarg :xml-stream      :initform nil)
   (session         :accessor session         :initarg :session         :initform nil)
   (adapter         :accessor adapter         :initarg :adapter         :initform (make-instance 'xmpp%:usocket-adapter))
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
      (print (xmpp%::connection (xml-stream obj)) stream))))

(defmethod disconnect ((client client))
  (let* ((xml-stream (xml-stream client))
         (connection (when xml-stream
                       (xmpp%::connection xml-stream))))
    (when (and (not (null xml-stream))
               (xmpp%:openedp xml-stream))
      (xmpp%:close-stream xml-stream))
    (when (and (not (null connection))
               (xmpp%:connectedp connection))
      (xmpp%:close-connection connection))))

(defmethod connect ((client client))
  (let ((connection (make-instance 'xmpp%:connection
                                   :adapter  (adapter         client)
                                   :hostname (server-hostname client)
                                   :port     (server-port     client))))
    (xmpp%:open-connection connection)
    (when (xmpp%:connectedp connection)
      (let ((xml-stream (make-instance 'xmpp%:xml-stream
                                       :connection connection
                                       :debuggable (debuggable client))))
        (setf (xml-stream client) xml-stream)
        (xmpp%:open-stream xml-stream)
        (xmpp%:negotiate-tls xml-stream)))))

(defmethod authorize ((client client) &key username password mechanism)
  "SASL authorization over TLS connection, should be called after the connection
is established. In case of error signals negotiate-sasl-condition which should be
handled by the caller."
  (let ((xml-stream (xml-stream client)))
    (when (xmpp%:tls-negotiatedp xml-stream)
      (setf (username client) username
            (password client) password)
      ;; This hell is needed for suppression of errors.
      ;; Default xmpp%:handle-stanza signals a handle-stanza-error,
      ;; thus if client didn't define handle-stanza method for appropriate
      ;; type of stanza, authorization will fail.
      ;; There is another case about sasl negotiation, when authorization failed
      ;; and server sends <failure/> stanza, but client didn't manage to define
      ;; handle-stanza for failure stanza.
      (handler-bind
          ((xmpp%:handle-stanza-error
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
            (xmpp%:negotiate-sasl xml-stream
                                      :username username
                                      :password password
                                      :mechanism mechanism)
            (%bind% client)
            (%session% client)
            ;; TODO:
            ;; move this into session.lisp
            (send-presence-show client :show "online")
            (proceed-stanza client)))))))


;; TODO:
;; replace proceed-stanza with something else,
;; because it seems like HANDLE-STANZA interface is deprecated now.
;;
(defmethod %bind% ((client client))
  (let ((xml-stream (xml-stream client))
        (resource   (resource client)))
    (xmpp%:with-stanza-output (xml-stream)
      (make-instance 'xmpp%:iq-set-bind-stanza
                     :id       "bind"
                     :resource (resource client)))
    (proceed-stanza client)))

(defmethod %session% ((client client))
  (let ((xml-stream (xml-stream client)))
    (xmpp%:with-stanza-output (xml-stream)
      (make-instance 'xmpp%:iq-set-session-stanza
                     :to (server-hostname client)
                     :id "sess"))
    (proceed-stanza client)))

;;
;; These methods are DEPRECATED, use cl-ngxmpp-client's high interface instead.
;;
;; Blocking I/O methods, usefull for bots.
;; These methods call `xmpp%:handle-stanza' callback
;; after receiving a message from network.
;;
(defmethod proceed-stanza-loop ((client client))
  (let ((xml-stream (xml-stream client)))
    (handler-case
        (loop
           :until (xmpp%:closedp xml-stream)
           :do (proceed-stanza client))
      (xmpp%:handle-stanza-error (c) (format nil "~S" c)))))

(defmethod proceed-stanza ((client client))
  (xmpp%:handle-stanza (read-stanza client)))

;;
;; Method just receives any stanza from the network
;; and returns it. User can do anything with result
;; of this method (i.e call handle-stanza).
;; It's usefull for event-loop.
;;
(defmethod read-stanza ((client client))
  (with-slots (xml-stream) client
    (xmpp%:with-stanza-input (xml-stream stanza)
      stanza)))

;;
;; Methods for sending stanzas from core RFC.
;; For the rest of send-* methods, see client/xeps/xep-XXXX.lisp files.
;;
(defmethod send-message ((client client) &key to body)
  (let ((xml-stream (xml-stream client)))
    (xmpp%:with-stanza-output (xml-stream)
      (make-instance 'xmpp%:message-stanza
                     :from (jid client)
                     :to   to
                     :body body))))

(defmethod send-presence-subscribe ((client client) &key to from status)
  (let ((xml-stream (xml-stream client)))
    (xmpp%:with-stanza-output (xml-stream)
      (make-instance 'xmpp%:presence-subscribe-stanza
                     :to to :from from :status status))))

(defmethod send-presence-show ((client client) &key to from show)
  (let ((xml-stream (xml-stream client)))
    (xmpp%:with-stanza-output (xml-stream)
      (make-instance 'xmpp%:presence-show-stanza
                     :to to :from from :show show))))
