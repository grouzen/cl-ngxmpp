;;;; client.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The root class which represents a single client (in server-client terminology).
;; So, for example: if you're writing a xmpp client that supports multiple accounts, you might
;; want to make multiple `client` instances for each xmpp account.
;;

(defclass client (xmpp%:debuggable xmpp%:statefull)
  ((username        :accessor username        :initarg :username        :initform "")
   (password        :accessor password        :initarg :password        :initform "")
   (resource        :accessor resource        :initarg :resource        :initform "cl-ngxmpp")
   (server-hostname :accessor server-hostname :initarg :server-hostname :initform xmpp%:*default-hostname*)
   (server-port     :accessor server-port     :initarg :server-port     :initform xmpp%:*default-port*)
   (xml-stream      :accessor xml-stream      :initarg :xml-stream      :initform nil)))

(defmethod initialize-instance :after ((client client) &key)
  (setf (xmpp%::state client) 'disconnected))
           
(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A " (jid obj))
    (when (xml-stream obj)
      (print (xmpp%::connection (xml-stream obj)) stream))))

(defmethod jid ((client client))
  "Returns full jid, i.e. username@server.com/resource."
  (concatenate 'string
               (username client)
               "@"
               (server-hostname client)
               "/"
               (resource client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; State predicates
;;

(defmethod connectedp ((client client))
  (with-accessors ((state xmpp%::state)) client
    (or (eq state 'connectedp)
        (eq state 'loggedin))))

(defmethod loggedinp ((client client))
  (eq (xmpp%::state client) 'loggedin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic client's protocol: connect, disconnect, authorize
;;

(defmethod connect-client ((client client) &key server-hostname (server-port 5222) (adapter 'xmpp%:usocket-adapter))
  (let* ((adapter    (make-instance adapter))
         (connection (make-instance 'xmpp%:connection
                                    :adapter  adapter
                                    :hostname server-hostname
                                    :port     server-port)))
    (setf (server-hostname client) server-hostname
          (server-port     client) server-port)
    (xmpp%:open-connection connection)
    (when (xmpp%:connectedp connection)
      (let ((xml-stream (make-instance 'xmpp%:xml-stream
                                       :connection connection
                                       :debuggable (xmpp%:debuggable client))))
        (setf (xml-stream client) xml-stream)
        (xmpp%:open-stream xml-stream)
        (xmpp%:negotiate-tls xml-stream)
        (when (xmpp%:tls-negotiatedp xml-stream)
          (setf (xmpp%::state client) 'connected))))))

(defmethod disconnect-client ((client client))
  (when (connectedp client)
    (with-slots (xml-stream) client
      (let ((connection (xmpp%::connection xml-stream)))
        (xmpp%:close-stream xml-stream)
        (xmpp%:close-connection connection)
        (setf (xmpp%::state client) 'disconnectedp)))))

(defmethod login-client ((client client) &key username password mechanism)
  "SASL authorization over TLS connection, should be called after the connection
is established. In case of error signals negotiate-sasl-condition which should be
handled by the caller."
  (let ((xml-stream (xml-stream client)))
    (when (xmpp%:tls-negotiatedp xml-stream)
      (setf (username client) username
            (password client) password)
      ;; This hell is needed to supress errors.
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
            (proceed-stanza client)
            (setf (xmpp%::state client) 'loggedin)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO:
;;
;; replace proceed-stanza with something else,
;; because it seems like HANDLE-STANZA interface is deprecated now.
;;
;; See README.md for information about hooks, `proceed-stanza` will be replaced
;; by `run-hook`
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (xmpp%:handle-stanza (receive-stanza client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Methods: receive, send
;;

(defmethod receive-stanza ((client client))
  (with-slots (xml-stream) client
    (xmpp%:with-stanza-input (xml-stream stanza)
      stanza)))

(defmethod send-stanza ((client client) stanza-name &rest args)
  (with-slots (xml-stream) client
    (xmpp%:with-stanza-output (xml-stream)
      (apply #'make-instance stanza-name args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: delete
;;
;; DEPRECATED
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun use-xeps (names)
;;   (xmpp%:use-xeps names)
;;   (loop
;;      :for name :in names
;;      :do (let ((xep-methods (getf *xeps-methods* (xmpp%:string-to-keyword name))))
;;            ;; TODO: throw error that xep doesn't exist
;;            (when (and (xmpp%:xep-available-p name) xep-methods)
;;              (loop
;;                 :for method-closure :in xep-methods
;;                 :do (funcall method-closure))))))
