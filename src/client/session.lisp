;;;; session.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-client)

;; (defclass session (client)
;;   ((mechanism :accessor mechanism :initarg :mechanism :initform nil)
;;    (xeps-list :accessor xeps-list :initarg :xeps-list :initform nil)
;;    (domains   :accessor domains   :initarg :domains   :initform nil)))

;; (defun create-session (&key server-hostname (server-port 5222)
;;                          username password (mechanism nil) (debuggable nil)
;;                          (xeps-list nil))
;;   "Creates a new connection to server, logs in, and uses xeps
;; from XEPS-LIST. When XEPS-LIST is NIL, all available xeps will be used."
;;   (use-xeps xeps-list)
;;   (make-instance 'session
;;                  :server-hostname server-hostname
;;                  :server-port     server-port
;;                  :username        username
;;                  :password        password
;;                  :xeps-list       xeps-list
;;                  :mechanism       mechanism
;;                  :debuggable      debuggable))

;; (defmethod open-session ((session session))
;;   "Creates a new connection to the server, logs in. After this
;; step you can send and receive stanzas.
;; Exceptional situations:
;; If SERVER-HOSTNAME is wrong or not available, an error of type
;; CL-NGXMPP:CONNECTION-ERROR is signaled. If SERVER-PORT is closed
;; on SERVER-HOSTNAME, an error of type CL-NGXMPP:CONNECTION-ERROR is signaled.
;; If authorize step is failed for any reason, an error of type
;; CL-NGXMPP:NEGOTIATE-SASL-ERROR is signaled."
;;   (with-slots (username password mechanism xml-stream) session
;;     (connect session)
;;     (when (xmpp%:openedp xml-stream)
;;       (authorize session
;;                  :username username
;;                  :password password
;;                  :mechanism mechanism))))

;; (defmethod close-session ((session session))
;;   (disconnect session))


;; New API

;; (defclass session ()
;;   ((client    :accessor client    :initarg :client    :initform nil)
;;    (xeps-list :accessor xeps-list :initarg :xeps-list :initform nil)
;;    (domains   :accessor domains   :initarg :domains   :initform nil)
;;    (hooks     :accessor hooks     :initarg :hooks     :initform nil)))
                 
;; (defun open-session (&key server-hostname (server-port 5222)
;;                        username password (mechanism nil) (debuggable nil)
;;                        (xeps-list nil) (hooks nil))
;;   (let ((client (make-instance 'client)))
;;     (xmpp%::chain-statefull
;;         (connect-client client
;;                         :server-hostname server-hostname
;;                         :server-port     server-port)
;;       ((connectedp client) (login-client client
;;                                          :username username
;;                                          :password password
;;                                          :mechanism mechanism)))))

  
