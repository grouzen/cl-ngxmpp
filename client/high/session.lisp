;;;; session.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client)

(defclass session (client)
  ((mechanism :accessor mechanism :initarg :mechanism :initform nil)))

(defun create-session (&key server-hostname (server-port 5222)
                         username password (mechanism nil) (debuggable nil))
  (make-instance 'session
                 :server-hostname server-hostname
                 :server-port     server-port
                 :username        username
                 :password        password
                 :mechanism       mechanism
                 :debuggable      debuggable))

(defmethod open-session ((session session))
  (connect session)
  (when (cl-ngxmpp:openedp (xml-stream session))
    (with-slots (username password mechanism) session
      (authorize session :username username :password password :mechanism mechanism))))

(defmethod close-session ((session session))
  (disconnect session))
 
      
                 
                 
                         
