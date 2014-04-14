
(defpackage #:cl-ngxmpp-client.high.examples)

(in-package #:cl-ngxmpp-client.high.examples)


(defparameter *session* (cl-ngxmpp-client:create-session
                         :server-hostname "ch3kr.net"
                         :username        "clngxmpp"
                         :password        "clngxmpp"))

(cl-ngxmpp-client:open-session *session*)


                  
