
(defpackage #:cl-ngxmpp-client.high.examples)

(in-package #:cl-ngxmpp-client.high.examples)


(defparameter *session* (xmpp:create-session
                         :server-hostname "ch3kr.net"
                         :username        "clngxmpp"
                         :password        "clngxmpp"))

(xmpp:open-session *session*)


                  
