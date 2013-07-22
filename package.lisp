
(defpackage #:cl-ngxmpp
  (:use :cl)
  (:export #:create-connection
           #:create-xml-stream
           #:defcreate
           #:connect
           #:disconnect
           #:negotiate-tls
           #:negotiate-sasl
           #:connectedp
           #:open-stream
           #:close-stream
           #:create-stream
           #:connection
           #:openedp
           #:closedp
           #:*default-hostname*
           #:*default-port*))

(defpackage #:cl-ngxmpp-client
  (:use :cl)
  (:export #:create-client
           #:disconnect
           #:connect))
