
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
           #:with-stanza-input
           #:with-stanza-output
           #:openedp
           #:closedp
           #:handle-stanza
           #:*default-hostname*
           #:*default-port*))

(defpackage #:cl-ngxmpp-client
  (:use :cl)
  (:export #:create-client
           #:disconnect
           #:authorize
           #:send-message
           #:proceed-stanza
           #:proceed-stanza-loop
           #:connect))
