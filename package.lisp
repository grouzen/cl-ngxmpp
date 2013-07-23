
(defpackage #:cl-ngxmpp
  (:use :cl)
  (:export #:*default-hostname*
           #:*default-port*
           ;; Methods
           #:create-connection
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
           ;; Stanzas
           #:message-stanza
           #:iq-result-stanza
           #:iq-set-bind-stanza
           #:iq-set-session-stanza
           #:presence-stanza))

(defpackage #:cl-ngxmpp-client
  (:use :cl)
  (:export #:create-client
           #:disconnect
           #:authorize
           #:send-message
           #:send-presence
           #:proceed-stanza
           #:proceed-stanza-loop
           #:connect))
