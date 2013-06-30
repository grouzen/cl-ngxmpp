
(defpackage #:cl-ngxmpp
  (:use :cl)
  (:export #:create-connection
           #:create-xml-stream
           #:defcreate
           #:connect
           #:disconnect
           #:connectedp
           #:open-stream
           #:close-stream
           #:create-stream
           #:connection
           #:*default-hostname*
           #:*default-port*))

(defpackage #:cl-ngxmpp-client
  (:use :cl)
  (:export #:create-client
           #:connect))
