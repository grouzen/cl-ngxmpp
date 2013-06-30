
(defpackage #:cl-ngxmpp
  (:use :cl)
  (:export #:connect
           #:disconnect
           #:connectedp
           #:open-stream
           #:close-stream
           #:create-stream
           #:*default-hostname*
           #:*default-port*))

(defpackage #:cl-ngxmpp-ext
  (:use :cl)
  (:export #:connect))
