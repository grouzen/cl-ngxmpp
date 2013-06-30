
(in-package :cl-ngxmpp-ext)

(defun connect (username password
                &key (hostname cl-ngxmpp:*default-hostname*) (port cl-ngxmpp:*default-port*) (debuggable nil))
  (let ((connection (cl-ngxmpp:create-connection :hostname hostname :port port)))
    (when (cl-ngxmpp:connectedp (cl-ngxmpp:connect connection))
      (cl-ngxmpp:open-stream
       (cl-ngxmpp:create-xml-stream :connection connection :debuggable debuggable)))
    connection))
        
