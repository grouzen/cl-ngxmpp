
(in-package :cl-ngxmpp-ext)

(defun connect (username password
                &key (hostname cl-ngxmpp:*default-hostname*) (port cl-ngxmpp:*default-port*) (debuggable nil))
  (let ((connection (cl-ngxmpp:connect :hostname hostname :port port)))
    (when (cl-ngxmpp:connectedp connection)
      (cl-ngxmpp:open-stream
       (cl-ngxmpp:create-stream connection :debuggable debuggable)))
    connection))
        
