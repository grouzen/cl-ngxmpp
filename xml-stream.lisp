
(in-package :cl-ngxmpp)

(defclass xml-stream ()
  ((connection
    :accessor connection
    :initarg :connection
    :initform nil)
   (id
    :accessor id
    :initarg :id
    :initform nil)
   (features
    :accessor features
    :initarg :features
    :initform nil)
   (stanza-callback
    :accessor stanza-callback
    :initarg :stanza-callback
    :initform nil)
   (state
    :accessor state
    :initarg :state
    :initform 'closed)
   (debuggable
    :accessor debuggable
    :initarg :debuggable
    :initform nil)))

(defmethod write-to-stream ((stream xml-stream) string)
  (let ((seq (babel:string-to-octets string))
        (socket-stream (socket-stream (connection stream))))
    (write-sequence seq socket-stream)
    (force-output socket-stream)
    (when (debuggable stream)
      (write-string string *debug-io*))
    (force-output *debug-io*)))

(defmethod close-stream ((stream xml-stream))
  (with-xml (stream)
    (cxml:with-element "stream:stream")))
    
(defmethod open-stream ((stream xml-stream))
  (with-xml (stream)
    (cxml:with-element "stream:stream"
      (cxml:attribute "to" (hostname (connection stream)))
      (cxml:attribute "xmlns" "jabber:client")
      (cxml:attribute "xmlns:stream" "http://etherx.jabber.org/streams")
      (cxml:attribute "version" "1.0"))))
  

(defmacro with-xml ((xml-stream) &body body)
  (let ((xml (gensym "xml"))
        (xml-string (gensym "xml-string"))
        (stream (gensym "stream"))
        (debuggable (gensym "debuggable")))
    `(let ((,stream (socket-stream (connection ,xml-stream)))
           (,debuggable (debuggable ,xml-stream)))
       (prog1
           (let* ((,xml (cxml:with-xml-output
                            (cxml:make-octet-vector-sink :canonical nil :indentation 2)
                          ,@body))
                  (,xml-string (babel:octets-to-string ,xml)))
             (write-to-stream ,xml-stream ,xml-string))))))

(defun create-stream (connection &key (debuggable nil))
  (make-instance 'xml-stream :connection connection :debuggable debuggable))
