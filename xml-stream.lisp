
(in-package :cl-ngxmpp)

(defconstant +default-read-seq-size+ 1024)

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
   (state
    :accessor state
    :initarg :state
    :initform 'closed)
   (debuggable
    :accessor debuggable
    :initarg :debuggable
    :initform nil)))

(defcreate xml-stream
  ((:connection connection)
   (:debuggable debuggable)))

(defmethod print-object ((obj xml-stream) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A ~A" (id obj) (symbol-name (state obj)) (debuggable obj))))

(defmethod write-to-stream ((xml-stream xml-stream) string)
  (let ((seq (babel:string-to-octets string))
        (socket-stream (socket-stream (connection xml-stream))))
    (write-sequence seq socket-stream)
    (force-output socket-stream)
    (when (debuggable xml-stream)
      (write-string string *debug-io*)
      (force-output *debug-io*))))

(defmethod read-from-stream ((xml-stream xml-stream))
  (let ((seq (make-array +default-read-seq-size+
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0
                         :adjustable t))
        (socket-stream (socket-stream (connection xml-stream))))
    (loop for byte = (read-byte socket-stream nil 0)
       until (zerop byte)
       do  (vector-push-extend byte seq))
    (babel:octets-to-string seq)))
  
(defmethod openedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'opened))

(defmethod closedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'closed))

(defmethod close-stream ((xml-stream xml-stream))
  (with-stanza-output (xml-stream)
    (make-instance 'stream-stanza-close)))

;; FIXME: when we just load systems cl-ngxmpp and cl-ngxmpp-client
;; and call (cl-ngxmpp-client:connect *client*),
;; we receive error CL-NGXMPP::XML-STREAM is undefined function,
;; but if we just redefine open-stream (C-x C-e in slime) error disappears.
(defmethod open-stream ((xml-stream xml-stream))
  (progn
    (with-stanza-output (xml-stream)
      (make-instance 'stream-stanza
                     :to (hostname (connection xml-stream))
                     :xmlns "jabber:client"))
    (with-stanza-input (xml-stream stanza-input)
      stanza-input)))
             
(defmacro with-stream-xml-input ((xml-stream xml-input) &body body)
  `(let ((,xml-input (cxml:parse (read-from-stream ,xml-stream) (cxml-dom:make-dom-builder))))
     ,@body))
  
(defmacro with-stream-xml-output ((xml-stream) &body body)
  (let ((xml (gensym "xml"))
        (xml-string (gensym "xml-string")))
    `(let* ((,xml (cxml:with-xml-output
                      (cxml:make-octet-vector-sink :canonical nil :indentation 2)
                    ,@body))
            (,xml-string (babel:octets-to-string ,xml)))
       (write-to-stream ,xml-stream ,xml-string))))
