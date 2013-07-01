
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
       do (progn (princ byte) (vector-push-extend byte seq)))
    (babel:octets-to-string seq)))
  
;(defmacro with-xml-stream ((xml-stream &rest slots) &body body)
;  `(with-slots (,xml-stream ,@slots)
;       ,@body))

(defmethod openedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'opened))

(defmethod closedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'closed))

(defmethod close-stream ((xml-stream xml-stream))  
  (with-stream-xml-output (xml-stream)
    (cxml:with-element "stream:stream")))
    
(defmethod open-stream ((xml-stream xml-stream))
  (progn
    (with-stream-xml-output (xml-stream)
      (cxml:with-element "stream:stream"
        (cxml:attribute "to" (hostname (connection xml-stream)))
        (cxml:attribute "xmlns" "jabber:client")
        (cxml:attribute "xmlns:stream" "http://etherx.jabber.org/streams")
        (cxml:attribute "version" "1.0")))))
 ;   (cxml:parse (read-from-stream xml-stream)
 ;               (make-instance 'xml-stream-handler :xml-stream xml-stream))))

;(defmacro with-stream-xml-input ((stream) &body body)
;  ,@body)

(defmacro with-stream-xml-output ((xml-stream) &body body)
  (let ((xml (gensym "xml"))
        (xml-string (gensym "xml-string")))
    `(let* ((,xml (cxml:with-xml-output
                      (cxml:make-octet-vector-sink :canonical nil :indentation 2)
                    ,@body))
            (,xml-string (babel:octets-to-string ,xml)))
       (write-to-stream ,xml-stream ,xml-string))))

;(defclass xml-stream-handler (sax:default-handler)
;  ((xml-stream
;    :accessor xml-stream
;    :initarg :xml-stream
;    :initform nil)
;   (answer
;    :accessor :answer
;    :initarg :initarg
;    :initform nil)))

;(defmethod sax:start-element ((handler xml-stream-handler) namespace-uri lname qname attrs)
;  (
