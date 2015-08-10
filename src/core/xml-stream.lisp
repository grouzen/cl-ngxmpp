;;;; xml-stream.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helpers for processing I/O over XML-stream
;; 

(defmacro with-stream-xml-input ((xml-stream xml-input) &body body)
  `(let ((,xml-input (cxml:parse (read-from-stream ,xml-stream) (cxml-dom:make-dom-builder))))
     ,@body))
  
(defmacro with-stream-xml-output ((xml-stream) &body body)
  (let ((xml (gensym "xml"))
        (xml-string (gensym "xml-string")))
    `(let* ((,xml (cxml:with-xml-output
                      (cxml:make-octet-vector-sink :canonical 1)
                    ,@body))
            (,xml-string (babel:octets-to-string ,xml)))
       (write-to-stream ,xml-stream ,xml-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XML STREAM 
;;

(defclass xml-stream (debuggable statefull)
  ((connection :accessor connection :initarg :connection :initform nil)
   (id         :accessor id         :initarg :id         :initform nil)
   (features   :accessor features   :initarg :features   :initform nil)))

(defmethod initialize-instance :after ((xml-stream xml-stream) &key)
  (setf (state xml-stream) 'closed))

(defmethod print-object ((obj xml-stream) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A ~A" (id obj) (symbol-name (state obj)) (debuggable obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; State-related predicates
;;

(defmethod openedp ((xml-stream xml-stream))
  (with-slots (state) xml-stream
    (or (eq state 'opened)
        (eq state 'tls-negotiated)
        (eq state 'sasl-negotiated))))

(defmethod closedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'closed))

(defmethod tls-negotiatedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'tls-negotiated))

(defmethod sasl-negotiatedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'sasl-negotiated))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic operations over xml-stream: write, read, open, close, restart
;;

(defmethod write-to-stream ((xml-stream xml-stream) string)
  (resolve-async-value (adapter-write-to-stream (adapter (connection xml-stream)) string))
  (print-debug xml-stream "Sent: ~A" string))

(defmethod read-from-stream ((xml-stream xml-stream) &key (stanza-reader 'stanza-reader))
  (let ((result (resolve-async-value (adapter-read-from-stream (adapter (connection xml-stream)) :stanza-reader stanza-reader))))
    (print-debug xml-stream "Received: ~A" result)
    result))

(defmethod close-stream ((xml-stream xml-stream))
  (with-stream-xml-output (xml-stream)
    (stanza-to-xml (make-instance 'stream-close-stanza)))
  (setf (state xml-stream) 'closed))

(defmethod restart-stream ((xml-stream xml-stream))
  (setf (features xml-stream) nil)
  (setf (id xml-stream) nil)
  (open-stream xml-stream))

(defmethod open-stream ((xml-stream xml-stream))
  ;; Send <stream:stream> to initiate connection
  (write-to-stream xml-stream
                   (format nil "<?xml version='1.0'?><stream:stream to='~A' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>"
                           (hostname (connection xml-stream))))
  ;; Read <?xml?> header
  (read-from-stream xml-stream :stanza-reader 'stanza-reader-header)
  ;; Read <stream:stream> with features
  (let* ((features-result
          (format nil "~A</stream:stream>" (read-from-stream xml-stream :stanza-reader 'stanza-reader-features)))
         (features-result-xml (cxml:parse features-result (cxml-dom:make-dom-builder)))
         (features-stanza (xml-to-stanza (make-instance 'stanza :xml-node features-result-xml))))
    (setf (state xml-stream) 'opened)
    (setf (features xml-stream) features-stanza)
    (print-debug xml-stream "Received stream: ~A" features-result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The smallest piece of data in XMPP is so called 'stanza', in fact it is just a string.
;; The reason why we need this code is that XMPP doesn't send any terminating symbols after
;; stanzas, so we can't easily split a stream into separate XML pieces. So, to be able to do
;; this we can use classical FSM approach.
;;
;; FSM for xml reading.
;;
;; Most of code taken and ported from:
;; https://github.com/dmatveev/shampoo-emacs/blob/8302cc4e14653980c2027c98d84f9aa3d1b59ebb/shampoo.el#L400
;;
;; Thanks, yoghurt!
;;

(define-condition stanza-reader-error (proxy-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In some cases XMPP forces us to work with malformed XML, so there are 
;; several stanza-reader implementations:
;;
;;   * standard `stanza-reader` reads a valid XML
;;   * `stanza-reader-header` reads XML header '<?xml version="1.0"?>'.
;;      As far as you can see, the header actually is not a valid XML - tags are not balanced.
;;   * `stanza-reader-features` reads XMPP's 'features'
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default reader for valid XML
;;

(defclass stanza-reader ()
  ((stanza-stream :accessor stanza-stream :initarg :stanza-stream :initform nil)
   (state         :accessor state         :initarg :state         :initform :init)
   (depth         :accessor depth         :initarg :depth         :initform 0)
   (last-chars    :accessor last-chars    :initarg :last-chars    :initform nil)
   (result        :accessor result        :initarg :result        :initform
                  (make-array 4096 :element-type 'character :fill-pointer 0 :adjustable t))))

(defmethod print-object ((obj stanza-reader) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "state: ~A, depth: ~D, result: ~A, last-chars: ~A"
            (state obj) (depth obj) (result obj) (last-chars obj))))

(defmethod stanza-reader-complete-p ((stanza-reader stanza-reader))
  (let ((state (state stanza-reader))
        (depth (depth stanza-reader)))
    (and (eq depth 0)
         (or (eq state :node-closed)
             (eq state :tag-closed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reader for XML header
;;

(defclass stanza-reader-header (stanza-reader) ())

(defmethod stanza-reader-complete-p ((stanza-reader stanza-reader-header))
  (let ((state (state stanza-reader))
        (depth (depth stanza-reader)))
    (and (eq depth 1)
         (eq state :node-opened))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reader for XMPP's 'feature' stanza
;;

(defclass stanza-reader-features (stanza-reader)
  ((push-result
    :accessor push-result
    :initarg :push-result
    :initform nil)))

(defmethod stanza-reader-complete-p ((stanza-reader stanza-reader-features))
  (let ((state (state stanza-reader))
        (depth (depth stanza-reader)))
    (and (eq depth 1)
         (eq state :node-closed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FSM actions
;;

(defmethod stanza-reader-switch ((stanza-reader stanza-reader) state)
  (let ((current-state (state stanza-reader)))
    (when (not (eq current-state state))
      (setf (state stanza-reader) state)
      (incf (depth stanza-reader)
            (cond ((eq state :tag-opened) 1)
                  ((eq state :tag-closed) -1)
                  ((eq state :node-closed) -1)
                  (t 0))))))

(defmethod stanza-reader-process ((stanza-reader stanza-reader))
  (let* ((state (state stanza-reader))
         (next-state
          (cond ((eq state :init)
                 (when (stanza-reader-<-p stanza-reader)
                   :tag-opened))
                
                ((eq state :tag-opened)
                 (cond ((stanza-reader->-p  stanza-reader) :node-opened)
                       ((stanza-reader-/>-p stanza-reader) :tag-closed)))

                ((eq state :tag-closed)
                 (cond ((stanza-reader-</-p stanza-reader) :node-closing)
                       ((stanza-reader-<-p  stanza-reader) :tag-opened)))

                ((eq state :node-opened)
                 (cond ((stanza-reader-<-p  stanza-reader) :tag-opened)
                       ((stanza-reader-</-p stanza-reader) :node-closing)))

                ((eq state :node-closing)
                 (when (stanza-reader->-p stanza-reader)
                   :node-closed))

                ((eq state :node-closed)
                 (cond ((stanza-reader-<-p  stanza-reader) :tag-opened)
                       ((stanza-reader-</-p stanza-reader) :node-closing)))

                (t nil))))
    (when next-state
      (stanza-reader-switch stanza-reader next-state))))

(defmethod stanza-reader-push-result ((stanza-reader stanza-reader))
  (vector-push-extend (stanza-reader-read-char stanza-reader) (result stanza-reader)))

(defmethod stanza-reader-push-last-chars ((stanza-reader stanza-reader) char)
  (setf (last-chars stanza-reader) (append (list char) (last-chars stanza-reader))))

(defmethod stanza-reader-pop-last-chars ((stanza-reader stanza-reader))
  (let ((char (car (last-chars stanza-reader))))
    (setf (last-chars stanza-reader) (cdr (last-chars stanza-reader)))
    char))

(defmethod stanza-reader-read-char ((stanza-reader stanza-reader))
  (if (null (last-chars stanza-reader))
      (read-char (stanza-stream stanza-reader) t)
      (stanza-reader-pop-last-chars stanza-reader)))

(defmethod stanza-reader-read-stream ((stanza-reader stanza-reader))
  (loop
     :until (stanza-reader-complete-p stanza-reader)
     :do (with-proxy-error stanza-reader-error
               (end-of-file)
             (stanza-reader-process stanza-reader)
             (stanza-reader-push-result stanza-reader)))
  ;; TODO: remove this hack
  (mapcar #'(lambda (c) (vector-push-extend c (result stanza-reader))) (last-chars stanza-reader))
  stanza-reader)

(defmacro stanza-reader-with-char ((stanza-reader char) &body body)
  (let ((result (gensym "result")))
    `(let* ((,char (stanza-reader-read-char ,stanza-reader))
            (,result ,@body))
       (stanza-reader-push-last-chars ,stanza-reader ,char)
       ,result)))
       
(defmethod stanza-reader-<-p ((stanza-reader stanza-reader))
  (stanza-reader-with-char (stanza-reader current-char)
    (stanza-reader-with-char (stanza-reader next-char)
      (and (eq current-char #\<)
           (not (eq next-char #\/))))))
          
(defmethod stanza-reader->-p ((stanza-reader stanza-reader))
  (stanza-reader-with-char (stanza-reader current-char)
    (eq current-char #\>)))

(defmethod stanza-reader-/>-p ((stanza-reader stanza-reader))
  (stanza-reader-with-char (stanza-reader current-char)
    (stanza-reader-with-char (stanza-reader next-char)
        (and (eq current-char #\/)
             (eq next-char #\>)))))

(defmethod stanza-reader-</-p ((stanza-reader stanza-reader))
  (stanza-reader-with-char (stanza-reader current-char)
    (stanza-reader-with-char (stanza-reader next-char)
      (and (eq current-char #\<)
           (eq next-char #\/)))))

