;;;; xml-stream.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

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
    :initform nil
    :documentation
    "Instance of class `stream-stanza-features'")
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
  (let ((socket-stream (socket-stream (connection xml-stream))))
    (write-string string socket-stream)
    (force-output socket-stream)
    (when (debuggable xml-stream)
      (write-line (format nil "Sent: ~A" string) *debug-io*)
      (force-output *debug-io*))))

(defmethod read-from-stream ((xml-stream xml-stream))
  (let* ((socket-stream (socket-stream (connection xml-stream)))
         (result (result (stanza-reader-read-stream
                          (make-instance 'stanza-reader :stanza-stream socket-stream)))))
    (when (debuggable xml-stream)
      (write-line (format nil "Received: ~A" result) *debug-io*)
      (force-output *debug-io*))
    result))

(defmethod openedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'opened))

(defmethod closedp ((xml-stream xml-stream))
  (eq (state xml-stream) 'closed))

(defmethod close-stream ((xml-stream xml-stream))
  (setf (state xml-stream) 'closed)
  (with-stanza-output (xml-stream)
    (make-instance 'stream-close-stanza)))

(defmethod restart-stream ((xml-stream xml-stream))
  (setf (features xml-stream) nil)
  (setf (id xml-stream) nil)
  (open-stream xml-stream))

(defmethod has-mandatory-to-negotiate ((xml-stream xml-stream))
  t)

(defmethod open-stream ((xml-stream xml-stream))
  (let ((socket-stream (socket-stream (connection xml-stream))))
    ;; Send <stream:stream> to initiate connection
    (write-to-stream xml-stream
                     (format nil "<?xml version='1.0'?><stream:stream to='~A' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>"
                             (hostname (connection xml-stream))))
    ;; Read <?xml?> header
    (stanza-reader-read-stream (make-instance 'stanza-reader-header :stanza-stream socket-stream))
    ;; Read <stream:stream> with features
    (let* ((features-result
            (format nil "~A</stream:stream>"
                    (result (stanza-reader-read-stream
                             (make-instance 'stanza-reader-features :stanza-stream socket-stream)))))
           (features-result-xml (cxml:parse features-result (cxml-dom:make-dom-builder)))
           (features-stanza (xml-to-stanza (make-instance 'stanza :xml-node features-result-xml))))
      (setf (state xml-stream) 'opened)
      (setf (features xml-stream) features-stanza)
      (when (debuggable xml-stream)
        (write-line (format nil "Received stream: ~A" features-result) *debug-io*)
        (print features-stanza *debug-io*)
        (force-output *debug-io*)))))
             
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

;;
;; FSM for xml reading.
;;
;; Most of code taken and ported from:
;; https://github.com/dmatveev/shampoo-emacs/blob/8302cc4e14653980c2027c98d84f9aa3d1b59ebb/shampoo.el#L400
;;
(defclass stanza-reader ()
  ((stanza-stream
    :accessor stanza-stream
    :initarg :stanza-stream
    :initform nil)
   (state
    :accessor state
    :initarg :state
    :initform :init)
   (depth
    :accessor depth
    :initarg :depth
    :initform 0)
   (last-chars
    :accessor last-chars
    :initarg :last-chars
    :initform nil)
   (result
    :accessor result
    :initarg :result
    :initform (make-array 4096 :element-type 'character :fill-pointer 0 :adjustable t))))

(defmethod print-object ((obj stanza-reader) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "state: ~A, depth: ~D, result: ~A, last-chars: ~A"
            (state obj) (depth obj) (result obj) (last-chars obj))))

(defmethod stanza-reader-switch ((stanza-reader stanza-reader) state)
  (let ((current-state (state stanza-reader)))
    (when (not (eq current-state state))
      (setf (state stanza-reader) state)
      (incf (depth stanza-reader)
            (cond ((eq state :tag-opened) 1)
                  ((eq state :tag-closed) -1)
                  ((eq state :node-closed) -1)
                  (t 0))))))
        
(defmethod stanza-reader-complete-p ((stanza-reader stanza-reader))
  (let ((state (state stanza-reader))
        (depth (depth stanza-reader)))
    (and (eq depth 0)
         (or (eq state :node-closed)
             (eq state :tag-closed)))))

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
      (read-char (stanza-stream stanza-reader) nil :eof)
      (stanza-reader-pop-last-chars stanza-reader)))

(defmethod stanza-reader-read-stream ((stanza-reader stanza-reader))
  (loop
     until (stanza-reader-complete-p stanza-reader)
     do (progn
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


(defclass stanza-reader-header (stanza-reader) ())

(defmethod stanza-reader-complete-p ((stanza-reader stanza-reader-header))
  (let ((state (state stanza-reader))
        (depth (depth stanza-reader)))
    (and (eq depth 1)
         (eq state :node-opened))))


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
