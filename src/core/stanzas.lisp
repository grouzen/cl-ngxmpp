;;;; stanzas.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tbe macros for receiving/sending stanzas.
;;

(defmacro with-stanza-output ((xml-stream) &body body)
  `(with-stream-xml-output (,xml-stream)
     (stanza-to-xml ,@body)))

(defmacro with-stanza-input ((xml-stream stanza-input dispatchers) &body body)
  (let ((xml-input (gensym "xml-input")))
    `(with-stream-xml-input (,xml-stream ,xml-input)
       (let ((,stanza-input (xml-to-stanza
                             (make-instance 'stanza :xml-node ,xml-input)
                             ,dispatchers)))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CXML-related stuff
;;

(defun get-elements-by-name (node el-name)
  (let ((elems nil))
    (loop :for el :across (dom:child-nodes node)
       :do (when (equal (dom:node-name el) el-name)
             (push el elems)))
    elems))

(defun get-element-by-name (node el-name)
  (loop :for el :across (dom:child-nodes node)
     :do (when (equal (dom:node-name el) el-name)
           (return el))))

(defun get-element-data (node)
  (let ((child (dom:first-child node)))
    (if (null child)
        ""
        (dom:data child))))

;; TODO: come up with a better name for this
(defun get-stanza-xml-string (stanza)
  (babel:octets-to-string
   (cxml:with-xml-output (cxml:make-octet-vector-sink :canonical 1)
     (stanza-to-xml stanza))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;
;; The 'protocol' to define stanzas
;;

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defvar *stanzas-dispatchers* nil))

(defun dispatch-stanza (stanza super-stanza-class dispatchers)
  (let ((xep-disps (getf dispatchers
                         (string-to-keyword (symbol-name super-stanza-class)))))
    (labels ((dispatch (disp-list)
               (if (null disp-list)
                   (make-instance 'unknown-stanza :xml-node (xml-node stanza))
                   (let* ((current (car disp-list))
                          (target-stanza-class (first current))
                          (target-dispatcher   (second current)))
                     (if (funcall target-dispatcher stanza)
                         (make-stanza stanza target-stanza-class)
                         (dispatch (cdr disp-list)))))))
      (dispatch xep-disps))))
    
(defgeneric make-stanza (stanza class-name dispatchers) 
  (:documentation
   "This method makes a new instance of `class-name' stanza,
fills it with necessary fields taken from a parent,
and calls `xml-to-stanza' method with the new instance. This method
needs to be implemented only for parental classes"))

(defgeneric xml-to-stanza (stanza dispatchers)
  (:documentation
   "Transforms xml to one of the children of STANZA class."))

(defgeneric stanza-to-xml (stanza)
  (:documentation
   "Transforms a particular stanza instance to an xml object which then will be converted to string."))

(defgeneric handle-stanza (stanza)
  (:documentation
   "This handler must be overrided on a client code."))

(define-condition handle-stanza-error (simple-condition) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use `defstanza` macro to define new stanzas
;;
;; Example of usage:
;;
;; (defstanza stanza ()
;;     (xml-node)
;;   (handle-stanza ((stanza))
;;     (error 'handle-stanza-error
;;            :format-control "Default stanza handler called. Please define handler for this type of stanza"))
;;  
;;   (make-stanza ((stanza) class-name)
;;     (xml-to-stanza (make-instance class-name :xml-node (xml-node stanza))))
;;
;;   (cool-method ((obj (conrete-obj concrete-class)) args)
;;     ...)
;;
;;   (xml-to-stanza (stanza)
;;     ...))
;;

;;
;; Compile-time errors
;;

(define-condition defstanza-method%-error (simple-condition)
  ((arg :reader arg :initarg :arg))
  (:report (lambda (condition stream)
             (format stream
                     "Argument ~A is neither a non-NIL symbol nor a list of form '(obj class)"
                     (arg condition)))))

(define-condition defstanza-class%-error (simple-condition)
  ((slot :reader slot :initarg :slot))
  (:report (lambda (condition stream)
             (format stream
                     "Slot ~A is neither a non-NIL symbol nor a list of form '(name initform)"
                     (slot condition)))))

(defmacro defstanza (stanza-name superclasses slots &rest methods)
  `(progn
     (defstanza-class% ,stanza-name ,superclasses ,slots)
     (defstanza-methods% ,stanza-name ,methods)))

(defmacro defstanza-class% (stanza-name superclasses slots)
  (let ((slotz (mapcar #'(lambda (slot)
                           (let* ((ds (cond ((listp slot)
                                             (if (> (length slot) 2)
                                                 (error 'defstanza-class%-error :slot slot)
                                                 slot))
                                            (t (list slot nil))))
                                  (name (first ds))
                                  (initform (second ds)))
                             (list name
                                   :accessor name
                                   :initarg (alexandria:make-keyword name)
                                   :initform initform)))
                       slots)))
    `(defclass ,stanza-name (,@superclasses) (,@slotz))))

(defmacro defstanza-methods% (stanza-name methods)
  `(progn ,@(mapcar #'(lambda (method)
              (if (eq (first method) :macro)
                  `(defmacro ,@(cdr `,method))
                  (let ((name (first method))
                        (args (second method))
                        (body (cdr (cdr method))))
                    `(defstanza-method% ,stanza-name ,name ,args ,@body))))
          methods)))

(defmacro defstanza-method% (stanza-name method-name method-args &body method-body)
  (let ((obj-args (mapcar #'(lambda (arg)
                              (cond ((listp arg)
                                     (if (> (length arg) 2)
                                         (error 'defstanza-method%-error :arg arg)
                                         arg))
                                    (t (list arg stanza-name))))
                          (first method-args)))
        (rest-args (cdr method-args)))
    `(defmethod ,method-name (,@obj-args ,@rest-args)
       ,@method-body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: doc
;;

(defstanza meta-element ()
    (xml-node xmlns)
    
  (make-stanza ((stanza) class-name dispatchers)
    (xml-to-stanza (make-instance class-name :xml-node (xml-node stanza)) dispatchers)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic stanza class.
;;
;; TODO:
;;   - export children of stanza class from cl-ngxmpp package.
;;   - stanza can't be an entry point anymore, since xmpp stream consists of a lot of
;      different XML elements.
;;

(defstanza stanza (meta-element)
    ;; According to the definition of "stanza" -- an each type of stanza should have
    ;; at least 4 attributes in its root element: id, to, from, type
    (id to from stanza-type)

  ;; (make-stanza ((stanza) class-name)
  ;;   (make-instance class-name
  ;;                  :xml-node    (xml-node    stanza)
  ;;                  :id          (id          stanza)
  ;;                  :to          (to          stanza)
  ;;                  :from        (from        stanza)
  ;;                  :stanza-type (stanza-type stanza)))
               
  ;; (handle-stanza ((stanza))
  ;;   (error 'handle-stanza-error
  ;;          :format-control "Default stanza handler was called. Please define handler for this type of stanza"))

  (handle-stanza ((stanza)) t)
  
  (xml-to-stanza ((stanza) dispatchers)
    (let ((qname (dom:node-name (dom:first-child (xml-node stanza)))))
      (string-case qname
        ("stream:stream"   (make-stanza stanza 'stream-element dispatchers))
        ("message"         (make-stanza stanza 'message-stanza dispatchers))
        ("failure"         (make-stanza stanza 'failure-element dispatchers))
        ("success"         (make-stanza stanza 'success-element dispatchers))
        ("proceed"         (make-stanza stanza 'proceed-element dispatchers))
        ("challenge"       (make-stanza stanza 'sasl-challenge-element dispatchers))
        ("iq"              (make-stanza stanza 'iq-stanza dispatchers))
        ("presence"        (make-stanza stanza 'presence-stanza dispatchers))
        (:default          (dispatch-stanza stanza 'stanza dispatchers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza unknown-stanza (stanza)
    ()
  
  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Unknown type of stanza: ~A" (dom:node-name (dom:first-child (xml-node obj))))))

  (xml-to-stanza ((stanza) dispatchers)
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stream elements
;;

(defstanza stream-element (meta-element)
    (id (to "") (from "") (xml-lang "en") (xmlns "jabber:client")
        (xmlns-stream "http://etherx.jabber.org/streams") (version "1.0"))
    
  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "to: ~A, from: ~A, id: ~A, xml-lang: ~A, xmlns: ~A, xmlns-stream: ~A, version: ~A"
              (to obj) (from obj) (id obj) (xml-lang obj) (xmlns obj) (xmlns-stream obj) (version obj))))


  (make-stanza ((stanza) class-name dispatchers)
    (let* ((xml-node (xml-node stanza))
           (stream-node (dom:first-child xml-node)))
      (xml-to-stanza (make-instance class-name
                                    :xml-node     xml-node
                                    :to           (dom:get-attribute stream-node "to")
                                    :from         (dom:get-attribute stream-node "from")
                                    :id           (dom:get-attribute stream-node "id")
                                    :xml-lang     (dom:get-attribute stream-node "xml:lang")
                                    :xmlns        (dom:get-attribute stream-node "xmlns")
                                    :xmlns-stream (dom:get-attribute stream-node "xmlns:stream")
                                    :version      (dom:get-attribute stream-node "version"))
                     dispatchers)))

  (xml-to-stanza ((stanza) dispatchers)
    (let* ((xml-node (xml-node stanza))
           (child-qname (dom:node-name (dom:first-child (dom:first-child xml-node)))))
      (string-case child-qname
        ("stream:features" (make-stanza stanza 'stream-features-element dispatchers))
        ("stream:error"    (make-stanza stanza 'stream-error-element dispatchers))
        (:default          (dispatch-stanza stanza 'stream-element dispatchers)))))

  (stanza-to-xml ((stanza))
    (cxml:with-element "stream:stream"
      (cxml:attribute "to" (to stanza))
      (cxml:attribute "id" (id stanza))
      (cxml:attribute "xmlns" (xmlns stanza))
      (cxml:attribute "xmlns:stream" (xmlns-stream stanza))
      (cxml:attribute "version" (version stanza)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza stream-features-element (stream-element)
    (features)

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (mapcar #'(lambda (feature)
                  (let ((feature-name (car feature))
                        (feature-required (cdr feature)))
                    (format stream "{~A -> ~A} " feature-name
                            (if feature-required
                                "required"
                                "not required"))))
              (features obj)))
    (call-next-method obj stream))

  (xml-to-stanza ((stanza) dispatchers)
    (let ((features (features stanza)))
      (dom:map-node-list
       #'(lambda (node)
           (let* ((feature-name (dom:node-name node))
                  (feature-required (string-case feature-name
                                      ("session"    t) ;; TODO: implement required checking.
                                      ("mechanisms" t) ;; These four features are 
                                      ("starttls"   t) ;; mandatory-to-negitiate for 
                                      ("bind"       t) ;; client and server, see RFC 6120 and RFC 3921.
                                      (:default     nil)))) ;; TODO: check on <required/> element
             (setf features (cons (cons feature-name feature-required) features))))
       (dom:child-nodes (dom:first-child (dom:first-child (xml-node stanza)))))
      (setf (features stanza) features)
      stanza)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza stream-close-element (stream-element)
    ()
  
  (xml-to-stanza ((stanza) dispatchers)
    stanza)

  (stanza-to-xml ((stanza))
    (cxml:with-element "stream:stream")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza stream-error-element (stream-element)
    (error-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Message stanzas
;;

(defstanza message-stanza (stanza)
    (thread (body ""))

  (print-object ((obj) stream)
    (with-slots (from to body) obj
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream "from: ~A, to: ~A, body: ~A"
                from to body))))
  
  (:macro with-message-stanza (message-stanza &body body)
    `(cxml:with-element "message"
       (unless (null (from ,message-stanza))
         (cxml:attribute "from" (from ,message-stanza)))
       (unless (null (to ,message-stanza))
         (cxml:attribute "to" (to ,message-stanza)))
       (unless (null (stanza-type ,message-stanza))
         (cxml:attribute "type" (stanza-type ,message-stanza)))
       (cxml:with-element "body"
         (cxml:text (body ,message-stanza)))
       ,@body))
  
  (stanza-to-xml ((stanza))
    (with-message-stanza stanza))

  (make-stanza ((stanza) class-name dispatchers)
    (let* ((message-node (dom:first-child (xml-node stanza)))
           (to           (dom:get-attribute message-node "to"))
           (from         (dom:get-attribute message-node "from"))
           (stanza-type (dom:get-attribute message-node "type"))
           (body         (get-element-data (get-element-by-name message-node "body"))))
      (xml-to-stanza (make-instance class-name
                                    :xml-node     (xml-node stanza)
                                    :stanza-type stanza-type
                                    :from         from
                                    :to           to
                                    :body         body)
                     dispatchers)))

  (xml-to-stanza ((stanza) dispatchers)
    (let* ((xml-node     (xml-node stanza))
           (message-node (dom:first-child xml-node))
           (to           (dom:get-attribute message-node "to"))
           (from         (dom:get-attribute message-node "from"))
           (body         (get-element-data (get-element-by-name message-node "body")))
           (disp         (dispatch-stanza stanza 'message-stanza dispatchers)))
      (if (typep disp 'unknown-stanza)
          (progn
            (setf (to stanza) to
                  (from stanza) from
                  (body stanza) body)
            stanza)
          disp))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza message-error-stanza (message-stanza)
    ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Presence stanzas
;;

(defstanza presence-stanza (stanza)
    (show status priority)

  (:macro with-presence-stanza (presence-stanza &body body)
    `(cxml:with-element "presence"
       (unless (null (id ,presence-stanza))
         (cxml:attribute "id" (id ,presence-stanza)))
       (unless (null (to ,presence-stanza))
         (cxml:attribute "to"   (to ,presence-stanza)))
       (unless (null (from ,presence-stanza))
         (cxml:attribute "from" (from ,presence-stanza)))
       (unless (null (stanza-type ,presence-stanza))
         (cxml:attribute "type" (stanza-type ,presence-stanza)))
       ,@body))

  (stanza-to-xml ((stanza))
    (with-presence-stanza stanza))

  (make-stanza ((stanza) class-name dispatchers)
    (let* ((xml-node      (xml-node stanza))
           (presence-node (dom:first-child xml-node))
           (stanza-type (dom:get-attribute presence-node "type"))
           (to            (dom:get-attribute presence-node "to"))
           (from          (dom:get-attribute presence-node "from")))
      (xml-to-stanza (make-instance class-name
                                    :xml-node xml-node
                                    :to to
                                    :from from
                                    :stanza-type stanza-type)
                     dispatchers)))
                                  
  (xml-to-stanza ((stanza) dispatchers)
    (let* ((xml-node      (xml-node stanza))
           (stanza-type (dom:get-attribute (dom:first-child xml-node) "type")))
      (string-case stanza-type
        ("subscribe" (make-stanza stanza 'presence-subscribe-stanza dispatchers))
        ("error"     (make-stanza stanza 'presence-error-stanza dispatchers))
        (:default
            (let ((show (get-element-by-name (dom:first-child (xml-node stanza)) "show")))
              (if show
                  (make-stanza stanza 'presence-show-stanza dispatchers)
                  (dispatch-stanza stanza 'presence-stanza dispatchers))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza presence-show-stanza (presence-stanza)
    ((show ""))

  (xml-to-stanza ((stanza) dispatchers)
    (let* ((xml-node  (xml-node stanza))
           (show      (get-element-data (get-element-by-name (dom:first-child xml-node) "show"))))
      (setf (show stanza) show)
      stanza))

  (stanza-to-xml ((stanza))
    (with-presence-stanza stanza
      (cxml:with-element "show"
        (cxml:text (show stanza))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza presence-subscribe-stanza (presence-stanza)
    ((status ""))

  (xml-to-stanza ((stanza) dispatchers)
    (let* ((xml-node (xml-node stanza))
           (status   (get-element-data
                      (get-element-by-name (dom:first-child xml-node) "status"))))
      (setf (status stanza) status)
      stanza))

  (stanza-to-xml ((stanza))
    (with-presence-stanza stanza
      (cxml:with-element "status"
        (cxml:text (status stanza))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza presence-error-stanza (presence-stanza)
    ()

  (xml-to-stanza ((stanza) dispatchers)
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;
;; IQ stanzas
;;

(defstanza iq-stanza (stanza)
    (id (stanza-type "get") to from)

  (:macro with-iq-stanza (iq-stanza &body body)
    `(cxml:with-element "iq"
       (cxml:attribute "id" (id ,iq-stanza))
       (unless (null (to ,iq-stanza))
         (cxml:attribute "to" (to ,iq-stanza)))
       (unless (null (from ,iq-stanza))
         (cxml:attribute "from" (from ,iq-stanza)))
       ,@body))

  (make-stanza ((stanza) class-name dispatchers)
    (let* ((xml-node (xml-node stanza))
           (iq-node (dom:first-child xml-node)))
      (xml-to-stanza (make-instance class-name
                                    :xml-node xml-node
                                    :to       (dom:get-attribute iq-node "to")
                                    :from     (dom:get-attribute iq-node "from")
                                    :id       (dom:get-attribute iq-node "id")
                                    :stanza-type  (dom:get-attribute iq-node "type"))
                     dispatchers)))

  (xml-to-stanza ((stanza) dispatchers)
    (let* ((xml-node (xml-node stanza))
           (stanza-type  (dom:get-attribute (dom:first-child xml-node) "type")))
      (string-case stanza-type
        ("result" (make-stanza stanza 'iq-result-stanza dispatchers))
        ("error"  (make-stanza stanza 'iq-error-stanza dispatchers))
        ("get"    (make-stanza stanza 'iq-get-stanza dispatchers))
        (:default (dispatch-stanza stanza 'iq-stanza dispatchers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza iq-get-stanza (iq-stanza)
    ()

  (xml-to-stanza ((stanza) dispatchers)
    ;; IQ's type is set to "get" by default           
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-set-stanza (iq-stanza)
    ()

  (:macro with-iq-set-stanza (iq-set-stanza &body body)
    `(with-iq-stanza ,iq-set-stanza
       (cxml:attribute "type" "set")
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza iq-set-bind-stanza (iq-set-stanza)
    (resource (xmlns "urn:ietf:params:xml:ns:xmpp-bind"))

  (stanza-to-xml ((stanza))
    (with-iq-set-stanza stanza
      (cxml:with-element "bind"
        (cxml:attribute "xmlns" (xmlns stanza))
        (unless (null (resource stanza))
          (cxml:with-element "resource"
            (cxml:text (resource stanza))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-set-session-stanza (iq-set-stanza)
    ((xmlns "urn:ietf:params:xml:ns:xmpp-session"))

  (stanza-to-xml ((stanza))
    (with-iq-set-stanza stanza
      (cxml:with-element "session"
        (cxml:attribute "xmlns" (xmlns stanza))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-result-stanza (iq-stanza)
    ()
  
  (xml-to-stanza ((stanza) dispatchers)
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-error-stanza (iq-stanza)
    ()

  (xml-to-stanza ((stanza) dispatchers)
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza failure-element (stanza)
    ((xmlns ""))

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "xmlns: ~A" (xmlns obj))))

  (xml-to-stanza ((stanza) dispatchers)
    (setf (xmlns stanza) (dom:get-attribute (dom:first-child (xml-node stanza)) "xmlns"))
    stanza))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         

(defstanza success-element (meta-element)
    ((xmlns ""))

  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xmlns) stanza
      (setf xmlns (dom:get-attribute (dom:first-child (xml-node stanza)) "xmlns"))
    stanza))

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "xmlns: ~A" (xmlns obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
