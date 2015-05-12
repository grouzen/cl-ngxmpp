;;;; stanzas.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros for receiving/sending stanzas.
;;

(defmacro with-stanza-output ((xml-stream) &body body)
  `(with-stream-xml-output (,xml-stream)
     (stanza-to-xml ,@body)))

(defmacro with-stanza-input ((xml-stream stanza-input) &body body)
  (let ((xml-input (gensym "xml-input")))
    `(with-stream-xml-input (,xml-stream ,xml-input)
       (let ((,stanza-input (xml-to-stanza (make-instance 'stanza :xml-node ,xml-input))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;
;; The 'protocol' for defining stanzas
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *stanzas-dispatchers* nil))

(defun dispatch-stanza (stanza super-stanza-class)
  (let ((dispatchers (getf *stanzas-dispatchers*
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
      (dispatch dispatchers))))
    
(defgeneric make-stanza (stanza class-name)
  (:documentation
   "This method makes new instance of `class-name' stanza,
fill it with the necessary fields taken from the parent,
and calls `xml-to-stanza' with this new instance. This method
needs to be implemented only for parental classes"))

(defgeneric xml-to-stanza (stanza)
  (:documentation
   "Method for serialization xml to one of the stanza's classes."))

(defgeneric stanza-to-xml (stanza)
  (:documentation
   "Returns xml object which is transformed further to string."))

(defgeneric handle-stanza (stanza)
  (:documentation
   "This handler must be overrided on client code."))

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

(defmacro defstanza (stanza-name superclasses slots &rest methods)
  `(progn
     (defstanza-class% ,stanza-name ,superclasses ,slots)
     (defstanza-methods% ,stanza-name ,methods)))

(defmacro defstanza-methods% (stanza-name methods)
  `(list ,@(mapcar #'(lambda (method)
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
                                         (error "Argument ~A is neither a non-NIL symbol nor a list of form '(obj class)" arg)
                                         arg))
                                    (t (list arg stanza-name))))
                          (first method-args)))
        (rest-args (cdr method-args)))
    `(defmethod ,method-name (,@obj-args ,@rest-args)
       ,@method-body)))

(defmacro defstanza-class% (stanza-name superclasses slots)
  (let ((slotz (mapcar #'(lambda (slot)
                           (let* ((ds (cond ((listp slot)
                                             (if (> (length slot) 2)
                                                 (error "Slot ~A is neither a non-NIL symbol nor a list of form '(name initform)" slot)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic stanza class.
;;
;; TODO: export children of stanza class from cl-ngxmpp package.
;;

(defstanza stanza ()
    (xml-node)
  (handle-stanza ((stanza))
    (error 'handle-stanza-error
           :format-control "Default stanza handler called. Please define handler for this type of stanza"))
 
  (make-stanza ((stanza) class-name)
    (xml-to-stanza (make-instance class-name :xml-node (xml-node stanza))))

  (xml-to-stanza ((stanza))
    (let ((qname (dom:node-name (dom:first-child (xml-node stanza)))))
      (string-case qname
        ("stream:stream"   (make-stanza stanza 'stream-stanza))
        ("message"         (make-stanza stanza 'message-stanza))
        ("failure"         (make-stanza stanza 'failure-stanza))
        ("success"         (make-stanza stanza 'success-stanza))
        ("proceed"         (make-stanza stanza 'proceed-stanza))
        ("challenge"       (make-stanza stanza 'sasl-challenge-stanza))
        ("iq"              (make-stanza stanza 'iq-stanza))
        ("presence"        (make-stanza stanza 'presence-stanza))
        (:default          (dispatch-stanza stanza 'stanza))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza unknown-stanza (stanza)
    ()
  
  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "Unknown type of stanza: ~A" (dom:node-name (dom:first-child (xml-node obj))))))

  (xml-to-stanza ((stanza))
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stream stanzas
;;

(defstanza stream-stanza (stanza)
    (id (to "") (from "") (xml-lang "en") (xmlns "jabber:client")
        (xmlns-stream "http://etherx.jabber.org/streams") (version "1.0"))
    
  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "to: ~A, from: ~A, id: ~A, xml-lang: ~A, xmlns: ~A, xmlns-stream: ~A, version: ~A"
              (to obj) (from obj) (id obj) (xml-lang obj) (xmlns obj) (xmlns-stream obj) (version obj))))


  (make-stanza ((stanza) class-name)
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
                                    :version      (dom:get-attribute stream-node "version")))))

  (xml-to-stanza ((stanza))
    (let* ((xml-node (xml-node stanza))
           (child (dom:first-child (dom:first-child xml-node)))
           (child-qname (dom:node-name child)))
      (string-case child-qname
        ("stream:features" (make-stanza stanza 'stream-features-stanza))
        ("stream:error"    (make-stanza stanza 'stream-error-stanza))
        (:default          (dispatch-stanza stanza 'stream-stanza)))))

  (stanza-to-xml ((stanza))
    (cxml:with-element "stream:stream"
      (cxml:attribute "to" (to stanza))
      (cxml:attribute "id" (id stanza))
      (cxml:attribute "xmlns" (xmlns stanza))
      (cxml:attribute "xmlns:stream" (xmlns-stream stanza))
      (cxml:attribute "version" (version stanza)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza stream-features-stanza (stream-stanza)
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

  (xml-to-stanza ((stanza))
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

(defstanza stream-close-stanza (stream-stanza)
    ()
  
  (xml-to-stanza ((stanza))
    stanza)

  (stanza-to-xml ((stanza))
    (cxml:with-element "stream:stream")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza stream-error-stanza (stream-stanza)
    (error-node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Message stanzas
;;

(defstanza message-stanza (stanza)
    (id from to thread message-type (body ""))

  (print-object ((obj) stream)
    (with-slots (from to body) obj
      (print-unreadable-object (obj stream :type t :identity t)
        (format stream "from: ~A, to: ~A, body: ~A"
                from to body))))

  (stanza-to-xml ((stanza))
    (with-message-stanza (stanza)))

  (make-stanza ((stanza) class-name)
    (let* ((message-node (dom:first-child (xml-node stanza)))
           (to           (dom:get-attribute message-node "to"))
           (from         (dom:get-attribute message-node "from"))
           (message-type (dom:get-attribute message-node "type"))
           (body         (get-element-data (get-element-by-name message-node "body"))))
      (xml-to-stanza (make-instance class-name
                                    :xml-node     (xml-node stanza)
                                    :message-type message-type
                                    :from         from
                                    :to           to
                                    :body         body))))

  (xml-to-stanza ((stanza))
    (let* ((xml-node     (xml-node stanza))
           (message-node (dom:first-child xml-node))
           (to           (dom:get-attribute message-node "to"))
           (from         (dom:get-attribute message-node "from"))
           (body         (get-element-data (get-element-by-name message-node "body")))
           (disp         (dispatch-stanza stanza 'message-stanza)))
      (if (typep disp 'unknown-stanza)
          (progn
            (setf (to stanza) to
                  (from stanza) from
                  (body stanza) body)
            stanza)
          disp)))
  
  (:macro with-message-stanza ((message-stanza) &body body)
    `(cxml:with-element "message"
       (unless (null (from ,message-stanza))
         (cxml:attribute "from" (from ,message-stanza)))
       (unless (null (to ,message-stanza))
         (cxml:attribute "to" (to ,message-stanza)))
       (unless (null (message-type ,message-stanza))
         (cxml:attribute "type" (message-type ,message-stanza)))
       (cxml:with-element "body"
         (cxml:text (body ,message-stanza)))
       ,@body)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza message-error-stanza (message-stanza)
    ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Presence stanzas
;;

(defstanza presence-stanza (stanza)
    (presence-type (id "") to from show status priority)

  (:macro with-presence-stanza ((presence-stanza) &body body)
    `(cxml:with-element "presence"
       (unless (null (to ,presence-stanza))
         (cxml:attribute "to"   (to ,presence-stanza)))
       (unless (null (from ,presence-stanza))
         (cxml:attribute "from" (from ,presence-stanza)))
       (unless (null (presence-type ,presence-stanza))
         (cxml:attribute "type" (presence-type ,presence-stanza)))
       ,@body))

  (stanza-to-xml ((stanza))
    (with-presence-stanza (stanza)))

  (make-stanza ((stanza) class-name)
    (let* ((xml-node      (xml-node stanza))
           (presence-node (dom:first-child xml-node))
           (presence-type (dom:get-attribute presence-node "type"))
           (to            (dom:get-attribute presence-node "to"))
           (from          (dom:get-attribute presence-node "from")))
      (xml-to-stanza (make-instance class-name
                                    :xml-node xml-node
                                    :to to
                                    :from from
                                    :presence-type presence-type))))
                                  
  (xml-to-stanza ((stanza))
    (let* ((xml-node      (xml-node stanza))
           (presence-type (dom:get-attribute (dom:first-child xml-node) "type")))
      (string-case presence-type
        ("subscribe" (make-stanza stanza 'presence-subscribe-stanza))
        ("error"     (make-stanza stanza 'presence-error-stanza))
        (:default
            (let ((show (get-element-by-name (dom:first-child (xml-node stanza)) "show")))
              (if show
                  (make-stanza stanza 'presence-show-stanza)
                  (dispatch-stanza stanza 'presence-stanza))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza presence-show-stanza (presence-stanza)
    ((show ""))

  (xml-to-stanza ((stanza))
    (let* ((xml-node  (xml-node stanza))
           (show      (get-element-data (get-element-by-name (dom:first-child xml-node) "show"))))
      (setf (show stanza) show)
      stanza))

  (stanza-to-xml ((stanza))
    (with-presence-stanza (stanza)
      (cxml:with-element "show"
        (cxml:text (show stanza))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza presence-subscribe-stanza (presence-stanza)
    ((status ""))

  (xml-to-stanza ((stanza))
    (let* ((xml-node (xml-node stanza))
           (status   (get-element-data
                      (get-element-by-name (dom:first-child xml-node) "status"))))
      (setf (status stanza) status)
      stanza))

  (stanza-to-xml ((stanza))
    (with-presence-stanza (stanza)
      (cxml:with-element "status"
        (cxml:text (status stanza))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza presence-error-stanza (presence-stanza)
    ()

  (xml-to-stanza ((stanza))
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;
;; IQ stanzas
;;

(defstanza iq-stanza (stanza)
    (id (iq-type "get") to from)

  (:macro with-iq-stanza ((iq-stanza) &body body)
    `(cxml:with-element "iq"
       (cxml:attribute "id" (id ,iq-stanza))
       (unless (null (to ,iq-stanza))
         (cxml:attribute "to" (to ,iq-stanza)))
       (unless (null (from ,iq-stanza))
         (cxml:attribute "from" (from ,iq-stanza)))
       ,@body))

  (make-stanza ((stanza) class-name)
    (let* ((xml-node (xml-node stanza))
           (iq-node (dom:first-child xml-node)))
      (xml-to-stanza (make-instance class-name
                                    :xml-node xml-node
                                    :to       (dom:get-attribute iq-node "to")
                                    :from     (dom:get-attribute iq-node "from")
                                    :id       (dom:get-attribute iq-node "id")
                                    :iq-type  (dom:get-attribute iq-node "type")))))

  (xml-to-stanza ((stanza))
    (let* ((xml-node (xml-node stanza))
           (iq-type  (dom:get-attribute (dom:first-child xml-node) "type")))
      (string-case iq-type
        ("result" (make-stanza stanza 'iq-result-stanza))
        ("error"  (make-stanza stanza 'iq-error-stanza))
        ("get"    (make-stanza stanza 'iq-get-stanza))
        (:default (dispatch-stanza stanza 'iq-stanza))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza iq-get-stanza (iq-stanza)
    ()

  (xml-to-stanza ((stanza))
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-set-stanza (iq-stanza)
    ()

  (:macro with-iq-set-stanza ((iq-set-stanza) &body body)
    `(with-iq-stanza (,iq-set-stanza)
       (cxml:attribute "type" "set")
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defstanza iq-set-bind-stanza (iq-set-stanza)
    (resource (xmlns "urn:ietf:params:xml:ns:xmpp-bind"))

  (stanza-to-xml ((stanza))
    (with-iq-set-stanza (stanza)
      (cxml:with-element "bind"
        (cxml:attribute "xmlns" (xmlns stanza))
        (unless (null (resource stanza))
          (cxml:with-element "resource"
            (cxml:text (resource stanza))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-set-session-stanza (iq-set-stanza)
    ((xmlns "urn:ietf:params:xml:ns:xmpp-session"))

  (stanza-to-xml ((stanza))
    (with-iq-set-stanza (stanza)
      (cxml:with-element "session"
        (cxml:attribute "xmlns" (xmlns stanza))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-result-stanza (iq-stanza)
    ()
  
  (xml-to-stanza ((stanza))
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza iq-error-stanza (iq-stanza)
    ()

  (xml-to-stanza ((stanza))
    stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza failure-stanza (stanza)
    ((xmlns ""))

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "xmlns: ~A" (xmlns obj))))

  (xml-to-stanza ((stanza))
    (setf (xmlns stanza) (dom:get-attribute (dom:first-child (xml-node stanza)) "xmlns"))
    stanza))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;         

(defstanza success-stanza (stanza)
    ((xmlns ""))

  (xml-to-stanza ((stanza))
    (with-slots (xmlns) stanza
      (setf xmlns (dom:get-attribute (dom:first-child (xml-node stanza)) "xmlns"))
    stanza))

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "xmlns: ~A" (xmlns obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
