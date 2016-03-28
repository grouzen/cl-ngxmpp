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
                             (make-instance 'meta-element :xml-node ,xml-input)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The 'protocol' to define stanzas
;;

;; TODO: come up with a better name for this
(defun get-stanza-xml-string (stanza)
  (babel:octets-to-string
   (cxml:with-xml-output (cxml:make-octet-vector-sink :canonical 1)
     (stanza-to-xml stanza))))

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
  (let* ((slotz
          (mapcar #'(lambda (slot)
                      (let ((ds (cond ((listp slot)
                                       (if (> (length slot) 2)
                                           (error 'defstanza-class%-error :slot slot)
                                           slot))
                                      (t (list slot nil)))))
                        (list (first ds) (second ds))))
                  slots))
         (slotz-make-stanza
          (reduce #'append
                  (mapcar #'(lambda (slot)
                              (let* ((slot-name (first slot))
                                     (initarg   (alexandria:make-keyword slot-name))
                                     (initval   (list slot-name 'stanza)))
                                (list initarg initval)))
                          slotz))))
    `(progn
       (defstanza-class% ,stanza-name ,superclasses ,slotz)
       (defstanza-methods% ,stanza-name ,methods)
       ;; Generate make-stanza method automatically
       (defstanza-method% ,stanza-name make-stanza ((stanza) class-name dispatchers)
         ;; I'm forced here by the lisp compiler to compute slots of superclasses in
         ;; run-time each time when the `make-stanza` method is called. I would be
         ;; glad to know a better solution.
         (let* ((superclasses-slotz
                 (reduce #'append
                         (mapcar
                          #'(lambda (superclass)
                              (reduce #'append
                                      (mapcar #'(lambda (slot-def)
                                                  (let* ((slot-name (cl-mop:slot-definition-name slot-def))
                                                         (initarg   (alexandria:make-keyword slot-name))
                                                         ;; TODO:
                                                         ;;   It has to be changed to be able to call accessors from
                                                         ;;   the xmpp-xeps package too.
                                                         (initval   (funcall (intern (symbol-name slot-name) :xmpp%) stanza)))
                                                    (list initarg initval)))
                                              (cl-mop:class-slots (find-class superclass)))))
                          ',superclasses)))
                (make-instance-args (append (list class-name) superclasses-slotz (list ,@slotz-make-stanza))))
           (xml-to-stanza (apply #'make-instance make-instance-args) dispatchers))))))

(defmacro defstanza-class% (stanza-name superclasses slots)
  (let ((slotz (mapcar #'(lambda (slot)
                           (let ((name     (first slot))
                                 (initform (second slot)))
                             (list name
                                   :accessor name
                                   :initarg  (alexandria:make-keyword name)
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
;; Very meta-level stuff.
;;
;; The hierarchy:
;;
;;   ,-----> id element----->stanza              +----------------------+
;;   |          |              |                 |                      |
;;   |          V              |--->message----->|                      |
;;   |       stream            |    stanza       |                      |
;;   |       element           |                 |                      |
;;   |                         |--->iq---------->|                      |
;;   |                         |    stanza       |          XEPs        |
;;   |                         |                 |        stanzas       |
;;   |                         `--->presence---->|          and         |
;;   |                              stanza       |        elements      |
;;   |                                           |                      |
;; meta                                          |                      |
;; element-------------------------------------->|                      |
;;   |                                           |                      |
;;   |                                           +----------------------+
;;   |-----> sasl-challenge
;;   |       element
;;   |
;;   |-----> proceed
;;   |       element
;;   |
;;   |-----> success
;;   |       element
;;   |
;;   `-----> failure
;;           element
;;           |    |
;;        .--/    |
;;        |       |
;;        V       V
;;       tls     sasl
;;      xmlns    xmlns
;;
(defstanza meta-element ()
    (xml-node (xmlns ""))

  (:macro with-meta-element (stanza &body body)
    (`(cxml:attribute "xmlns" (xmlns ,stanza))

      `(progn 
         (with-element "message"
           (cxml:attribute "to" to)
           (cxml:with-element "body"
             (cxml:text (body ,message-stanza))))
         (with-meta-element ,message-stanza)
         
  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xml-node) stanza
      (let* ((root-node (dom:first-child xml-node))
             (qname     (dom:node-name root-node))
             (xmlns     (dom:get-attribute root-node "xmlns")))
        (setf (xmlns stanza) xmlns)
        (string-case qname
          ("failure"       (make-stanza stanza 'failure-element dispatchers))
          ("success"       (make-stanza stanza 'success-element dispatchers))
          ("proceed"       (make-stanza stanza 'proceed-element dispatchers))
          ("challenge"     (make-stanza stanza 'sasl-challenge-element dispatchers))
          (:default        (make-stanza stanza 'id-element dispatchers)))))))

(defstanza id-element (meta-element)
    ((id (uuid:make-v4-uuid)))

  (:macro with-id-element (stanza &body body)
   `(progn
      ,@body
      (cxml:attribute "id" (id stanza))))
  
  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xml-node) stanza
      (let* ((root-node (dom:first-child xml-node))
             (qname     (dom:node-name root-node))
             (id        (dom:get-attribute root-node "id")))
        (setf (id stanza) id)
        (string-case qname
          ("stream:stream" (make-stanza stanza 'stream-element dispatchers))
          (:default        (make-stanza stanza 'stanza dispatchers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic stanza class.
;;
;; TODO:
;;   - export children of stanza class from cl-ngxmpp package.
;;

(defstanza stanza (id-element)
    ;; According to the definition of "stanza" -- an each type of stanza should have
    ;; at least 4 attributes in its root element: id, to, from, type
    (id to from stanza-type)

  (:macro with-stanza (stanza &body body)
    `(with-id-element (,stanza)
       (unless (null (from ,stanza))
         (cxml:attribute "from" (from ,stanza)))
       (unless (null (to ,stanza))
         (cxml:attribute "to" (to ,stanza)))
       (unless (null (stanza-type ,stanza))
         (cxml:attribute "type" (stanza-type ,stanza))))

  (handle-stanza ((stanza)) t)

  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xml-node) stanza
      (let* ((root-node (dom:first-child xml-node))
             (qname     (dom:node-name root-node))
             (id        (dom:get-attribute root-node "id"))
             (to        (dom:get-attribute root-node "to"))
             (from      (dom:get-attribute root-node "from"))
             (stype     (dom:get-attribute root-node "type")))
        (setf (id          stanza) id
              (to          stanza) to
              (from        stanza) from
              (stanza-type stanza) stype)
        (string-case qname
          ("message"         (make-stanza stanza 'message-stanza dispatchers))
          ("iq"              (make-stanza stanza 'iq-stanza dispatchers))
          ("presence"        (make-stanza stanza 'presence-stanza dispatchers))
          (:default          (dispatch-stanza stanza 'stanza dispatchers)))))))

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

(defstanza stream-element (id-element)
    ((xml-lang "en") (xmlns "jabber:client")
     (xmlns-stream "http://etherx.jabber.org/streams") (version "1.0"))

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "to: ~A, from: ~A, id: ~A, xml-lang: ~A, xmlns: ~A, xmlns-stream: ~A, version: ~A"
              (to obj) (from obj) (id obj) (xml-lang obj) (xmlns obj) (xmlns-stream obj) (version obj))))

  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xml-node) stanza
      (let* ((root-node    (dom:first-child xml-node))
             (child-qname  (dom:node-name (dom:first-child root-node)))
             (xml-lang     (dom:get-attribute root-node "xml:lang"))
             (xmlns-stream (dom:get-attribute root-node "xmlns:stream"))
             (version      (dom:get-attribute root-node "version")))
        (setf (xml-lang     stanza) xml-lang
              (xmlns-stream stanza) xmlns-stream
              (version      stanza) version)
        (string-case child-qname
          ("stream:features" (make-stanza stanza 'stream-features-element dispatchers))
          ("stream:error"    (make-stanza stanza 'stream-error-element dispatchers))
          (:default          (dispatch-stanza stanza 'stream-element dispatchers))))))

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
    (with-slots (xml-node features) stanza
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
       (dom:child-nodes (dom:first-child (dom:first-child xml-node))))
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

  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xml-node) stanza
      (let* ((message-node (dom:first-child xml-node))
             (body         (get-element-data (get-element-by-name message-node "body")))
             (disp         (dispatch-stanza stanza 'message-stanza dispatchers)))
        (setf (body stanza) body)
        (if (typep disp 'unknown-stanza)
            stanza
            disp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza message-error-stanza (message-stanza)
    ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Presence stanzas
;;

(defstanza presence-stanza (stanza)
    ()

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

  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xml-node stanza-type) stanza
      (let ((show (get-element-by-name (dom:first-child xml-node) "show")))
        (setf (show xml-node) show)
        (string-case stanza-type
          ("subscribe" (make-stanza stanza 'presence-subscribe-stanza dispatchers))
          ("error"     (make-stanza stanza 'presence-error-stanza dispatchers))
          (:default
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
    ((stanza-type "get") to from)

  (:macro with-iq-stanza (iq-stanza &body body)
    `(cxml:with-element "iq"
       (cxml:attribute "id" (id ,iq-stanza))
       (unless (null (to ,iq-stanza))
         (cxml:attribute "to" (to ,iq-stanza)))
       (unless (null (from ,iq-stanza))
         (cxml:attribute "from" (from ,iq-stanza)))
       ,@body))

  ;; (make-stanza ((stanza) class-name dispatchers)
  ;;   (let* ((xml-node (xml-node stanza))
  ;;          (iq-node (dom:first-child xml-node)))
  ;;     (xml-to-stanza (make-instance class-name
  ;;                                   :xml-node xml-node
  ;;                                   :to       (dom:get-attribute iq-node "to")
  ;;                                   :from     (dom:get-attribute iq-node "from")
  ;;                                   :id       (dom:get-attribute iq-node "id")
  ;;                                   :stanza-type  (dom:get-attribute iq-node "type"))
  ;;                    dispatchers)))

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

(defstanza failure-element (meta-element)
    ()

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "xmlns: ~A" (xmlns obj))))

  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xml-node xmlns) stanza
      (let ((node-xmlns (dom:get-attribute (dom:first-child xml-node) "xmlns")))
        (string-case node-xmlns
          ("urn:ietf:params:xml:ns:xmpp-tls"  (make-stanza stanza 'failure-tls-element dispatchers))
          ("urn:ietf:params:xml:ns:xmpp-sasl" (make-stanza stanza 'failure-sasl-element dispatchers)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstanza success-element (meta-element)
    ()

  (xml-to-stanza ((stanza) dispatchers)
    (with-slots (xmlns) stanza
      (setf xmlns (dom:get-attribute (dom:first-child (xml-node stanza)) "xmlns"))
    stanza))

  (print-object ((obj) stream)
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "xmlns: ~A" (xmlns obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
