;;;; xeps.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-client)

(defvar *xeps-methods* nil)

;; TODO: check if it's working
(defmacro call-methods-with-xep ((xep-name) &body methods-calls)
  (let ((real-methods
         (mapcar #'(lambda (method)
                     (let ((method-name (first method))
                           (method-args (cdr method)))
                       `(,(find-symbol
                           (symbol-name (cl-ngxmpp:concat-symbols `,xep-name '- `,method-name))
                           'cl-ngxmpp-client)
                          ,@method-args)))
                 (first methods-calls))))
    `(progn ,@real-methods)))

;;
;; (define-stanza-handler ((stanza message-stanza) :xep muc)
;;   (do something with data from server))
;; 
(defmacro define-stanza-handler (((stanza stanza-class) &key xep) &body body)
  (let ((namespace-stanza-class
         (find-symbol (symbol-name 
                       (if (eq `,xep nil)
                           `,stanza-class
                           (cl-ngxmpp:concat-symbols `,xep '- `,stanza-class)))
                      'cl-ngxmpp)))
    `(defmethod cl-ngxmpp:handle-stanza ((,stanza ,namespace-stanza-class))
       ,@body)))
         
(defmacro define-methods-with-xep ((xep-name) &body methods-definitions)
  ;; TODO: check if xep with xep-name exists.
  (let ((real-definitions
         (mapcar #'(lambda (method)
                     (let ((method-name   (first method))
                           (formal-params (second method))
                           (body          (third method)))
                       `(define-xep-method% ,xep-name (,method-name (,@formal-params))
                          ,body)))
                 (first methods-definitions))))
    `(progn ,@real-definitions)))

(defmacro define-xep-method% (xep-name (method-name (&rest formal-params)) &body body)
  (let ((xep-name-keyword (gensym "xep-name-keyword")))
    `(let* ((,xep-name-keyword (cl-ngxmpp:string-to-keyword (symbol-name ',xep-name)))
            (xep-methods (getf *xeps-methods* ,xep-name-keyword)))
       (unless xep-methods
         (append (list ,xep-name-keyword nil) *xeps-methods*))
       (push #'(lambda ()
                 (defmethod
                     ;; method name
                     ,(cl-ngxmpp:concat-symbols `,xep-name '- `,method-name)
                     ;; formal parameters
                     (,@formal-params)
                   ,@body))
             (getf *xeps-methods* ,xep-name-keyword)))))

(defmacro make-instance-with-xep ((xep-name) stanza-class &body body)
  (let ((namespace-stanza-class
         (find-symbol (symbol-name (cl-ngxmpp:concat-symbols `,xep-name '- (second stanza-class)))
                      'cl-ngxmpp)))
    `(make-instance ',namespace-stanza-class ,@body)))

(defun use-xeps (&rest names)
  (cl-ngxmpp:use-xeps names)
  (loop
     :for name :in names
     :do (let ((xep-methods (getf *xeps-methods* (cl-ngxmpp:string-to-keyword name))))
           ;; TODO: throw error that xep doesn't exist
           (when (and (cl-ngxmpp:xep-exists-p name) xep-methods)
             (loop
                :for method-closure :in xep-methods
                :do (funcall method-closure))))))
