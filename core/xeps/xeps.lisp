;;;; xeps.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

;; This is plist with format:
;;
;; '(:xep-name xep-class
;;    ...)
;;
(defvar *xeps-list* nil)

(defun get-xep (xep-name)
  (getf *xeps-list* (string-to-keyword xep-name)))

(defun xep-exists-p (xep-name)
  (get-xep xep-name))

;;
;; Attention!
;; These two function affect global state,
;; because they change *stanzas-dispatchers*.
;;
(defun use-xeps (names)
  (setf *stanzas-dispatchers* (build-stanzas-dispatchers% names nil)))

#+nil
(defun stop-use-xeps (names)
  (setf *stanzas-dispatchers*
        (remove-if #'(lambda (disp-name)
                       (member (string-downcase (symbol-name disp-name))
                               names
                               :test #'string=))
                   *stanzas-dispatchers*)))
  
(defun build-stanzas-dispatchers% (xeps-list dispatchers)
  (labels ((find-first-dep (deps-list xeps-list)
             (when deps-list
               (let ((dep (car deps-list)))
                 (if (member dep xeps-list :test #'equalp)
                     dep
                     (find-first-dep (cdr deps-list) xeps-list)))))
           (mapcddr (fn lyst)
             (when lyst
               (cons (funcall fn (car lyst) (cadr lyst))
                     (mapcddr fn (cddr lyst))))))
    (if (null xeps-list)
        dispatchers
        (let* ((xep                 (car xeps-list))
               (xep-obj             (get-xep xep))
               (xep-deps            (mapcar #'(lambda (d) (string-downcase (symbol-name d))) (depends-on xep-obj)))
               (xep-dispatchers     (dispatchers xep-obj))
               ;; TODO: find out more simple way
               (ret-dispatchers     (let* ((new-disps (reduce #'append
                                                              (mapcddr #'(lambda (k v)
                                                                           (list k (append (getf dispatchers k) v)))
                                                                       xep-dispatchers)))
                                           (intersect-disps (reduce #'append
                                                                    (mapcddr #'(lambda (k v)
                                                                                 (let ((member-of-new-p (getf new-disps k)))
                                                                                   (unless member-of-new-p
                                                                                     (list k v))))
                                                                             dispatchers))))
                                      (append intersect-disps new-disps)))
               (first-xep-dep       (find-first-dep xep-deps (cdr xeps-list)))
               (reordered-xeps-list (cons first-xep-dep (remove-if #'(lambda (x) (equalp x first-xep-dep)) xeps-list))))
          (if (or (null xep-deps) (null first-xep-dep))
              (build-stanzas-dispatchers% (cdr xeps-list) ret-dispatchers)
              (build-stanzas-dispatchers% reordered-xeps-list dispatchers))))))


(defclass xep ()
  ((name        :accessor name        :initarg :name        :initform nil)
   (order       :accessor order       :initarg :order       :initform nil)
   (author      :accessor author      :initarg :author      :initform "")
   (description :accessor description :initarg :description :initform "")
   (depends-on  :accessor depends-on  :initarg :depends-on  :initform nil) 
   (dispatchers :accessor dispatchers :initarg :dispatchers :initform nil)))

(defmethod print-object ((obj xep) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "name: ~A, order: ~A, author: ~A, description: ~A, depends-on: ~A"
            (name obj) (order obj) (author obj) (description obj) (depends-on obj))))

;;
;; (with-xep (muc)
;;   (push #'(lambda (s) t) (dispatchers muc)))
;;
(defmacro with-xep ((xep-name) &body body)
  (let ((xeps-list-keyword (string-to-keyword (symbol-name `,xep-name))))
    `(let ((,xep-name (getf *xeps-list* ,xeps-list-keyword)))
       ,@body)))
  

(defmacro make-stanza-with-xep (xep-name stanza-obj stanza-class)
  `(make-stanza ,stanza-obj (concat-symbols ',xep-name '- ,stanza-class)))

;;
;; (with-wrapper ((muc message-stanza) stanza)
;;   (cxml:with-elemnt "foo"
;;     ...))
;;  =>
;; (with-muc-message-stanza (stanza)
;;   (cxml:with-element "foo"
;;     ...))
;;
(defmacro with-wrapper (((xep-name stanza-name) stanza) &body body)
  (let ((wrapper-name (concat-symbols 'with '- `,xep-name '- `,stanza-name)))
    `(,wrapper-name (,stanza)
       ,@body)))

;;
;; XEP's declaration example:
;; 
;; (define-xep (muc :order "0045"
;;                  :author "John McCarty"
;;                  :description "Multi User Chat"
;;                  :depends-on (delayed-delivery))
;;
;;    ((message-stanza (message-stanza)
;;       ((to :initarg :to ...)
;;                 ...))
;;       (:methods ((stanza-to-xml (stanza)
;;                    (cxml:with-element "message"
;;                      (cxml:attribute "to" (to stanza))))
;;                   ...)
;;                  (xml-to-stanza (stanza)
;;                    ...)
;;                  (make-stanza (stanza class-name)
;;                    ...))
;;        :wrapper (((message-stanza) &body body)
;;          `(cxml:with-element "message"
;;              ...)))
;;        :dispathcher ((stanza)
;;         (unless (find-element a)
;;            nil)))
;;        
;;     (iq-set-message-stanza (iq-set-stanza) ())))
;;     ...
;;
;; TODO: verification and usefull errors messages.
(defmacro define-xep ((xep-name &key order author description depends-on) &body body)
    `(let* ((xep-name-string (string-downcase (symbol-name ',xep-name)))
            (xep-obj         (make-instance 'xep
                                         :name        xep-name-string
                                         :order       ,order
                                         :author      ,author
                                         :description ,description
                                         :depends-on  ',depends-on)))
       (setf (getf *xeps-list* (string-to-keyword xep-name-string)) xep-obj)
       ,@(mapcar #'(lambda (stanza-definition)
                     `(define-xep-stanza (,xep-name)
                        ,stanza-definition))
                 (car body))))

(defmacro define-xep-stanza ((xep-name) &body body)
  (let* ((stanza-repr   (first body))
         (stanza-name   (concat-symbols `,xep-name '- (car stanza-repr)))
         (super-classes (second stanza-repr))
         (slots         (third  stanza-repr))
         (helpers       (cadddr stanza-repr))
         (methods       (getf helpers :methods))
         (wrapper       (getf helpers :wrapper))
         (dispatcher    (getf helpers :dispatcher))
         (definitions   nil))
    ;; Stanza class definition
    (push `(defclass* ,stanza-name (,@super-classes) ,slots) definitions)
    ;; Methods for stanza: stanza-to-xml, xml-to-stanza, make-stanza
    (when methods
      (mapcar #'(lambda (method)
                  (let* ((method-name     (car method))
                         (method-args     (second method))
                         (method-obj-arg  (list (first method-args) stanza-name))
                         (method-rest-arg (cdr method-args))
                         (method-body     (third method)))
                    (push `(defmethod ,method-name ((,@method-obj-arg) ,@method-rest-arg) ,method-body) definitions)))
              methods))
    ;; Optional one macro for stanza: with-<stanza>
    (when wrapper
      (let* ((macro-name     (concat-symbols 'with '- `,stanza-name))
             (macro-args     (car wrapper))
             (macro-obj-arg  (car macro-args))
             (macro-body-arg (cdr macro-args))
             (macro-body     (cdr wrapper)))
        (push 
         `(defmacro ,macro-name ((,@macro-obj-arg) ,@macro-body-arg) ,@macro-body)
          definitions)))
    ;;
    ;; Dispatcher
    ;;
    ;; Here we're pushing dispatcher into a structure like:
    ;; (dispatchers <xep>) => (:super-class-stanza-name ((stanza-name #'lambda) ...) ...)
    ;; 
    (when dispatcher
      (let ((dispatcher-arg  (car dispatcher))
            (dispatcher-body (cdr dispatcher)))
        (push
         `(with-xep (,xep-name)
            (loop :for super-class :in '(,@super-classes)
               :do (push (list ',stanza-name #'(lambda (,@dispatcher-arg) ,@dispatcher-body))
                         (getf (dispatchers ,xep-name) (string-to-keyword (symbol-name super-class))))))
         definitions)))
    (setf definitions (reverse definitions))
    `(progn ,@definitions)))
