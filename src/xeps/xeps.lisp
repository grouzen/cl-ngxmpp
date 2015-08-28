;;;; xeps.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-xeps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The List of (available) XEPs.
;;
;; Structure:
;;
;;   '(:xep-name xep-class ...)
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *xeps-list* nil))

;; Wrappers

(defun get-xep (xep-name)
  (getf *xeps-list* (string-to-keyword xep-name)))

(defun xep-available-p (xep-name)
  (get-xep xep-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Attention!
;; These two function affect global state,
;; because they change *stanzas-dispatchers*.
;;

(defun use-xeps (names)
  (let ((xeps-list (if (null names)
                       (loop :for (k v) :on *xeps-list* :by #'cddr
                          :collect (string-downcase (symbol-name k)))
                       names)))
    (setf *stanzas-dispatchers* (build-stanzas-dispatchers% xeps-list nil))))

;; WTF???
;; (defun stop-using-xeps (names)
;;   (setf *stanzas-dispatchers*
;;         (remove-if #'(lambda (disp-name)
;;                        (member (string-downcase (symbol-name disp-name))
;;                                names
;;                                :test #'string=))
;;                    *stanzas-dispatchers*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: comments
;;

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
               ;; TODO: find out a simpler way
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: comments
;;

(defclass xep ()
  ((name        :accessor name        :initarg :name        :initform nil)
   (order       :accessor order       :initarg :order       :initform nil)
   (description :accessor description :initarg :description :initform "")
   (depends-on  :accessor depends-on  :initarg :depends-on  :initform nil) 
   (dispatchers :accessor dispatchers :initarg :dispatchers :initform nil)))

(defmethod print-object ((obj xep) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "name: ~A, order: ~A, description: ~A, depends-on: ~A"
            (name obj) (order obj) (description obj) (depends-on obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DSL that helps you to define a entire XEP in terms of stanzas and methods over them.
;; You can find examples how to use it inside `src/core/xeps/` directory.
;;
;; TODO: verification and usefull errors messages.
;;

(defmacro define-xep ((xep-name &key order description depends-on) &body body)
    `(let* ((xep-name-string (string-downcase (symbol-name ',xep-name)))
            (xep-obj         (make-instance 'xep
                                         :name        xep-name-string
                                         :order       ,order
                                         :description ,description
                                         :depends-on  ',depends-on)))
       (setf (getf *xeps-list* (string-to-keyword xep-name-string)) xep-obj)
       ,@(mapcar #'(lambda (stanza-definition)
                     `(define-xep-stanza% (,xep-name)
                        ,stanza-definition))
                 (car body))))

(defmacro define-xep-stanza% ((xep-name) &body body)
  (let* ((stanza-repr   (first body))
         (stanza-name   (alexandria:symbolicate `,xep-name '- (car stanza-repr)))
         (super-classes (second stanza-repr))
         (slots         (third  stanza-repr))
         (helpers       (cadddr stanza-repr))
         (methods       (getf helpers :methods))
         ;;(wrapper       (getf helpers :wrapper))
         (dispatcher    (getf helpers :dispatcher))
         (definitions   nil))
    
    (push
     `(defstanza ,stanza-name (,@super-classes) ,slots ,@methods)
     definitions)
    
    ;; Dispatcher
    ;;
    ;; Here we're pushing dispatcher into a structure like:
    ;; (dispatchers <xep>) => (:super-class-stanza-name ((stanza-name #'lambda) ...) ...)
    ;;
    ;; TODO:
    ;;   use hashmap instead of plist
    (when dispatcher
      (let ((dispatcher-arg  (car dispatcher))
            (dispatcher-body (cdr dispatcher)))
        (push
         `(loop :for super-class :in '(,@super-classes)
             :do (push (list ',stanza-name #'(lambda (,@dispatcher-arg) ,@dispatcher-body))
                       (getf (dispatchers (getf *xeps-list* (string-to-keyword (symbol-name ',xep-name))))
                             (string-to-keyword (symbol-name super-class)))))
         definitions)))
    (setf definitions (reverse definitions))       
    `(progn ,@definitions)))

