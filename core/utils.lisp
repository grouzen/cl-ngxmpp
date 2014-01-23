;;;; utils.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

(defmacro string-case (string &body cases)
  "I just didn't find a simple solution for case with strings,
so I wrote this ugly and I think very slow macro. If you know
a better way tell me, please."
  (let ((cond-cases (mapcar #'(lambda (case)
                                (if (eq (car case) :default)
                                    (list 't (cadr case))
                                    (list (list 'string= string (car case)) (cadr case))))
                            cases)))
    `(cond ,@cond-cases)))

(defun string-to-keyword (string)
  (multiple-value-bind (keyword-name keyword-status)
      (values (intern (string-upcase string) :keyword))
    (declare (ignore keyword-status))
    keyword-name))

(defun concat-symbols (&rest symbols)
  (multiple-value-bind (keyword-name keyword-status)
      (values (intern (apply #'concatenate 'string
                             (mapcar #'symbol-name symbols))))
    (declare (ignore keyword-status))
    keyword-name))

(defun future-value (future)
  (cond ((not (typep future 'cl-async-future::future)) future)
        ((cl-async-future::future-finished future)
         (first (cl-async-future::future-values future)))
        (t (cl-async-future:wait-for future
             (future-value future)))))

;;
;; For using condition with WITH-PROXY-ERROR macro,
;; it should be a successor of the PROXY-ERROR condition.
;;
;; In this example:
;;
;; (with-proxy-error connection-error
;;     (end-of-file)
;;   (print (read-char in)))
;;
;; CONNECTION-ERROR will be thrown if READ-CHAR will cause the END-OF-FILE,
;; and original instance of END-OF-FILE will be saved in the ORIGINAL-ERROR
;; slot of the CONNECTION-ERROR.
;;
(define-condition proxy-error (simple-condition)
  ((original-error :accessor original-error :initarg :original-error :initform nil)))

(defmacro with-proxy-error (error (&rest proxied-errors) &body form)
  (let ((expanded-cases
         (mapcar #'(lambda (c)
                     `(,c (e) (error (make-condition ',error
                                                     :original-error e
                                                     :format-control "Original error: ~A"
                                                     :format-arguments (list e)))))
                 proxied-errors)))
    `(handler-case
         (progn ,@form)
       ,@expanded-cases)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some garbage.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defmacro defcreate (class &body body)
  "This macro creates function `create-class' which is replacement
for make-instance, but with some features. It's necessary for creation of
objects outside of this package, so encapsulation isn't broken."
  (let ((func-args
         (append
          '(&key)
          (reduce #'append
                  (mapcar #'(lambda (arg)
                              (cdr arg))
                          (car body)))))
        (body-args
         (reduce #'(lambda (a b)
                     (flet ((simplify (arg)
                              (if (listp (cadr arg))
                                  (list (car arg) (caadr arg))
                                  arg)))
                       (let ((sa (simplify a))
                             (sb (simplify b)))
                         (append sa sb))))
                 (car body))))
    `(defun
         ,(concat-symbols 'create `,class)
         (,@func-args)
       (make-instance ',class ,@body-args))))

;;
;; TODO: add keys arguments for optional exporting of slots, class name, etc.
;;
;; Maybe it should be changed to similar function from defclass-star library.
;;
#+nil
(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  "Use this macro if you need export class name and slots from class implicitly."
  ;; export class name
  ;(export name *package*)
  ;; export slots
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses ,direct-slots ,@options)
     ,@(mapcar #'(lambda (slot)
                   `(export (find-symbol (symbol-name (car ,slot)) *package*)
                            (package-name *package*)))
               `,direct-slots)))

#+nil
(progn
  (defun mapcddr (fn lyst)
    (when lyst
      (cons (funcall fn (car lyst) (cadr lyst))
            (mapcddr fn (cddr lyst)))))

  (defun make-cartesian-set (a b)
    (reduce #'append
            (mapcar #'(lambda (x)
                        (mapcar #'(lambda (y)
                                    (cons x y)) b))
                    a)))

  (defun intersect (a b)
    (mapcar #'(lambda (x)
                (car x))
            (remove-if #'(lambda (x)
                           (not (eq (car x) (cdr x))))
                       (make-cartesian-set a b)))))
