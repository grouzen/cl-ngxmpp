;;;; utils.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp)

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
         ,(intern (apply #'concatenate 'string (mapcar #'symbol-name (list 'create- `,class))))
         (,@func-args)
       (make-instance ',class ,@body-args))))

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
