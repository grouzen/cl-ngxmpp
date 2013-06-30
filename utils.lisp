
(in-package #:cl-ngxmpp)

(defmacro defcreate (class &body body)
  "This macro creates function `create-class' which is replacement
for make-instance, but with some features. It's necessary for creation of
objects outside of this package, so encapsulation isn't broken."
  (let ((func-args
         (append '(&key) (reduce #'append (mapcar #'(lambda (arg) (cdr arg)) (car body)))))
        (body-args (reduce #'append (car body))))
  `(defun ,(intern (apply #'concatenate 'string (mapcar #'symbol-name (list 'create- `,class))))
       (,@func-args)
     (make-instance ',class ,@body-args))))

