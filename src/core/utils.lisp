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
  `(cond ,@(mapcar #'(lambda (case)
                       (if (eq (car case) :default)
                           (list 't (cadr case))
                           (list (list 'string= string (car case)) (cadr case))))
                   cases)))

(defun string-to-keyword (string)
  (multiple-value-bind (keyword-name keyword-status)
      (values (intern (string-upcase string) :keyword))
    (declare (ignore keyword-status))
    keyword-name))

(defun resolve-async-value (av)
  (cond ((not (bb:promisep av)) av)
        (t (let ((ret ""))
             (bb:alet ((v av))
               (setf ret v))
             ret))))

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
