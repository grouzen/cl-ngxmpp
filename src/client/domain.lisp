;;;; domain.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-client)

;; TODO:
;;
;; Make possibility to receive roster and presence information manually.
;;
;; Routing for incoming stanzas:
;;   - starting point for stanzas' routing could be a new facility
;;     (i.e. (domain-route-stanza (read-stanza *client*)))
;;   - each domain should describe what stanzas it can take.
;;   - ???

;; Global routing table.
(defparameter *domains-routes* nil)

(define-condition domain-error (simple-condition)
  ())

(defclass domain (session)
  ((domain-id
    :accessor domain-id
    :initform (xmpp%:string-to-keyword (symbol-name (gensym "domain"))))
   (mandatory-xep-deps :accessor mandatory-xep-deps :initform nil)))

(defmacro define-domain (domain-name &optional (lifetime :singleton)
                                       (&rest mandatory-xep-deps) &body slots)
  `(progn
     (defclass ,domain-name (domain) (,@slots))
     
     (defmethod initialize-instance :after ((domain ,domain-name) &key)
       (with-slots (mandatory-xep-deps) domain
         ;; Check if XEP is available for current session 
         (mapcar #'(lambda (dep)
                     (unless (member dep (xeps-list domain))
                       (error (make-condition 'domain-error
                                              :format-control "You should use one of specified xeps: ~A"
                                              :format-arguments (list mandatory-xep-deps)))))
                 mandatory-xep-deps)
         (setf (mandatory-xep-deps domain) mandatory-xep-deps)))

     ;; TODO: replace with macros which will take same arguments,
     ;;       and additional :routes argument.
     (defmethod ,@(alexandria:symbolicate 'make '- `,domain-name) ((session session))
       (with-slots (domains) session
         (when (eql ,lifetime :singleton)
           (getf (domains session) (string-to-keyword (symbol-name `,domain-name))))
         (let ((instance ((make-instance `,domain-name
                                         :username        (username        session)
                                         :server-hostname (server-hostname session)
                                         :xml-stream      (xml-stream      session)
                                         :xeps-list       (xeps-list       session)))))
           instance)))))

