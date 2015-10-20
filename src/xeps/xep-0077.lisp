;;;; xep-0077.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-xeps)

(define-xep (in-band-registration :order "0077"
                                  :description "XEP 0077, In-Band Registration"
                                  :depends-on (data-forms))

  ((iq-get-registration-fields-stanza (iq-get-stanza)
     ()

     (:methods
      ((stanza-to-xml ((stanza))
         (with-iq-stanza stanza
           (cxml:with-element "query"
             (cxml:with-attribute "xmlns" "jabber:iq:register")))))))

   (iq-result-registration-fields-stanza (iq-result-stanza)
     (instructions username nick password name first last email address city
      state zip phone url date misc text key registered)             
       
     (:methods
      ((xml-to-stanza ((stanza))
         ;; according to http://xmpp.org/extensions/xep-0077.html#registrar-formtypes
         (let* ((instructions-node (get-element-by-name query-node "instructions"))
                (instructions      (when instructions-node
                                     (get-element-data instructions-node))))
           (make-instance 'iq-result-registration-fields-stanza
                          :instructions instructions
                          :username     (field-required-p stanza "username")
                          :password     (field-required-p stanza "password")
                          :name         (field-required-p stanza "name")
                          :first        (field-required-p stanza "first")
                          :last         (field-required-p stanza "last")
                          :email        (field-required-p stanza "email")
                          :address      (field-required-p stanza "address")
                          :city         (field-required-p stanza "city")
                          :state        (field-required-p stanza "state")
                          :zip          (field-required-p stanza "zip")
                          :phone        (field-required-p stanza "phone")
                          :url          (field-required-p stanza "url")
                          :date         (field-required-p stanza "date")
                 (mapcar #'(lambda (v)
                         (cxml:with-element "value"
                           (cxml:text v)))
                     option-values)))))))         :misc         (field-required-p stanza "misc")
                          :text         (field-required-p stanza "text")
                          :key          (field-required-p stanza "key"))))
       
       ;; helper
       (field-required-p ((stanza) field-name)
         (get-element-by-name (dom:first-child (xml-node stanza)) field-name))
