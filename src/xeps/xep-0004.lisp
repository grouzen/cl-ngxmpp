;;;; xep-0004.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-xeps)

(define-xep (data-forms :order "0004"
                        :description "XEP 0004, Data Forms")

  ;;
  ;; Current XEP contains no 'stanzas', instead it contains regular xml-elements
  ;; with the same meaning as the `stream-element` "stanza" has.
  ;;
  ;; All elements presented here actually are building blocks for other "stanzas",
  ;; it means that they will never be a top-level element (child of "<stream:stream>").
  ;;
  ;; Also, in fact this code serializes/deserializes from/to XML and does nothing else
  ;; according to the given XML Schema -- http://xmpp.org/extensions/xep-0004.html#schema,
  ;; and I believe that the rest of XMPP also shouldn't contain additional logic, so my
  ;; idea is to generate such code using "XSD to S-exp" generator.
  ;;
  
  ((x-element (meta-element)
     ((xmlns "jabber:x:data")
      x-type
      title
      instructions
      fields
      reported
      items)

     (:methods
      ((xml-to-stanza ((stanza) dispatchers)
         (with-slots (xml-node) stanza
           (setf (x-type      stanza) (dom:get-attribute xml-node "type")
                 (title       stanza) (get-element-data (get-element-by-name xml-node "title"))
                 (intructions stanza) (mapcar #'get-element-data
                                              (get-elements-by-name xml-node "instructions"))
                 (fields      stanza) (mapcar #'(lambda (el)
                                                  (xml-to-stanza (make-instance 'data-forms-field-element
                                                                                :xml-node el)
                                                                 dispatchers))
                                              (get-elements-by-name xml-node "field"))
                 (reported    stanza) (let ((el get-element-by-name xml-node "reported"))
                                        (when el
                                          (xml-to-stanza (make-instance 'data-forms-reported-element
                                                                        :xml-node el)
                                                         dispatchers)))
                 (items       stanza) (mapcar #'(lambda (el)
                                                  (xml-to-stanza (make-instance 'data-forms-item-element
                                                                                :xml-node el)
                                                                 dispatchers))
                                              (get-elements-by-name xml-node "item")))
           stanza))

       (stanza-to-xml ((stanza))
         (with-slots (xmlns x-type title instructions fields reported items) stanza
           (cxml:with-element "x"
             (cxml:attribute "xmlns" xmlns)
             (cxml:attribute "type"  x-type)
             (mapcar #'(lambda (i) (cxml:with-element "instructions"
                                     (cxml:text i)))
                     instructions)
             (mapcar #'stanza-to-xml fields)
             (unless (null reported)
               (stanza-to-xml reported))
             (mapcar #'stanza-to-xml items)))))))
             

   (field-element (meta-element)
     (desc required field-values options label var element-type)

     (:methods
      ((xml-to-stanza ((stanza) dispatchers)
         (with-slots (xml-node) stanza
           (setf (desc     stanza) (get-element-data (get-element-by-name xml-node "desc"))
                 (required stanza) (not (nullp (get-element-by-name xml-node "required")))
                 (field-values   stanza) (mapcar #'get-element-data
                                           (get-elements-by-name xml-node "value"))
                 (options  stanza) (mapcar #'(lambda (el)
                                               (xml-to-stanza (make-instance 'data-forms-field-option-element :xml-node el)
                                                              dispatchers))
                                           (get-elements-by-name xml-node "option"))
                 (label    stanza) (dom:get-attribute xml-node "label")
                 (var      stanza) (dom:get-attribute xml-node "var")
                 (element-type stanza) (dom:get-attribute xml-node "type"))
           stanza))

       (stanza-to-xml ((stanza))
         (with-slots (desc required values options label var element-type) stanza
           (cxml:with-element "field"
             (unless (null label)
               (cxml:attribute "label" label))
             (unless (null var)
               (cxml:attribute "var" var))
             (unless (null element-type)
               (cxml:attribute "type" element-type))
             (unless (null desc)
               (cxml:with-element "desc"
                 (cxml:text desc)))
             (unless (null required)
               (cxml:with-element "required"))
             (mapcar #'(lambda (v) (cxml:with-element "value" (cxml:text v))) field-values)
             (mapcar #'(lambda (o) (cxml:with-element "option" (cxml:text o))) options)))))))

   (field-option-element (meta-element)
     (option-values label)

     (:methods
      ((xml-to-stanza ((stanza) dispatchers)
         (with-slots (xml-node) stanza
           (setf (option-values stanza) (mapcar #'get-element-data
                                                (get-elements-by-name xml-node "value"))
                 (label         stanza) (dom:get-attribute xml-node "label"))
           stanza))

       (stanza-to-xml ((stanza))
         (with-slots (option-values label) stanza
           (cxml:with-element "option"
             (cxml:attribute "label" label)
             (mapcar #'(lambda (v)
                         (cxml:with-element "value"
                           (cxml:text v)))
                     option-values)))))))

   (reported/item-element (meta-element)
     (fields)

     (:methods
      ((xml-to-stanza ((stanza) dispatchers)
         (with-slots (xml-node) stanza
           (setf (fields stanza)
                 (mapcar #'(lambda (f)
                             (xml-to-stanza (make-instance 'data-forms-field-element :xml-node f)
                                            dispatchers))
                         (get-elements-by-name xml-node "field")))
           stanza))

       (stanza-to-xml ((stanza))
         (with-slots (fields) stanza
           (cxml:with-element "reported"
             (mapcar #'(lambda (f) (stanza-to-xml f)) fields)))))))

   (item-element (reported/item-element) ())
   (reported-element (reported/item-element) ())))


