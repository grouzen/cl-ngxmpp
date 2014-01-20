;;;; stanzas-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

(defun string-to-xml-node (str)
  (cxml:parse str (cxml-dom:make-dom-builder)))

(defun string-to-stanza (stanza-class str)
  (make-instance stanza-class :xml-node (string-to-xml-node str)))

(deftestsuite stanzas-test (cl-ngxmpp-test)
  ())


(deftestsuite stanzas-xml-utils-test (stanzas-test)
  ((xml-node (string-to-xml-node
              "<foo>
                 <bar2>dog</bar2>
                 <bar></bar>
                 <baz>
                   <cat>scary</cat>
                 </baz>
                 <bar>
                   <dog>cat</dog>
                 </bar>
               </foo>"))))

(addtest (stanzas-xml-utils-test)
  get-element-by-name-does-not-exists
  (ensure-null (cl-ngxmpp::get-element-by-name (dom:first-child xml-node) "bar3")))

(addtest (stanzas-xml-utils-test)
  get-element-data-does-not-exists
  (ensure-same "" (cl-ngxmpp::get-element-data
                   (cl-ngxmpp::get-element-by-name (dom:first-child xml-node) "bar"))))

(addtest (stanzas-xml-utils-test)
  get-element-data-equals
  (ensure-same "scary" (cl-ngxmpp::get-element-data
                        (cl-ngxmpp::get-element-by-name 
                         (cl-ngxmpp::get-element-by-name (dom:first-child xml-node) "baz")
                         "cat"))))
               
(addtest (stanzas-xml-utils-test)
  get-element-by-name-deep-search-fail
  (ensure-null (cl-ngxmpp::get-element-by-name (dom:first-child xml-node) "cat")))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-find-bar
  (ensure-same 2 (length (cl-ngxmpp::get-elements-by-name (dom:first-child xml-node) "bar"))))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-find-there-is-more-then
  (ensure (not (= 3 (length (cl-ngxmpp::get-elements-by-name (dom:first-child xml-node) "bar2"))))))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-nothing
  (ensure-null (cl-ngxmpp::get-elements-by-name (dom:first-child xml-node) "zero")))


(deftestsuite stanzas-dispatcher-test (stanzas-test)
  ((xeps-list '("multi-user-chat" "delayed-delivery"))
   (groupchat-stanza-delayed
    (string-to-stanza 'cl-ngxmpp::multi-user-chat-message-groupchat-stanza
                      "<message type='groupchat'><delay/><body>test</body></message>"))
   (groupchat-stanza
    (string-to-stanza 'cl-ngxmpp:message-stanza
                      "<message type='groupchat'><body>test</body></message>"))
   (message-stanza-delayed
    (string-to-stanza 'cl-ngxmpp:message-stanza
                      "<message><delay/><body>test</body></message>"))
   (unknown-stanza
    (string-to-stanza 'cl-ngxmpp:unknown-stanza "<unsupported-unknown/>")))
  (:setup    (cl-ngxmpp:use-xeps xeps-list))
  (:teardown (setf cl-ngxmpp::*stanzas-dispatchers* nil))
  :equality-test #'typep)

(addtest (stanzas-dispatcher-test)
  unknown-stanza
  (ensure-same (cl-ngxmpp::dispatch-stanza unknown-stanza 'cl-ngxmpp:stanza)
               'cl-ngxmpp:unknown-stanza
               :test #'typep))

(addtest (stanzas-dispatcher-test)
  correct-order-1
  (ensure-same (cl-ngxmpp::dispatch-stanza groupchat-stanza-delayed 'cl-ngxmpp::multi-user-chat-message-groupchat-stanza)
               'cl-ngxmpp::delayed-delivery-message-groupchat-stanza))

(addtest (stanzas-dispatcher-test)
  correct-order-2
  (ensure-same (cl-ngxmpp::dispatch-stanza groupchat-stanza 'cl-ngxmpp:message-stanza)
               'cl-ngxmpp::multi-user-chat-message-groupchat-stanza))

(addtest (stanzas-dispatcher-test)
  correct-order-3
  (ensure-same (cl-ngxmpp::dispatch-stanza message-stanza-delayed 'cl-ngxmpp:message-stanza)
               'cl-ngxmpp::delayed-delivery-message-stanza))
