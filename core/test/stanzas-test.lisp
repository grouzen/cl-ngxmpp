;;;; stanzas-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

(deftestsuite stanzas-test ()
  ())


(deftestsuite stanzas-xml-utils-test (stanzas-test)
  ((xml-node (dom:first-child
              (cxml:parse
               "<foo><bar2>dog</bar2><bar></bar><baz><cat>scary</cat></baz><bar><dog>cat</dog></bar></foo>"
               (cxml-dom:make-dom-builder))))))

(addtest (stanzas-xml-utils-test)
  get-element-by-name-does-not-exists
  (ensure-null (cl-ngxmpp::get-element-by-name xml-node "bar3")))

(addtest (stanzas-xml-utils-test)
  get-element-data-does-not-exists
  (ensure-same "" (cl-ngxmpp::get-element-data
                   (cl-ngxmpp::get-element-by-name xml-node "bar"))))

(addtest (stanzas-xml-utils-test)
  get-element-data-equals
  (ensure-same "scary" (cl-ngxmpp::get-element-data
                        (cl-ngxmpp::get-element-by-name 
                         (cl-ngxmpp::get-element-by-name xml-node "baz")
                         "cat"))))
               
(addtest (stanzas-xml-utils-test)
  get-element-by-name-deep-search-fail
  (ensure-null (cl-ngxmpp::get-element-by-name xml-node "cat")))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-find-bar
  (ensure-same 2 (length (cl-ngxmpp::get-elements-by-name xml-node "bar"))))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-find-there-is-more-then
  (ensure (not (= 3 (length (cl-ngxmpp::get-elements-by-name xml-node "bar2"))))))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-nothing
  (ensure-null (cl-ngxmpp::get-elements-by-name xml-node "zero")))


(describe (run-tests))
   
