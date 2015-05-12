;;;; stanzas-test.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-to-xml-node (str)
  (cxml:parse str (cxml-dom:make-dom-builder)))

(defun string-to-stanza (stanza-class str)
  (make-instance stanza-class :xml-node (string-to-xml-node str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic suite
;;

(deftestsuite stanzas-test (cl-ngxmpp-test)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Suite for defstanza macros
;;

(deftestsuite stanzas-defstanza-test (stanzas-test)
  ())

(deftestsuite stanzas-defstanza-method%-test (stanzas-defstanza-test)
  ())

(addtest (stanzas-defstanza-method%-test)
  wrong-method-arguments-format
  (ensure-condition 'xmpp%:defstanza-method%-error
    (macroexpand-1 
     '(xmpp%::defstanza-method% stanza xml-to-stanza ((o (b d e) c) a) (body)))))

(addtest (stanzas-defstanza-method%-test)
  correct-auto-addition-of-stanza-class-name-to-arguments
  (let* ((args (third (macroexpand-1
                       '(xmpp%::defstanza-method% stanza
                         xml-to-stanza ((a (c d)) a)
                         (body)))))
         (arg-a (first args)))
    (ensure-same (second arg-a) 'stanza)))

(addtest (stanzas-defstanza-method%-test)
  simple-arg-is-still-simple
  (let* ((args (third (macroexpand-1
                       '(xmpp%::defstanza-method% stanza
                         xml-to-stanza ((a b) c) (body)))))
         (arg-c (third args)))
    (ensure-same arg-c 'c)))


(deftestsuite stanzas-defstanza-class%-test (stanzas-defstanza-test)
  ())

(addtest (stanzas-defstanza-class%-test)
  wrong-slots-format
  (ensure-condition 'xmpp%:defstanza-class%-error
    (macroexpand-1
     '(xmpp%::defstanza-class% stanza-error (stanza) (a (b c) (d e f))))))

(addtest (stanzas-defstanza-class%-test)
  slots-are-in-correct-form
  (ensure-no-warning
   (macroexpand-1
    '(xmpp%::defstanza-class% stanza () (a (b "initval"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utils
;;

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
  (ensure-null (xmpp%::get-element-by-name (dom:first-child xml-node) "bar3")))

(addtest (stanzas-xml-utils-test)
  get-element-data-does-not-exists
  (ensure-same "" (xmpp%::get-element-data
                   (xmpp%::get-element-by-name (dom:first-child xml-node) "bar"))))

(addtest (stanzas-xml-utils-test)
  get-element-data-equals
  (ensure-same "scary" (xmpp%::get-element-data
                        (xmpp%::get-element-by-name 
                         (xmpp%::get-element-by-name (dom:first-child xml-node) "baz")
                         "cat"))))
               
(addtest (stanzas-xml-utils-test)
  get-element-by-name-deep-search-fail
  (ensure-null (xmpp%::get-element-by-name (dom:first-child xml-node) "cat")))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-find-bar
  (ensure-same 2 (length (xmpp%::get-elements-by-name (dom:first-child xml-node) "bar"))))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-find-there-is-more-then
  (ensure (not (= 3 (length (xmpp%::get-elements-by-name (dom:first-child xml-node) "bar2"))))))

(addtest (stanzas-xml-utils-test)
  get-elements-by-name-nothing
  (ensure-null (xmpp%::get-elements-by-name (dom:first-child xml-node) "zero")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dispatch-stanza function
;;

(deftestsuite stanzas-dispatcher-test (stanzas-test)
  ((xeps-list '("multi-user-chat" "delayed-delivery"))
   (groupchat-stanza-delayed
    (string-to-stanza 'xmpp%::multi-user-chat-message-groupchat-stanza
                      "<message type='groupchat'><delay/><body>test</body></message>"))
   (groupchat-stanza
    (string-to-stanza 'xmpp%:message-stanza
                      "<message type='groupchat'><body>test</body></message>"))
   (message-stanza-delayed
    (string-to-stanza 'xmpp%:message-stanza
                      "<message><delay/><body>test</body></message>"))
   (unknown-stanza
    (string-to-stanza 'xmpp%:unknown-stanza "<unsupported-unknown/>")))
  (:setup    (xmpp%:use-xeps xeps-list))
  (:teardown (setf xmpp%::*stanzas-dispatchers* nil))
  :equality-test #'typep)

(addtest (stanzas-dispatcher-test)
  unknown-stanza
  (ensure-same (xmpp%::dispatch-stanza unknown-stanza 'xmpp%:stanza)
               'xmpp%:unknown-stanza
               :test #'typep))

(addtest (stanzas-dispatcher-test)
  correct-order-1
  (ensure-same (xmpp%::dispatch-stanza groupchat-stanza-delayed 'xmpp%::multi-user-chat-message-groupchat-stanza)
               'xmpp%::delayed-delivery-message-groupchat-stanza))

(addtest (stanzas-dispatcher-test)
  correct-order-2
  (ensure-same (xmpp%::dispatch-stanza groupchat-stanza 'xmpp%:message-stanza)
               'xmpp%::multi-user-chat-message-groupchat-stanza))

(addtest (stanzas-dispatcher-test)
  correct-order-3
  (ensure-same (xmpp%::dispatch-stanza message-stanza-delayed 'xmpp%:message-stanza)
               'xmpp%::delayed-delivery-message-stanza))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stanzas
;;

;;
;; TODO: come up with a better solution for testing stanzas
;;

(deftestsuite stream-stanza-stanzas-test (stanzas-test)
  ((features-stanza
    (string-to-stanza 'xmpp%::stream-features-stanza
                      "<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='3519827146' from='jabber.ru' version='1.0' xml:lang='ru'><stream:features><starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/><compression xmlns='http://jabber.org/features/compress'><method>zlib</method></compression><push xmlns='p1:push'/><rebind xmlns='p1:rebind'/><ack xmlns='p1:ack'/><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>DIGEST-MD5</mechanism><mechanism>SCRAM-SHA-1</mechanism></mechanisms><c xmlns='http://jabber.org/protocol/caps' hash='sha-1' node='http://www.process-one.net/en/ejabberd/' ver='S4v2n+rKmTsgLFog7BtVvK2o660='/><register xmlns='http://jabber.org/features/iq-register'/></stream:features></stream:stream>")))
  (:equality-test #'typep))
   
(addtest (stream-stanza-stanzas-test)
  correct-stream-stanza
  (ensure-same (xmpp%::xml-to-stanza features-stanza)
               'xmpp%::stream-stanza))

    

    
