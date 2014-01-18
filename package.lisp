;;;; package.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp
  (:use #:cl)
  (:export #:*default-hostname*
           #:*default-port*
           ;; Methods
           #:defcreate
           #:string-to-keyword
           #:negotiate-tls
           #:negotiate-sasl
           #:connectedp
           #:open-connection
           #:close-connection
           #:open-stream
           #:close-stream
           #:create-stream
           #:with-stanza-input
           #:with-stanza-output
           #:openedp
           #:closedp
           #:handle-stanza
           #:xep-exists-p
           #:concat-symbols
           #:use-xeps
           #:stanza-reader-read-stream           
           ;; Classes
           #:connection
           #:xml-stream
           #:stanza-reader
           #:stanza-reader-header
           #:stanza-reader-features
           #:adapter
           #:usocket-adapter
           #:iolib-adapter
           ;; Stanzas
           #:stanza
           #:message-stanza
           #:iq-result-stanza
           #:iq-get-stanza
           #:iq-set-stanza
           #:iq-result-stanza
           #:iq-set-bind-stanza
           #:iq-set-session-stanza
           #:presence-stanza
           #:presence-subscribe-stanza
           #:presence-show-stanza
           ;; Conditions
           #:handle-stanza-condition
           #:negotiate-sasl-condition))

(defpackage #:cl-ngxmpp-client
  (:use #:cl)
  (:export #:disconnect
           #:authorize
           #:send-message
           #:send-presence
           #:proceed-stanza
           #:proceed-stanza-loop
           #:read-stanza
           #:connect
           #:define-stanza-handler
           #:call-methods-with-xep
           #:use-xeps
           ;; Classes
           #:client))
