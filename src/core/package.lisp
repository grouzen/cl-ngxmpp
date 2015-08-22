;;;; package.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp
  (:use #:cl)
  (:nicknames #:xmpp%)
  (:export #:*default-hostname*
           #:*default-port*
           
           ;; Utils
           #:debuggable
           #:statefull

           #:chain-statefull
           #:string-to-keyword
           #:string-case
           
           ;; Methods
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
           #:tls-negotiatedp
           #:sasl-negotiatedp
           #:handle-stanza
           #:xep-available-p
           #:concat-symbols
           #:use-xeps
           #:stop-use-xeps
           #:stanza-reader-read-stream
           #:resolve-async-value
           #:print-debug
           #:get-stanza-xml-string
           
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
           #:unknown-stanza
           
           ;; Conditions
           #:handle-stanza-error
           #:negotiate-sasl-error
           #:stanza-reader-error
           #:connection-error
           #:defstanza-method%-error
           #:defstanza-class%-error))
