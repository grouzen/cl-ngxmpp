;;;; package.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp-client
  (:use #:cl)
  (:nicknames #:xmpp)
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
           #:client

           ;; High interface: client/high/
           #:open-session
           #:close-session
           #:create-session
           ;;Classes
           #:session
           #:domain))
