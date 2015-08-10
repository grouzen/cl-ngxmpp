;;;; package.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:cl-ngxmpp-client
  (:use #:cl)
  (:nicknames #:xmpp)
  (:export ;; Basics
           #:disconnect-client
           #:connect-client
           #:login-client
           
           #:proceed-stanza
           #:proceed-stanza-loop
           #:receive-stanza
           #:send-stanza

           ;; Xeps
           #:define-stanza-handler
           #:call-methods-with-xep
           #:use-xeps
           
           ;; Deprecated
           #:send-message
           #:send-presence

           ;; Classes
           #:client

           ;; High interface: client/high/
           #:open-session
           #:close-session
           
           ;;Classes
           #:session
           #:domain))

