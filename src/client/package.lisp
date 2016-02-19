;;;; package.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(defpackage #:cl-ngxmpp-client
  (:use #:cl)
  (:nicknames #:xmpp)
  (:export ;; Basics
           #:disconnect-client
           #:connect-client
           #:login-client
           #:connectedp
           #:loggedinp
           
           #:proceed-stanza
           #:proceed-stanza-loop
           #:receive-stanza
           #:send-stanza

           #:register-xeps
           
           ;; Deprecated
           #:send-message
           #:send-presence

           ;; Classes
           #:client

           
           ;; ;; High interface: client/high/
           ;; #:open-session
           ;; #:close-session
           
           ;; ;;Classes
           ;; #:session
           ;; #:domain
           
           ))

