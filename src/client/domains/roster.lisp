;;;; roster.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-client)

(define-domain :singleton roster ()
  ;; Each route will be added into a global routing table.
  ;; The table looks like:
  ;; (:presence-stanza
  ;;   (roster add-entry
  ;;          ;; implicit function for destructuring incoming stanza.
  ;;          #'(lambda (jid name description) (destruct-stanza-object-and-return-needed-fields))
  ;;          ;; user-defined function on which the main action takes the place.
  ;;          #'(lambda (jid name desription) action))
  ;;  :message-stanza
  ;;   (...))
  :routes ((add-entry    (s 'presence-subscribed-stanza (jid name subscription))
             (destruct-stanza-object-and-return-needed-fields))
           (delete-entry (s 'presence-unsubscribed-stanza (jid name subscription))
             (...)))
  :slots ((entries :accessor entries :initarg :entries :initform nil)))

(defclass roster-entry ()
  ((jid          :accessor jid          :initarg :jid          :initform "")
   (resource     :accessor resource     :initarg :resource     :initform "")
   (subscription :accessor subscription :initarg :subscription :initform "")
   (status       :accessor status       :initarg :status       :initform "online")))
