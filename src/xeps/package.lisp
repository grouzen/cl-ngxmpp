;;;; package.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(defpackage #:cl-ngxmpp-xeps
  (:use #:cl #:xmpp%)
  (:nicknames #:xmpp-xeps)
  (:export ;; Utils
           #:get-xep ;; TODO: consider removing this from the export list
           #:xep-available-p
           #:register-xeps))
