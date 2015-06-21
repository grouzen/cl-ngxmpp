;;;; suite.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(in-package #:cl-ngxmpp-test)

(deftestsuite cl-ngxmpp-test ()
  ())

(defparameter *test-print-test-case-names* t)

(defun run-all-tests ()
  "Run suite."
  (describe (run-tests :suite 'cl-ngxmpp-test)))
