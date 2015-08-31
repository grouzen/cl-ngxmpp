;;;; suite.lisp
;;;;
;;;; This file is part of the CL-NGXMPP library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Michael Nedokushev <michael.nedokushev@gmail.com>

(in-package #:cl-ngxmpp-client-test)

(deftestsuite cl-ngxmpp-client-test ()
  ())

(defparameter *test-print-test-case-names* t)

(defun run-all-tests ()
  "Run suite."
  (describe (run-tests :suite 'cl-ngxmpp-client-test)))
