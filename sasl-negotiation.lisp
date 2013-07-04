
(in-package :cl-ngxmpp)

;;
;; Because cl-sasl library doesn't support SCRAM-SHA-1 and SCRAM-SHA-1-PLUS
;; mechanisms we are forced to support old and week PLAIN and DIGEST-MD5
;;
(defconstant +sasl-mandatory-mechanisms+
  '(:digest-md5 :plain))

