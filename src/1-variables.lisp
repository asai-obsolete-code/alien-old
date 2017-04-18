;; decide the state encoding.

(in-package :alien)

(defvar *sas*)

(define-symbol-macro %variables% (sas-variables *sas*))

(defun sas-variable (n)
  "return the object representing sas variable"
  (aref %variables% n))

(defun sas-value (n m)
  "return the object representing sas atom"
  (aref (variable-values (aref %variables% n)) m))


