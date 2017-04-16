;; decide the state encoding.

(in-package :alien)

;; (defvar *sas*)
;; (defparameter *sas* (fd-preprocess #p"sets/aaai16-opt/blocks/p01.pddl"))
(defparameter *sas* (fd-preprocess (asdf:system-relative-pathname :alien "sets/misc-adl/psr-middle/p01.pddl")))

(define-symbol-macro %variables% (sas-variables *sas*))

(defun sas-variable (n)
  "return the object representing sas variable"
  (aref %variables% n))

(defun sas-value (n m)
  "return the object representing sas atom"
  (aref (variable-values (aref %variables% n)) m))


