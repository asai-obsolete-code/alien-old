

(defvar *sas*)

(define-symbol-macro %variables% (sas-variables *sas*))
(defun variable (n)
  (aref %variables% n))
(defun value (n m)
  (aref (aref %variables% n) m))

(defvar *states*)
(defvar *operators*)


