

(in-package :alien)

(cl-syntax:use-syntax :cl-interpol)
(defun fd-preprocess (domain problem)
  ($ "echo $PWD" :output t :error-output t)
  ($ #?"time ${*fd-home*}/fast-downward.py --translate ${domain} ${problem}" :output t :error-output t)
  (sas-parser:parse "output.sas")
  #+nil ($ #?"time ${*fd-home*}/fast-downward.py --preprocess output.sas" :output t :error-output t)
  #+nil ($ #?"mv output ${path}")
  #+nil (sas-parser:parse "output"))

