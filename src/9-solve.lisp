
(in-package :alien)


(defun solve (problem &optional domain)
  (with-manager ()
    (-> (fd-preprocess problem domain)
      (encode-sas-to-zdd)
      (run-search-single))))


;; (defparameter *sas* (fd-preprocess #p"sets/aaai16-opt/blocks/p01.pddl"))
(setf *sas* (fd-preprocess (asdf:system-relative-pathname :alien "sets/misc-adl/psr-middle/p01.pddl")))
(setf *operator-schema* (operator-schema))
(setf *state-schema* (state-schema))
(defparameter *movie* (asdf:system-relative-pathname :alien "sets/misc-strips/movie/p001.pddl"))
(defparameter *movie-sas* (fd-preprocess *movie*))
(defparameter *movie-task* (encode-sas-to-zdd *movie-sas*))
(nop
  
  (solve ))
