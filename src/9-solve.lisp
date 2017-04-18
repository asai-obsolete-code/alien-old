
(in-package :alien)


(defun solve (problem &optional domain)
  (with-manager ()
    (-> (fd-preprocess problem domain)
      (encode-sas-to-zdd)
      (run-search))))

;; (solve (asdf:system-relative-pathname :alien "sets/misc-strips/movie/p001.pddl"))
