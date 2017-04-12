
(in-package :alien)


(defun solve (problem &optional domain)
  (-> (fd-preprocess problem domain)
    (encode-sas-to-zdd)
    (run-search)))
