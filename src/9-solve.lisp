
(in-package :alien)


(defun solve (problem &optional domain)
  (with-manager ()
    (-> (fd-preprocess problem domain)
      (encode-sas-to-zdd)
      (run-search))))
