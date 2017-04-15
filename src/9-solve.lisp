
(in-package :alien)


(defun solve (problem &optional domain)
  (with-manager ()
    (let ((*sas* (fd-preprocess problem domain)))
      (-> (encode-sas-to-zdd)
        (run-search)))))
