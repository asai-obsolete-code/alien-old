
(in-package :alien)


(defun solve (problem &optional domain)
  (with-manager ()
    (let* ((*sas* (fd-preprocess problem domain))
           (*operator-schema* (operator-schema))
           (*state-schema* (state-schema)))
      (-> (encode-sas-to-zdd)
        (run-search)))))
