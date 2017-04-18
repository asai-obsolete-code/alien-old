
(in-package :alien)



(defun map-paths (task states callback)
  "Iterate over the paths leading to the states stored in STATES.
Calls CALLBACK with a list of operators on the path."
  (match task
    ((task :state-schema ss
           :operator-schema os
           :operators operators
           :axioms axioms)
     (do-ones (state-bv states)
       (funcall
        callback
        (iter (for op = (follow-diagram
                         operators
                         (ash
                          (bitvector->integer
                           (subseq state-bv
                                   (schema-index ss :action)
                                   (schema-index ss :cost)))
                          (schema-index os :body))))
              (assert (= 1 (zdd-count-minterm op)))
              (for op-index = (bitvector->integer
                               (subseq (do-ones (op-bv op) (return op-bv))
                                       (schema-index os :index))))
              (for sas-op = (if (= op-index #b11111111111111111111111111111111)
                                nil
                                (aref (sas-operators *sas*) op-index)))
              (while sas-op)
              (collect sas-op)
              (dump-break :follow op)
              (-<>> states
                (unapply-op op)
                (apply-axiom axioms)
                (setf states))))))))


