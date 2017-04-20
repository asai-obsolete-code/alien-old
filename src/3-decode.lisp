
(in-package :alien)



(defun map-paths (task states callback)
  "Iterate over the paths leading to the states stored in STATES.
Calls CALLBACK with a list of operators on the path."
  (match task
    ((task :state-schema ss
           :operator-schema os
           :operators operators
           :goal-op goal-op
           :axioms axioms)
     (do-ones (state-bv states)
       (funcall
        callback
        (iter (for op = (follow-diagram
                         (zdd-union operators goal-op)
                         (integer->zdd
                          (ash
                           (bitvector->integer
                            (subseq state-bv
                                    (schema-index ss :action)
                                    (schema-index ss :cost)))
                           (schema-index os :body)))))
              ;; (dump-break :follow op)
              ;; #+nil
              (break+ :bv (subseq state-bv
                                  (schema-index ss :action)
                                  (schema-index ss :cost))
                      op
                      (do-ones (op-bv op) (return op-bv)))
              (assert (= 1 (zdd-count-minterm op)))
              (for op-index = (bitvector->integer
                               (subseq (do-ones (op-bv op) (return op-bv))
                                       (schema-index os :index))))
              (for sas-op = (case op-index
                              (#b11111111111111111111111111111111 nil)
                              (#b11111111111111111111111111111110 (next-iteration))
                              (t (aref (sas-operators *sas*) op-index))))
              (break+ :sas-op)
              (while sas-op)
              (collect sas-op)
              (-<>> states
                (unapply-op op)
                (apply-axiom axioms)
                (setf states))))))))


