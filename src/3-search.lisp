
(in-package :alien)

(defvar *task*)
(defvar *states* nil
  "A special variable holding the closed/open list dd.")

(defun run-search (*task*)
  "BFS"
  (match *task*
    ((task init-op goal-op operators axioms
           :state-schema *state-schema*
           :operator-schema *operator-schema*)
     (let ((*states* (zdd-emptyset)))
       (setf *states* (apply-op init-op *states*))
       (iter (unless (node-equal (zdd-emptyset) (apply-op goal-op *states*))
               (signal 'solution-found :states *states*))
             (setf *states* (apply-op operators *states*))
             (setf *states* (apply-axiom axioms *states*)))))))

