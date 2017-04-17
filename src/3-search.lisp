
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
       (iter (let ((goals (apply-op goal-op *states*)))
               (unless (node-equal (zdd-emptyset) goals)
                 (signal 'solution-found :states goals)))
             (-<>> *states*
               (apply-op operators)
               (apply-axiom axioms)
               (zdd-union *states*)
               (setf *states*)))))))

