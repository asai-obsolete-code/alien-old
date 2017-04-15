
(in-package :alien)

(defvar *task*)
(defvar *states* nil
  "A special variable holding the closed/open list dd.")

(defun run-search (*task*)
  "BFS"
  (match *task*
    ((task init-op goal-op operators axioms)
     (let ((*states* (zdd-emptyset)))
       (setf *states* (apply-op init-op *states*))
       (iter (unless (node-equal (zdd-emptyset) (apply-axiom goal-op *states*))
               (signal 'solution-found))
             (iter (for op in operators)
                   (setf *states* (apply-op op *states*)))
             (iter (for ax in axioms)
                   (setf *states* (apply-axiom ax *states*))))))))

