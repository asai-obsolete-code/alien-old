
(in-package :alien)

(defvar *task*)
(defvar *states* nil
  "A special variable holding the closed/open list dd.")

(defmacro break* (&body body)
  `(break (with-output-to-string (*standard-output*)
            ,@body)))

(defun dump-states (&optional (*states* *states*))
  (map-ones *states* #'print))

(defun dump-states-monad (comment states)
  (break* (print comment) (dump-states states))
  states)

(defun run-search (*task*)
  "BFS"
  (match *task*
    ((task init-op goal-op operators axioms
           :state-schema *state-schema*
           :operator-schema *operator-schema*)
     (let ((*states* (zdd-set-of-emptyset)))
       (break* (print :zero) (dump-states))
       (setf *states* (apply-op init-op *states*))
       (break* (print :init) (dump-states))
       (iter (let ((goals (apply-op goal-op *states*)))
               (unless (node-equal (zdd-emptyset) goals)
                 (signal 'solution-found :states goals)))
             (-<>> *states*
               (apply-op operators)
               (dump-states-monad :op)
               (apply-axiom axioms)
               (dump-states-monad :axiom)
               (zdd-union *states*)
               (dump-states-monad :union)
               (setf *states*)))))))

