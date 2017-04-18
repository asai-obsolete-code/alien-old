
(in-package :alien)

(defvar *task*)
(defvar *states* nil
  "A special variable holding the closed/open list dd.")

(defmacro break* (&body body)
  `(break (with-output-to-string (*standard-output*)
            ,@body)))

(defun dump-break (comment states)
  (break* (print comment) (dump states))
  states)

(defun run-search (*task*)
  "BFS"
  (match *task*
    ((task init-op goal-op operators axioms
           :state-schema *state-schema*
           :operator-schema *operator-schema*)
     (let ((*states* (zdd-set-of-emptyset)))
       #+nil
       (dump-break :zero *states*)
       (setf *states* (apply-op init-op *states*))
       #+nil
       (dump-break :init *states*)
       (iter (let ((goals (apply-op goal-op *states*)))
               (unless (node-equal (zdd-emptyset) goals)
                 (->> goals
                   (dump-break :goals))
                 (signal 'solution-found :states goals)))
             (-<>> *states*
               (apply-op operators)
               #+nil
               (dump-break :op)
               (apply-axiom axioms)
               #+nil
               (dump-break :axiom)
               (zdd-union *states*)
               #+nil
               (dump-break :union)
               (setf *states*)))))))

(defun run-search-single (task)
  (handler-case (run-search task)
    (solution-found (c)
      (match c
        ((solution-found states)
         (map-paths task states
                    (lambda (first-solution) (return-from run-search-single first-solution))))))))
