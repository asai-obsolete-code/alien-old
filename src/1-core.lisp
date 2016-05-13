


(in-package :alien)

(defun run (open)
  (iter (for parent = (fetch open))
        (for child in (expand parent))
        (when (goal-p child)
          (return child))
        (send child open)))

(defgeneric fetch (open-list)
  (:generic-function-class inlined-generic-function))
(defgeneric send  (open-list)
  (:generic-function-class inlined-generic-function))
(defgeneric expand (node)
  (:generic-function-class inlined-generic-function))
(defgeneric goal-p (node)
  (:generic-function-class inlined-generic-function))
(defgeneric send  (open-list node)
  (:generic-function-class inlined-generic-function))

