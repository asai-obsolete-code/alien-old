


(in-package :alien)

(defun run (storage)
  (iter (for parent = (fetch storage))
        (for child in (expand parent))
        (when (goal-p child)
          (return child))
        (send child storage)))

(defgeneric fetch (storage)
  (:generic-function-class inlined-generic-function))
(defgeneric send  (storage)
  (:generic-function-class inlined-generic-function))
(defgeneric expand (node)
  (:generic-function-class inlined-generic-function))
(defgeneric goal-p (node)
  (:generic-function-class inlined-generic-function))
(defgeneric send  (storage node)
  (:generic-function-class inlined-generic-function))

