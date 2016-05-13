


(in-package :alien)

(defun run (open)
  (iter (for parent = (fetch open))
        (for child in (expand parent))
        (when (goal-p child)
          (return child))
        (send child open)))

(defgeneric fetch (open-list))
(defgeneric send  (open-list))
(defgeneric expand (node))
(defgeneric goal-p (node))
(defgeneric send  (open-list node))

