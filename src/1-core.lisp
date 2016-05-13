
#|

I try to be abstract as possible.
This is in order to (possibly) escaping from the notion of OPEN/CLOSED list search.

|#


(in-package :alien)

(define-condition solution-found ()
  ((node :initarg :node :reader node)))

(defun step (storage)
  (iter (for child in (expand (fetch storage)))
        (when (goal-p child)
          (signal 'solution-found :node child))
        (send child storage))
  storage)

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

