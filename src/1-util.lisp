
(in-package :alien)

(defmacro with-renaming (bindings &body body)
  (let ((tmps (make-gensym-list (length bindings))))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst tmp old body :test #'equalp)))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst new tmp body :test #'equalp))))
  `(progn ,@body))

(setf (fdefinition '$) #'uiop:run-program)

(defun power-of-2-p (x)
  (= 1 (logcount x)))

(defun required (&optional message)
  (error "Missing a required slot"))
