

(in-package :alien)

(defvar *states* nil
  "A special variable holding the closed/open list dd.")

(defvar *operators* nil
  "A special variable holding the operator zdd.")

(defclass variable ()
     ((value :type integer))
  (:metaclass zdd-class))

(defclass raw-state ()
     ((variables :type variable :length :*))
  (:metaclass zdd-class))

(defclass operator ()
     ((precondition :type raw-state)
      (add :type raw-state)
      (del :type raw-state))
  (:metaclass zdd-class))

(defclass state ()
     ((bits :type raw-state)
      #+nil (parent :type raw-state)
      (parent-add :type raw-state)
      (parent-del :type raw-state)
      (status :type integer))
  (:metaclass zdd-class))

(defun encode-sas-to-zdd (*sas*)
  (match *sas*
    ((sas :


  )
