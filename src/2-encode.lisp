

(in-package :alien)

(defvar *states* nil
  "A special variable holding the closed/open list dd.")

;; (defclass variable ()
;;      ((value :type integer))
;;   (:metaclass zdd-class))
;; 
;; (defclass raw-state ()
;;      ((variables :type variable :length :*))
;;   (:metaclass zdd-class))
;; 
;; (defclass operator ()
;;      ((precondition :type raw-state)
;;       (add :type raw-state)
;;       (del :type raw-state))
;;   (:metaclass zdd-class))
;; 
;; (defclass state ()
;;      ((bits :type raw-state)
;;       #+nil (parent :type raw-state)
;;       (parent-add :type raw-state)
;;       (parent-del :type raw-state)
;;       (status :type integer))
;;   (:metaclass zdd-class))

(defstruct op
  (pre  (required) :type cudd:node)
  (add  (required) :type cudd:node)
  (del  (required) :type cudd:node)
  (cost (required) :type fixnum))

(defstruct task
  (operators (required) :type (array op))
  (init      (required) :type op)
  (goals     (required) :type op)
  mutex-groups)

(defun encode-sas-to-zdd ()
  (match *sas*
    ((sas variables
          operators
          init
          goals
          mutex-groups)
     (make-task :operators (map 'vector #'encode-operator operators)
                :init      (encode-init init)
                :goals     (encode-goals goals)
                ;; :mutex-groups (encode-mutex-groups mutex-groups)
                ))))

;; (defvar *operators* nil
;;   "A special variable holding the operator zdd.")

(defun zdd-change-by-integer (zdd int)
  (iter (for i below (integer-length int))
        (when (logbitp i int)
          (setf zdd (zdd-change zdd i))))
  zdd)

(defun zdd-by-integer (int)
  (zdd-change-by-integer (zdd-set-of-emptyset) int))

(defun encode-operator (operator)
  (ematch operator
    ((operator prevail effects cost)
     (let ((_pre 0)
           (_add 0)
           (_del 0))
       (iter (for (var . val) in-vector prevail)
             (setf _pre (logior _pre (encode-value var val))))
       (iter (for e in-vector effects)
             (match e
               ((effect conditions affected require newval)
                (when (plusp require)
                  (let ((val (encode-value affected require)))
                    (setf _pre (logior _pre val))
                    (setf _del (logior _del val))))
                (setf _add (logior _add (encode-value affected newval))))))
       (make-op :pre (zdd-by-integer _pre)
                :add (zdd-by-integer _add)
                :del (zdd-by-integer _del)
                :cost cost)))))

(defun encode-init (init)
  (make-op :pre (zdd-set-of-emptyset)
           :add (zdd-by-integer
                 (iter (with result = 0)
                       (for value in-vector init with-index i)
                       (setf result (logior result (encode-value i value)))
                       (finally (return result))))
           :del (zdd-set-of-emptyset)
           :cost 0))


(defun encode-goals (goals)
  (make-op :pre (zdd-by-integer
                 (iter (with result = 0)
                       (for (var . val) in-vector goals)
                       (setf result (logior result (encode-value var val)))
                       (finally (return result))))
           :add (zdd-set-of-emptyset)
           :del (zdd-set-of-emptyset)
           :cost 0))

