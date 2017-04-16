
(in-package :alien)

(defstruct task
  (operators (required) :type (array op))
  (axioms    (required) :type (array op))
  (init-op   (required) :type op)
  (goal-op   (required) :type op)
  mutex-groups)

(defconstant +true+ 0)
(defconstant +false+ 1)
(defconstant +add+ 2)
(defconstant +del+ 3)
(defconstant +body+ 0)
(defconstant +cost+ 1)

(defun state-schema ()
  (match *sas*
    ((sas variables)
     (schema nil
             (schema :body
                     (iter (for v in-vector variables)
                           (collecting
                            (match v
                              ((variable name values)
                               (schema name
                                       (iter (for i below (ceiling (log (length values) 2)))
                                             (collect (unate)))))))))
             ;; 32 bit integer
             (schema :cost (iter (repeat 32) (collect (unate))))))))

(defun effect-bit-schema (name)
  (schema name
          (schema :true)
          (schema :false)
          (schema :add)
          (schema :del)))

(defun operator-schema ()
  (match *sas*
    ((sas variables)
     (schema nil
             (schema :body
                     (iter (for v in-vector variables)
                           (collecting
                            (match v
                              ((variable name values)
                               (schema name
                                       (iter (for i below (ceiling (log (length values) 2)))
                                             (collecting (effect-bit-schema i)))))))))
             ;; 32 bit integer
             (schema :cost (iter (repeat 32) (collect (unate))))))))

(defparameter *operator-schema* (operator-schema))
(defparameter *state-schema*   (state-schema))

(defun encode-sas-to-zdd ()
  (match *sas*
    ((sas variables
          operators
          axioms
          init
          goals
          mutex-groups)
     (make-task :operators (map 'vector #'encode-operator operators)
                :axioms    (map 'vector #'encode-operator axioms)
                :init-op   (encode-init-op init)
                :goal-op   (encode-goal-op goals)
                ;; :mutex-groups (encode-mutex-groups mutex-groups)
                ))))

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
           (_del 0))                    ;TODO: effect condition
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

(defun encode-init-op (init)
  (make-op :pre (zdd-set-of-emptyset)
           :add (zdd-by-integer
                 (iter (with result = 0)
                       (for value in-vector init with-index i)
                       (setf result (logior result (encode-value i value)))
                       (finally (return result))))
           :del (zdd-set-of-emptyset)
           :cost 0))


(defun encode-goal-op (goals)
  (make-op :pre (zdd-by-integer
                 (iter (with result = 0)
                       (for (var . val) in-vector goals)
                       (setf result (logior result (encode-value var val)))
                       (finally (return result))))
           :add (zdd-set-of-emptyset)
           :del (zdd-set-of-emptyset)
           :cost 0))

(defun apply-op (op states)
  (with-renaming ((- zdd-difference)
                  (+ zdd-union)
                  (/ zdd-divide-binate)
                  (* zdd-product-binate))
    (match op
      ((op pre add del)
       (+ (- (* pre (/ states pre)) del) add)))))

(defun apply-axiom (op states)
  "cf. Helmert09 aij p11 Sec 2 Definition 5 algorithm evaluate-axioms"
  (with-renaming ((- zdd-difference)
                  (+ zdd-union)
                  (/ zdd-divide-binate)
                  (* zdd-product-binate))
    (match op
      ((op pre add del)
       (+ (- (* pre (/ states pre)) del) add)))))

;; define a recursive procedure for action application?

(defun apply-ops (ops states)
  (%apply ops states 0))
(defun %apply (ops states var)
  (cond
    ((<= (schema-size *state-schema*) var) states)
    ((node-equal (zdd-emptyset) ops)    states)
    ((node-equal (zdd-emptyset) states) states)
    (t
     (with-renaming ((- zdd-difference)
                     (+ zdd-union)
                     (/ zdd-divide-binate)
                     (* zdd-product-binate)
                     (_1 zdd-onset)
                     (_0 zdd-offset)
                     (set zdd-set)
                     (unset zdd-unset))
       (flet ((si () (schema-index *state-schema* (list var)))
              (oi (x) (schema-index *operator-schema* (list var x))))
         (symbol-macrolet ((true 0) (false 1) (add 2) (del 3))
           (-> (zdd-emptyset)
             (zdd-union (%apply (-> states (_1 (si)))
                                (-> ops    (_1 (oi true)) (_1 (oi del)))
                                (1+ var)))
             (zdd-union (-> (%apply (-> states (_0 (si)))
                                    (-> ops    (_1 (oi false)) (_1 (oi add)))
                                    (1+ var))
                          (set (si))))
             (zdd-union (-> (%apply (-> states (_1 (si)))
                                    (-> ops    (_1 (oi true)) (_0 (oi del)))
                                    (1+ var))
                          (set (si))))
             (zdd-union (%apply (-> states (_0 (si)))
                                (-> ops    (_1 (oi false)) (_0 (oi add)))
                                (1+ var))))))))))
