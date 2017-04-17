
(in-package :alien)

;;;; task definition

(defstruct task
  (operators (required) :type cudd:zdd-node)
  (axioms    (required) :type (array cudd:zdd-node))
  (init-op   (required) :type cudd:zdd-node)
  (goal-op   (required) :type cudd:zdd-node)
  (state-schema (required) :type schema)
  (operator-schema (required) :type schema))

;;;; schema definition

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
             (schema :action
                     (iter (for v in-vector variables)
                           (collecting
                            (match v
                              ((variable name values)
                               (schema name
                                       (iter (for i below (ceiling (log (length values) 2)))
                                             (collect (effect-bit-schema i)))))))))
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

;;;; encode SAS into ZDD

(defun encode-sas-to-zdd (&optional (*sas* *sas*)
                          &aux
                            (*operator-schema* (operator-schema))
                            (*state-schema* (state-schema)))
  (match *sas*
    ((sas variables
          operators
          axioms
          init
          goals
          mutex-groups)
     (make-task :operators (reduce #'zdd-union operators :key #'encode-operator)
                :axioms    (iter (with layers = (make-array (length variables) :initial-element nil))
                                 (for ax in-vector axioms)
                                 (push ax (aref layers (ematch ax
                                                         ((operator (effects (vector (effect affected))))
                                                          (match (aref variables affected)
                                                            ((variable axiom-layer)
                                                             axiom-layer))))))
                                 (finally
                                  (return
                                    (iter (for axioms in-vector layers)
                                          (when (first-iteration-p)
                                            (collecting
                                             (encode-default-op init variables)
                                             result-type 'vector))
                                          (when axioms
                                            (collecting
                                             (reduce #'zdd-union axioms :key #'encode-operator)
                                             result-type 'vector))))))
                :init-op   (encode-init-op init)
                :goal-op   (encode-goal-op goals)
                :state-schema *state-schema*
                :operator-schema *operator-schema*))))

(defun encode-condition (zdd var val)
  (iter (for i below (integer-length val))
        (setf zdd (zdd-change zdd (schema-index
                                   *operator-schema*
                                   +body+ var i (if (logbitp i val) +true+ +false+)))))
  zdd)
(defun encode-effect (zdd var val)
  (iter (for i below (integer-length val))
        (setf zdd (zdd-change zdd (schema-index
                                   *operator-schema*
                                   +body+ var i (if (logbitp i val) +add+ +del+)))))
  zdd)

(defun encode-operator (operator)
  (with-renaming ((! zdd-change))
    (ematch operator
      ((operator prevail effects cost)
       (let ((zdd (zdd-set-of-emptyset)))
         (iter (for (var . val) in-vector prevail)
               (setf zdd (encode-condition zdd var val)))
         (iter (for e in-vector effects)
               (match e
                 ((effect conditions affected require newval)
                  (when (plusp require)
                    (setf zdd (encode-condition zdd affected require)))
                  (setf zdd
                        (zdd-union (iter (with zdd = (encode-effect zdd affected newval))
                                         (for (var . val) in-vector conditions)
                                         (setf zdd (encode-condition zdd var val))
                                         (finally (return zdd)))
                                   zdd)))))
         (iter (for i below (integer-length cost))
               (when (logbitp i cost)
                 (setf zdd (! zdd (schema-index *operator-schema* +cost+ i)))))
         zdd)))))

(defun encode-init-op (init)
  (iter (with zdd = (zdd-set-of-emptyset))
        (for val in-vector init with-index var)
        (setf zdd (encode-effect zdd var val))
        (finally (return zdd))))

(defun encode-goal-op (goals)
  (iter (with zdd = (zdd-set-of-emptyset))
        (for (var . val) in-vector goals)
        (setf zdd (encode-condition zdd var val))
        (finally (return zdd))))

(defun encode-default-op (init variables)
  "Encode a defaulting operation for derived variables, used before evaluating axioms."
  (iter (with zdd = (zdd-set-of-emptyset))
        (for val      in-vector init       with-index var)
        (for variable in-vector variables)
        (when (>= (variable-axiom-layer variable) 0)
          (setf zdd (encode-effect zdd var val)))
        (finally (return zdd))))

;;;; apply operation

(defvar *apply-cache*)
(defun apply-op (ops states)
  (let ((*apply-cache* (make-hash-table :test 'equalp)))
    (%apply ops states 0)))

(defstruct apply-cache-key
  (states-ptr 0 :type (UNSIGNED-BYTE 64))
  (ops-ptr 0 :type (UNSIGNED-BYTE 64))
  (index 0 :type (UNSIGNED-BYTE 64)))

(defun apply-cache-key (states ops index)
  (make-apply-cache-key :states-ptr (cffi:pointer-address (node-pointer states))
                        :ops-ptr (cffi:pointer-address (node-pointer ops))
                        :index index))

(defun %apply (ops states index)
  (cond
    ((<= (schema-size (schema-ref *state-schema* +body+)) index) states)
    ((node-equal (zdd-emptyset) ops)    states)
    ((node-equal (zdd-emptyset) states) states)
    (t
     (let ((key (apply-cache-key states ops index)))
       (match (gethash key *apply-cache*)
         (nil
          (setf (gethash key *apply-cache*) (%%apply ops states index)))
         (result
          result))))))

(defun %%apply (ops states index)
  (flet ((si ()  (+ (schema-index *state-schema* +body+) index)) ;skips variable schema
         (oi (x) (+ (schema-index *operator-schema* +body+) (* 4 index) x)))
    (with-renaming ((+ zdd-union)
                    (_1 zdd-onset)
                    (_0 zdd-offset)
                    (set zdd-set)
                    (unset zdd-unset))
      ;; Apply operation should be linear in the zdd size.
      ;; 
      ;; Note: operators use a binate representation;
      ;; thus +add+/+del+ bits never becomes 1 simultaneously.
      ;; +true+/+false+ bits neither.
      (-> (zdd-emptyset)
        (+ (-> (%apply (-> states (_1 (si)))
                   ;;^^^ O(1) op since INDEX is always the top node
                   (-> ops    (_1 (oi +true+)) (_1 (oi +del+)))
                   ;;^^^ O(1) too -- +del+ may require x8 operations
                   ;; When +true+ is true, +false+ should be false.
                   ;; also when +del+ is true, +add+ should be false.
                   (1+ index))
             (set (oi +true+))
             (unset (oi +false+))
             (unset (oi +add+))
             (set (oi +del+))))
        (+ (-> (%apply (-> states (_1 (si)))
                       (-> ops    (_1 (oi +true+)) (_1 (oi +add+)))
                       ;; this allows +add+ to be 0 or 1.
                       ;; due to this operation, INDEX is no longer the top node index in OPERATOR
                       (1+ index))
             (set (si))
             (set (oi +true+))
             (unset (oi +false+))
             (set (oi +add+))
             (unset (oi +del+))))
        (+ (-> (%apply (-> states (_1 (si)))
                       (-> ops    (_1 (oi +true+)) (_0 (oi +add+)) (_0 (oi +del+)))
                       ;; this allows +add+ to be 0 or 1.
                       ;; due to this operation, INDEX is no longer the top node index in OPERATOR
                       (1+ index))
             (set (si))
             (set (oi +true+))
             (unset (oi +false+))
             (unset (oi +add+))
             (unset (oi +del+))))
        (+ (-> (%apply (-> states (_0 (si)))
                       ;;^^^ O(1) op since INDEX is always the top node
                       (-> ops    (_1 (oi +false+)) (_1 (oi +add+)))
                       (1+ index))
             (set (si))
             (unset (oi +true+))
             (set (oi +false+))
             (set (oi +add+))
             (unset (oi +del+))))
        (+ (-> (%apply (-> states (_0 (si)))
                   (-> ops    (_1 (oi +false+)) (_1 (oi +del+)))
                   (1+ index))
             (unset (oi +true+))
             (set (oi +false+))
             (unset (oi +add+))
             (set (oi +del+))))
        (+ (-> (%apply (-> states (_0 (si)))
                   (-> ops    (_1 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                   (1+ index))
             (unset (oi +true+))
             (set (oi +false+))
             (unset (oi +add+))
             (unset (oi +del+))))))))

;;;; evaluate axioms

(defun apply-axiom (axiom-layers states)
  "cf. Helmert09 aij p11 Sec 2 Definition 5 algorithm evaluate-axioms
Defaulting operation is also implemented as an operator. This is included in the first axiom layer."
  (let ((*apply-cache* (make-hash-table :test 'equalp)))
    (iter (for axioms in-vector axiom-layers)
          (iter (for tmp = (%apply axioms states 0))
                (until (node-equal states tmp))
                (setf states tmp)))
    states))

