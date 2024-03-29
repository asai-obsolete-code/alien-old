
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


(defconstant +state-body+ 0)
(defconstant +state-action+ 1)
(defconstant +state-cost+ 2)
(defun state-schema ()
  (match *sas*
    ((sas variables)
     (schema :states
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
                                             (collect (effect-bit-schema)))))))))
             ;; 32 bit integer
             (schema :cost (iter (repeat 32) (collect (unate))))))))

(defconstant +true+ 0)
(defconstant +false+ 1)
(defconstant +add+ 2)
(defconstant +del+ 3)
(defun effect-bit-schema ()
  (schema :effect
          (schema :true)
          (schema :false)
          (schema :add)
          (schema :del)))

(defconstant +operator-body+ 0)
(defconstant +operator-cost+ 1)
(defconstant +operator-index+ 2)
(defun operator-schema ()
  (match *sas*
    ((sas variables)
     (schema :operator
             (schema :body
                     (iter (for v in-vector variables)
                           (collecting
                            (match v
                              ((variable name values)
                               (schema name
                                       (iter (for i below (ceiling (log (length values) 2)))
                                             (collecting (effect-bit-schema)))))))))
             ;; 32 bit integer
             (schema :cost (iter (repeat 32) (collect (unate))))
             ;; 32 bit integer
             (schema :index (iter (repeat 32) (collect (unate))))))))

(defvar *operator-schema*)
(defvar *state-schema*)

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
     (make-task :operators (encode-operators operators)
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
  (iter (for i below (max 1 (integer-length val)))
        (setf zdd (zdd-change zdd (schema-index
                                   *operator-schema*
                                   +operator-body+ var i (if (logbitp i val) +true+ +false+)))))
  zdd)

(defun encode-effect (zdd var val)
  (iter (for i below (max 1 (integer-length val)))
        (setf zdd (zdd-change zdd (schema-index
                                   *operator-schema*
                                   +operator-body+ var i (if (logbitp i val) +add+ +del+)))))
  zdd)

(defun encode-operators (operators)
  (iter (for operator in-vector operators with-index index)
        (reducing
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
                             (if (zerop (length conditions))
                                 (encode-effect zdd affected newval)
                                 (zdd-union (iter (with zdd = zdd)
                                                  (for (var . val) in-vector conditions)
                                                  (setf zdd (encode-condition zdd var val))
                                                  (finally (return (encode-effect zdd affected newval))))
                                            zdd))))))
              (iter (for i below (integer-length cost))
                    (when (logbitp i cost)
                      (setf zdd (! zdd (schema-index *operator-schema* +operator-cost+ i)))))
              (iter (for i below (integer-length index))
                    (when (logbitp i index)
                      (setf zdd (! zdd (schema-index *operator-schema* +operator-index+ i)))))
              zdd)))
         by #'zdd-union)))

(defun encode-init-op (init)
  (let ((zdd (zdd-set-of-emptyset)))
    (iter (for val in-vector init with-index var)
          (setf zdd (encode-effect zdd var val)))
    ;; init op has index number = -1
    (iter (for i below 32)
          (setf zdd (! zdd (schema-index *operator-schema* +operator-index+ i))))
    zdd))

(defun encode-goal-op (goals)
  (let ((zdd (zdd-set-of-emptyset)))
    (iter (for (var . val) in-vector goals)
          (setf zdd (encode-condition zdd var val)))
    ;; init op has index number = -2
    (iter (for i below 31)
          (setf zdd (! zdd (schema-index *operator-schema* +operator-index+ i))))
    zdd))

(defun encode-default-op (init variables)
  "Encode a defaulting operation for derived variables, used before evaluating axioms."
  (iter (with zdd = (zdd-set-of-emptyset))
        (for val      in-vector init       with-index var)
        (for variable in-vector variables)
        (when (>= (variable-axiom-layer variable) 0)
          (setf zdd (encode-effect zdd var val)))
        (finally (return zdd))))

;;;; apply cache

(defvar *apply-cache*)

(defstruct apply-cache-key
  (states-ptr 0 :type (UNSIGNED-BYTE 64))
  (ops-ptr 0 :type (UNSIGNED-BYTE 64))
  (index 0 :type (UNSIGNED-BYTE 64)))

(defun apply-cache-key (states ops index)
  (make-apply-cache-key :states-ptr (cffi:pointer-address (node-pointer states))
                        :ops-ptr (cffi:pointer-address (node-pointer ops))
                        :index index))

;;;; apply operation

(defun apply-op (ops states)
  (let ((*apply-cache* (make-hash-table :test 'equalp)))
    (%apply ops states 0)))

(defun %apply (ops states index)
  (let ((key (apply-cache-key states ops index)))
    (match (gethash key *apply-cache*)
      (nil
       (setf (gethash key *apply-cache*) (%%apply ops states index)))
      (result
       result))))

(defun %%apply (ops states index)
  (flet ((si ()  (+ (schema-index *state-schema* +state-body+) index)) ;state index
         (di (x) (+ (schema-index *state-schema* +state-action+) (* 4 index) x)) ;recording action
         (oi (x) (+ (schema-index *operator-schema* +operator-body+) (* 4 index) x))
         (%apply (ops states index)
           ;; prevent recursion to the real function (for better debugging)
           (cond
             ((node-equal (zdd-emptyset) ops)    (zdd-emptyset))
             ((node-equal (zdd-emptyset) states) (zdd-emptyset))
             ((<= (schema-size (schema-ref *state-schema* +state-body+)) index) states)
             (t
              (%apply ops states index)))))
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
        (+ (-> (%apply (-> ops    (_1 (oi +true+)) (_1 (oi +del+)))
                       (-> states (_1 (si)))
                       (1+ index))
             (set   (di +true+))
             (unset (di +false+))
             (unset (di +add+))
             (set   (di +del+))))
        (+ (-> (%apply (-> ops    (_1 (oi +true+)) (_1 (oi +add+)))
                       (-> states (_1 (si)))
                       (1+ index))
             (set (si))
             (set   (di +true+))
             (unset (di +false+))
             (set   (di +add+))
             (unset (di +del+))))
        (+ (-> (%apply (-> ops    (_1 (oi +true+)) (_0 (oi +add+)) (_0 (oi +del+)))
                       (-> states (_1 (si)))
                       (1+ index))
             (set (si))
             (set   (di +true+))
             (unset (di +false+))
             (unset (di +add+))
             (unset (di +del+))))
        (+ (-> (%apply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +del+)))
                       (-> states (_1 (si)))
                       (1+ index))
             (unset (di +true+))
             (unset (di +false+))
             (unset (di +add+))
             (set   (di +del+))))
        (+ (-> (%apply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)))
                       (-> states (_1 (si)))
                       (1+ index))
             (set (si))
             (unset (di +true+))
             (unset (di +false+))
             (set   (di +add+))
             (unset (di +del+))))
        (+ (-> (%apply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                       (-> states (_1 (si)))
                       (1+ index))
             (set (si))
             (unset (di +true+))
             (unset (di +false+))
             (unset (di +add+))
             (unset (di +del+))))
        (+ (-> (%apply (-> ops    (_1 (oi +false+)) (_1 (oi +add+)))
                       (-> states (_0 (si)))
                       (1+ index))
             (set (si))
             (unset (di +true+))
             (set   (di +false+))
             (set   (di +add+))
             (unset (di +del+))))
        (+ (-> (%apply (-> ops    (_1 (oi +false+)) (_1 (oi +del+)))
                       (-> states (_0 (si)))
                       (1+ index))
             (unset (di +true+))
             (set   (di +false+))
             (unset (di +add+))
             (set   (di +del+))))
        (+ (-> (%apply (-> ops    (_1 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                       (-> states (_0 (si)))
                       (1+ index))
             (unset (di +true+))
             (set   (di +false+))
             (unset (di +add+))
             (unset (di +del+))))
        (+ (-> (%apply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)))
                       (-> states (_0 (si)))
                       (1+ index))
             (set (si))
             (unset (di +true+))
             (unset (di +false+))
             (set   (di +add+))
             (unset (di +del+))))
        (+ (-> (%apply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +del+)))
                       (-> states (_0 (si)))
                       (1+ index))
             (unset (di +true+))
             (unset (di +false+))
             (unset (di +add+))
             (set   (di +del+))))
        (+ (-> (%apply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                       (-> states (_0 (si)))
                       (1+ index))
             (unset (di +true+))
             (unset (di +false+))
             (unset (di +add+))
             (unset (di +del+))))))))

;;;; unapply

(defun unapply-op (ops states)
  (let ((*apply-cache* (make-hash-table :test 'equalp)))
    (%unapply ops states 0)))

(defun %unapply (ops states index)
  (let ((key (apply-cache-key states ops index)))
    (match (gethash key *apply-cache*)
      (nil
       (setf (gethash key *apply-cache*) (%%unapply ops states index)))
      (result
       result))))

(defun %%unapply (ops states index)
  (dump ops)
  (dump states)
  (flet ((si ()  (+ (schema-index *state-schema* +state-body+) index)) ;state index
         (oi (x) (+ (schema-index *operator-schema* +operator-body+) (* 4 index) x))
         (%unapply (ops states index)
           ;; prevent recursion to the real function (for better debugging)
           (cond
             ((node-equal (zdd-emptyset) ops)    (zdd-emptyset))
             ((node-equal (zdd-emptyset) states) (zdd-emptyset))
             ((<= (schema-size (schema-ref *state-schema* +state-body+)) index) states)
             (t
              (dump
               (%unapply ops states index))))))
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
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))))
          #+nil
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))))
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_1 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))
               (zdd-dont-care index)))
          #+nil
          (+ (-> (%unapply (-> ops    (_1 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))))
          #+nil
          (+ (-> (%unapply (-> ops    (_1 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))))
          (+ (-> (%unapply (-> ops    (_1 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_1 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))
               (set (si))))
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_1 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))))
          #+nil
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_1 (oi +false+)) (_1 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))))
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_1 (oi +false+)) (_0 (oi +add+)) (_1 (oi +del+)))
                           (-> states (_0 (si)))
                           (1+ index))))
          ;; 
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))
                 (set (si))))
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))
               (zdd-dont-care index)))
          #+nil
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_1 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))))
          (+ (-> (%unapply (-> ops    (_1 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))
                 (set (si))))
          (+ (-> (%unapply (-> ops    (_1 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))
               (set (si))))
          #+nil
          (+ (-> (%unapply (-> ops    (_1 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_1 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))))
          #+nil
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_1 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))))
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_1 (oi +false+)) (_1 (oi +add+)) (_0 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))
               (set (si))))
          #+nil
          (+ (-> (%unapply (-> ops    (_0 (oi +true+)) (_1 (oi +false+)) (_0 (oi +add+)) (_1 (oi +del+)))
                           (-> states (_1 (si)))
                           (1+ index))))))))

;;;; evaluate axioms

(defun apply-axiom (axiom-layers states)
  "cf. Helmert09 aij p11 Sec 2 Definition 5 algorithm evaluate-axioms
Defaulting operation is also implemented as an operator. This is included in the first axiom layer."
  (let ((*apply-cache* (make-hash-table :test 'equalp)))
    (iter (for axioms in-vector axiom-layers)
          (iter (for tmp = (%applyx axioms states 0))
                (until (node-equal states tmp))
                (setf states tmp)))
    states))

(defun %applyx (ops states index)
  (let ((key (apply-cache-key states ops index)))
    (match (gethash key *apply-cache*)
      (nil
       (setf (gethash key *apply-cache*) (%%applyx ops states index)))
      (result
       result))))

(defun %%applyx (ops states index)
  (flet ((si ()  (+ (schema-index *state-schema* +state-body+) index)) ;state index
         (oi (x) (+ (schema-index *operator-schema* +operator-body+) (* 4 index) x))
         (%applyx (ops states index)
           ;; prevent recursion to the real function (for better debugging)
           (cond
             ((node-equal (zdd-emptyset) ops)    (zdd-emptyset))
             ((node-equal (zdd-emptyset) states) (zdd-emptyset))
             ((<= (schema-size (schema-ref *state-schema* +state-body+)) index) states)
             (t
              (%applyx ops states index)))))
    (with-renaming ((+ zdd-union)
                    (_1 zdd-onset)
                    (_0 zdd-offset)
                    (set zdd-set)
                    (unset zdd-unset))
      ;; Applyx operation should be linear in the zdd size.
      ;; 
      ;; Note: operators use a binate representation;
      ;; thus +add+/+del+ bits never becomes 1 simultaneously.
      ;; +true+/+false+ bits neither.
      ;;
      ;; unlike apply, it does not modify the action fields.
      (-> (zdd-emptyset)
        (+ (%applyx (-> ops    (_1 (oi +true+)) (_1 (oi +del+)))
                    (-> states (_1 (si)))
                    (1+ index)))
        (+ (-> (%applyx (-> ops    (_1 (oi +true+)) (_1 (oi +add+)))
                        (-> states (_1 (si)))
                        (1+ index))
             (set (si))))
        (+ (-> (%applyx (-> ops    (_1 (oi +true+)) (_0 (oi +add+)) (_0 (oi +del+)))
                        (-> states (_1 (si)))
                        (1+ index))
             (set (si))))
        (+ (%applyx (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +del+)))
                    (-> states (_1 (si)))
                    (1+ index)))
        (+ (-> (%applyx (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)))
                        (-> states (_1 (si)))
                        (1+ index))
             (set (si))))
        (+ (-> (%applyx (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                        (-> states (_1 (si)))
                        (1+ index))
             (set (si))))
        (+ (-> (%applyx (-> ops    (_1 (oi +false+)) (_1 (oi +add+)))
                        (-> states (_0 (si)))
                        (1+ index))
             (set (si))))
        (+ (%applyx (-> ops    (_1 (oi +false+)) (_1 (oi +del+)))
                    (-> states (_0 (si)))
                    (1+ index)))
        (+ (%applyx (-> ops    (_1 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                    (-> states (_0 (si)))
                    (1+ index)))
        (+ (-> (%applyx (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +add+)))
                        (-> states (_0 (si)))
                        (1+ index))
             (set (si))))
        (+ (%applyx (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_1 (oi +del+)))
                    (-> states (_0 (si)))
                    (1+ index)))
        (+ (%applyx (-> ops    (_0 (oi +true+)) (_0 (oi +false+)) (_0 (oi +add+)) (_0 (oi +del+)))
                    (-> states (_0 (si)))
                    (1+ index)))))))
