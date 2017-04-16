
(in-package :alien)

;;;; task definition

(defstruct task
  (operators (required) :type (array op))
  (axioms    (required) :type (array op))
  (init-op   (required) :type op)
  (goal-op   (required) :type op)
  mutex-groups)

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

(defun encode-sas-to-zdd ()
  (match *sas*
    ((sas variables
          operators
          axioms
          init
          goals
          mutex-groups)
     (make-task :operators (reduce #'zdd-union operators :key #'encode-operator)
                :axioms    (reduce #'zdd-union axioms :key #'encode-axioms)
                :init-op   (encode-init-op init)
                :goal-op   (encode-goal-op goals)
                ;; :mutex-groups (encode-mutex-groups mutex-groups)
                ))))

(defun encode-condition (zdd var val)
  (iter (for i below (integer-length val))
        (setf zdd (! zdd (schema-index
                          *operator-schema*
                          +body+ var i (if (logbitp i val) +true+ +false+)))))
  zdd)
(defun encode-effect (zdd var val)
  (iter (for i below (integer-length val))
        (setf zdd (! zdd (schema-index
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
                        (if-then-else (iter (with zdd = (zdd-set-of-emptyset))
                                            (for (var . val) in-vector conditions)
                                            (setf zdd (encode-condition zdd var val))
                                            (finally (return zdd)))
                                      (encode-effect zdd affected newval)
                                      zdd)))))
         (iter (for i below (integer-length cost))
               (when (logbitp i val)
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
        (setf zdd (encode-precondition zdd var val))
        (finally (return zdd))))

;;;; apply operation

(defvar *apply-cache*)
(defun apply-ops (ops states)
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
  (with-renaming ((+ zdd-union)
                  (_1 zdd-onset)
                  (_0 zdd-offset)
                  (set zdd-set)
                  (unset zdd-unset))
    (flet ((si ()  (+ (schema-index *state-schema* +body+) index)) ;skips variable schema
           (oi (x) (+ (schema-index *operator-schema* +body+) (* 4 index) x)))
      ;; Apply operation should be linear in the zdd size.
      ;; 
      ;; Note: operators use a binate representation;
      ;; thus +add+ bit and +del+ bit never becomes 1 simultaneously.
      (-> (zdd-emptyset)
        (+ (%apply (-> states (_1 (si)))
                   ;;^^^ O(1) op since INDEX is always the top node
                   (-> ops    (_1 (oi +true+)) (_1 (oi +del+)))
                   ;;^^^ O(1) too -- +del+ may require x8 operations
                   ;; When +true+ is true, +false+ should be false.
                   ;; also when +del+ is true, +add+ should be false.
                   (1+ index)))
        (+ (-> (%apply (-> states (_0 (si)))
                       ;;^^^ O(1) op since INDEX is always the top node
                       (-> ops    (_1 (oi +false+)) (_1 (oi +add+)))
                       (1+ index))
             (set (si))))
        (+ (-> (%apply (-> states (_1 (si)))
                       (-> ops    (_1 (oi +true+)) (_0 (oi +del+)))
                       ;; this allows +add+ to be 0 or 1.
                       ;; due to this operation, INDEX is no longer the top node index in OPERATOR
                       (1+ index))
             (set (si))))
        (+ (%apply (-> states (_0 (si)))
                   (-> ops    (_1 (oi +false+)) (_0 (oi +add+)))
                   (1+ index)))))))

;;;; evaluate axioms

(defun apply-axiom (op states)
  "cf. Helmert09 aij p11 Sec 2 Definition 5 algorithm evaluate-axioms"
  (let ((*apply-cache* (make-hash-table :test 'equalp)))
    (%applyx ops states 0)))

(defun %applyx (ops states index)
  (cond
    ((<= (schema-size (schema-ref *state-schema* +body+)) index) states)
    ((node-equal (zdd-emptyset) ops)    states)
    ((node-equal (zdd-emptyset) states) states)
    (t
     (let ((key (apply-cache-key states ops index)))
       (match (gethash key *apply-cache*)
         (nil
          (setf (gethash key *apply-cache*) (%%applyx ops states index)))
         (result
          result))))))

(defun %%applyx (ops states index)
  (with-renaming ((+ zdd-union)
                  (_1 zdd-onset)
                  (_0 zdd-offset)
                  (set zdd-set)
                  (unset zdd-unset))
    (flet ((si ()  (+ (schema-index *state-schema* +body+) index)) ;skips variable schema
           (oi (x) (+ (schema-index *operator-schema* +body+) (* 4 index) x)))
      (-> (zdd-emptyset)
        ;; (+ (%apply (-> states (_1 (si)))
        ;;            ;;^^^ O(1) op since INDEX is always the top node
        ;;            (-> ops    (_1 (oi +true+)) (_1 (oi +del+)))
        ;;            ;;^^^ O(1) too -- +del+ may require x8 operations
        ;;            ;; When +true+ is true, +false+ should be false.
        ;;            ;; also when +del+ is true, +add+ should be false.
        ;;            (1+ index)))
        ;; (+ (-> (%apply (-> states (_0 (si)))
        ;;                ;;^^^ O(1) op since INDEX is always the top node
        ;;                (-> ops    (_1 (oi +false+)) (_1 (oi +add+)))
        ;;                (1+ index))
        ;;      (set (si))))
        ;; (+ (-> (%apply (-> states (_1 (si)))
        ;;                (-> ops    (_1 (oi +true+)) (_0 (oi +del+)))
        ;;                ;; this allows +add+ to be 0 or 1.
        ;;                ;; due to this operation, INDEX is no longer the top node index in OPERATOR
        ;;                (1+ index))
        ;;      (set (si))))
        ;; (+ (%apply (-> states (_0 (si)))
        ;;            (-> ops    (_1 (oi +false+)) (_0 (oi +add+)))
        ;;            (1+ index)))
        ))))
