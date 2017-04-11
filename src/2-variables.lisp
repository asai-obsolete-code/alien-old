;; decide the state encoding.

(in-package :alien)

(defun index-table^2 (sas)
  "returns a list of variable encoding length"
  (match sas
    ((sas variables)
     (iter (for v in-vector variables)
           (match v
             ((variable values)
              (collect
                  (length values))))))))

(defun index-table (sas)
  "returns a list of variable encoding length"
  (match sas
    ((sas variables)
     (iter (for v in-vector variables)
           (match v
             ((variable values)
              (collect
                  (ceiling (log (length values) 2)))))))))

(defun state-encoding-length (sas)
  (reduce #'+ (index-table sas)))

;; (log (reduce #'* (index-table^2))) --- lower bound

;; two variables with 3 values and 5 values: 15 combinations
;; 4 bits (15<16) is better than 5 bits (3<4=2^2 + 5<8=2^3)

;; power of 2
(defun power-of-2 (x)
  (= 1 (logcount x)))

(setf tmp (nth-value 1 (fd-preprocess #p"sets/aaai16-opt/blocks/p01.pddl")))

(state-encoding-length tmp)             ;->17

