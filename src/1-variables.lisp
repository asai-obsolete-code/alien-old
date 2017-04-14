;; decide the state encoding.

(in-package :alien)

;; (defvar *sas*)
;; (defparameter *sas* (fd-preprocess #p"sets/aaai16-opt/blocks/p01.pddl"))
(defparameter *sas* (fd-preprocess #p"sets/aaai16-opt/airport/p01.pddl"))

(define-symbol-macro %variables% (sas-variables *sas*))

(defun sas-variable (n)
  "return the object representing sas variable"
  (aref %variables% n))

(defun sas-value (n m)
  "return the object representing sas atom"
  (aref (variable-values (aref %variables% n)) m))

#|

Naming: size and position are derived from cl:byte-size and cl:byte-position

|#

;;;; unate encoding

(defun sizes-unate ()
  "returns a list of variable encoding length"
  (match *sas*
    ((sas variables)
     (iter (for v in-vector variables)
           (match v
             ((variable values)
              (collect
                  (ceiling (log (length values) 2)))))))))

(defun size-unate (n)
  (match *sas*
    ((sas variables)
     (match (elt variables n)
       ((variable values)
        (ceiling (log (length values) 2)))))))

(defun state-encoding-length-unate ()
  (reduce #'+ (sizes-unate)))

(defun positions-unate ()
  (iter (with current = 0)
        (for size in (sizes-unate))
        (collect current)
        (incf current size)))

(defun position-unate (n)
  (elt (positions-unate) n))

;;;; binate encoding

(defun sizes-binate ()
  "returns a list of variable encoding length"
  (match *sas*
    ((sas variables)
     (iter (for v in-vector variables)
           (match v
             ((variable values)
              (collect
                  (* 2 (ceiling (log (length values) 2))))))))))

(defun size-binate (n)
  (match *sas*
    ((sas variables)
     (match (elt variables n)
       ((variable values)
        (* 2 (ceiling (log (length values) 2))))))))

(defun state-encoding-length-binate ()
  (reduce #'+ (sizes-binate)))

(defun positions-binate ()
  (iter (with current = 0)
        (for size in (sizes-binate))
        (collect current)
        (incf current size)))

(defun position-binate (n)
  (elt (positions-binate) n))

;;;; encode values

;; binate encoding:
;; 0 -> 01
;; 1 -> 10
;; unknown -> 00
;; be careful not to forget encoding the most significant bits.

(defun unate-binate (unate length)
  (let ((binate 0))
    (iter (for i below length)
          (setf (ldb (byte 2 (* 2 i)) binate)
                (if (logbitp i unate) #b10 #b01)))
    binate))

(defun encode-value (n m)
  (ash (unate-binate m (size-unate n))
       (position-binate n)))

#|

implementation note:

perhaps it is beneficial to merge some variables

two variables with 3 values and 5 values: 15 combinations
4 bits (15<16) is better than 5 bits (3<4=2^2 + 5<8=2^3)


alternative encoding?  ------- not useful for binate zdd
0 -> 00
1 -> 01
? -> 10


|#
