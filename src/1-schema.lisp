
(in-package :alien)

;;;; zdd variable schema

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct schema
    name
    (size 1 :type integer)
    (children (vector) :type (array schema))
    (offsets (vector) :type (array integer))))

(defun schema (name &rest children)
  (let ((children (flatten children)))
    (if children
        (make-schema :name name
                     :size (reduce #'+ children :key #'schema-size)
                     :children (coerce children 'simple-vector)
                     :offsets (let ((acc 0))
                                (map 'vector (lambda (x) (prog1 acc (incf acc (schema-size x)))) children)))
        (make-schema :name name))))

(defun unate (&optional name) (schema name))
(defun binate (&optional name) (schema name (schema :true) (schema :false)))

(defun schema-index (schema indices)
  (ematch* (schema indices)
    (((schema :children (vector)) nil)
     0)
    (((schema children offsets) (list* (and index (type integer)) more-indices))
     (+ (aref offsets index)
        (schema-index (aref children index) more-indices)))
    (((schema children offsets) (list* name more-indices))
     (iter (for c in-vector children with-index index)
           (when (equal (schema-name c) name)
             (return
               (+ (aref offsets index)
                  (schema-index (aref children index) more-indices))))))))

