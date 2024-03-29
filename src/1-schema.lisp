
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

(defun unate () (schema :unate))
(defun binate () (schema :binate (schema :true) (schema :false)))

(defun schema-index (schema &rest indices)
  (labels ((rec (schema indices)
             (ematch* (schema indices)
               ((_ nil)
                0)
               (((schema :children (vector)) _)
                (error "too many indices! ~a" indices))
               (((schema children offsets) (list* (and index (type integer)) more-indices))
                (+ (aref offsets index)
                   (rec (aref children index) more-indices)))
               (((schema children offsets) (list* name more-indices))
                (iter (for c in-vector children with-index index)
                      (when (equal (schema-name c) name)
                        (return
                          (+ (aref offsets index)
                             (rec c more-indices)))))))))
    (rec schema (flatten indices))))

(defun schema-ref (schema &rest indices)
  (labels ((rec (schema indices)
             (ematch* (schema indices)
               ((_ nil)
                schema)
               (((schema :children (vector)) _)
                (error "too many indices! ~a" indices))
               (((schema children) (list* (and index (type integer)) more-indices))
                (rec (aref children index) more-indices))
               (((schema children) (list* name more-indices))
                (iter (for c in-vector children with-index index)
                      (when (equal (schema-name c) name)
                        (return
                          (rec c more-indices))))))))
    (rec schema (flatten indices))))
