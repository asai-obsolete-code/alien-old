
(in-package :alien)

;;;; zdd variable schema

(defstruct schema
  name
  (size 1 :type integer)
  (children (vector) :type (array schema))
  (offsets (vector) :type (array integer))
  #+ng
  (%children-bt (trivialib.red-black-tree:leaf) :type trivialib.red-black-tree:rb-tree))

(defun schema (name &rest children)
  (if children
      (make-schema :name name
                   :size (reduce #'+ children :key #'schema-size)
                   :children (coerce children 'simple-vector)
                   :offsets (let ((acc 0))
                              (map 'vector (lambda (x) (prog1 acc (incf acc (schema-size x)))) children))
                   #+ng :%children-bt #+ng (make-bt children))
      (make-schema :name name)))

(defun unate (&optional name) (schema name))
(defun binate (&optional name) (schema name (schema :true) (schema :false)))

#+example
(print
 (schema (leaf)
         (schema (leaf)
                 (leaf))
         (leaf)
         (schema (leaf)
                 (leaf))))

(defun schema-index (schema indices)
  (ematch* (schema indices)
    (((schema :children (vector)) nil)
     0)
    (((schema children offsets) (list* (and index (type integer)) more-indices))
     (+ (aref offsets index)
        (schema-index (aref children index) more-indices)))
    (((schema children offsets) (list* name more-indices))
     (+ (aref offsets index)
        (schema-index (find index children :key #'schema-name) more-indices)))))

#+example
(schema-index (schema (leaf)
                      (schema (leaf)
                              (leaf))
                      (schema (leaf)
                              (leaf))) '(2 0))

#+ng
(defun make-bt (children)
  (with-renaming ((leaf   trivialib.red-black-tree:leaf)
                  (insert trivialib.red-black-tree:rb-insert))
    (iter (with tree = (leaf))
          (for c in children)
          (with offset = 0)
          (for s = (schema-size c))
          (setf tree (insert tree offset c))
          (incf offset s)
          (finally (return tree)))))
