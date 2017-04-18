
(in-package :alien)



(defun decode-parent-op (states schema)
  (let ((from (schema-index schema :action))
        (to   (schema-index schema :cost))
        acc)
    (map-ones states
              (lambda (bv)
                (push (subseq bv from to) acc)))
    acc))
