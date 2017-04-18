
(in-package :alien)

(defun get-first-state (states)
  (let ((bv (block nil
              (map-ones states
                        (lambda (bv)
                          (return bv))))))
    (iter (with zdd = (zdd-set-of-emptyset))
          (for b in-vector bv with-index i)
          (when (= 1 b)
            (setf zdd (zdd-change zdd i)))
          (finally (return zdd)))))

(defun decode-parent-op (states schema)
  (let ((from (schema-index schema :action))
        (to   (schema-index schema :cost))
        acc)
    (map-ones states
              (lambda (bv)
                (push (subseq bv from to) acc)))
    acc))

(defun obtain-solutions (states schema)
  (let ((op (decode-parent-op states schema)))
    (unapply-op states )))
