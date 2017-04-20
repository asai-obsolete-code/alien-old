#|
  This file is a part of alien project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage alien
  (:use :cl :trivia :alexandria :iterate :cl-cudd :sas-parser :arrow-macros)
  (:shadowing-import-from :sas-parser :variable)
  (:shadowing-import-from :arrow-macros :<>)
  (:export :solve :find-domain
           :fd-preprocess
           :solution-found
           :schema
           :unate
           :binate
           :schema-index))
(in-package :alien)

;; common definitions

(define-condition solution-found () ((states :initarg :states :reader solution-found-states)))

(defvar *fd-home* (asdf:system-relative-pathname :alien "FastDownward/"))

(defmacro with-renaming (bindings &body body)
  (let ((tmps (make-gensym-list (length bindings))))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst tmp old body :test #'equalp)))
    (iter (for (old new) in bindings)
          (for tmp in tmps)
          (setf body (subst new tmp body :test #'equalp))))
  `(progn ,@body))

(setf (fdefinition '$) #'uiop:run-program)
(setf (fdefinition '!) #'cudd:zdd-change)

(defun power-of-2-p (x)
  (= 1 (logcount x)))

(defun required (&optional message)
  (error "Missing a required slot ~@[~a~]" message))

(defun zip (&rest lists)
  (apply #'map 'list #'list lists))

(defmacro nop (&body body)
  (declare (ignore body))
  nil)

(defun dump (zdd)
  (map-ones zdd #'print))

(defmacro break+ (&rest args)
  (let* ((last-form (lastcar args))
         (last last-form)
         (butlast (butlast args)))
    (once-only (last)
      `(progn
         (break "~@{~a~2%~<;;~@; result:~4i~:@_~a~;~:>~2%~}"
                ,@(iter (for arg in butlast)
                        (collect `',arg)
                        (collect `(list ,arg)))
                ',last-form (list ,last))
         ,last))))

(defun bitvector->integer (bv)
  (iter (for b in-vector bv with-index i)
        (with res = 0)
        (when (> b 0)
          (setf (ldb (byte 1 i) res) 1))
        (finally (return res))))

(assert (= (bitvector->integer #*10000) 1))
(assert (= (bitvector->integer #*01000) 2))
