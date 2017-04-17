#|
  This file is a part of alien project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage alien
  (:use :cl :trivia :alexandria :iterate :cl-cudd :sas-parser :arrow-macros)
  (:shadowing-import-from :sas-parser :variable)
  (:shadowing-import-from :trivia :<>)
  (:export :solve :find-domain
           :fd-preprocess
           :solution-found
           :schema
           :unate
           :binate
           :schema-index))
(in-package :alien)

;; common definitions

(define-condition solution-found () ((states :initarg states)))

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

(defun power-of-2-p (x)
  (= 1 (logcount x)))

(defun required (&optional message)
  (error "Missing a required slot"))
