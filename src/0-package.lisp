#|
  This file is a part of alien project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage alien
  (:use :cl :trivia :alexandria :iterate :cl-cudd)
  (:export :solve))
(in-package :alien)

;; common definitions

(define-condition solution-found () ())

(defvar *fd-home* (asdf:system-relative-pathname :alien "FastDownward/"))

