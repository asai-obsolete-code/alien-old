#|
  This file is a part of alien project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :alien.test
  (:use :cl
        :alien
        :fiveam
        :trivia :alexandria :iterate :pddl2))
(in-package :alien.test)



(def-suite :alien)
(in-suite :alien)

;; run test with (run! test-name) 

(test alien

  )



