#|
  This file is a part of alien project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :alien.test
  (:use :cl
        :alien
        :fiveam
        :trivia :alexandria :iterate))
(in-package :alien.test)



(def-suite :alien)
(in-suite :alien)

;; run test with (run! test-name) 

(test alien
  (finishes
    (schema nil
            (schema nil)
            (schema nil
                    (schema nil)
                    (schema nil))
            (schema nil)
            (schema nil
                    (schema nil)
                    (schema nil))))
  (is (= 4
         (schema-index (schema nil
                               (schema nil)
                               (schema nil
                                       (schema nil)
                                       (schema nil))
                               (schema nil)
                               (schema nil
                                       (schema nil)
                                       (schema nil)))
                       '(3 0))))
  (is (= 4
         (schema-index (schema nil
                               (schema nil)
                               (schema nil
                                       (schema nil)
                                       (schema nil))
                               (schema nil)
                               (schema :last
                                       (schema :true)
                                       (schema nil)))
                       '(:last :true)))))



