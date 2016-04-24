#|
  This file is a part of alien project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage alien.test-asd
  (:use :cl :asdf))
(in-package :alien.test-asd)


(defsystem alien.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of alien"
  :license "LLGPL"
  :depends-on (:alien
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run! :alien))"))
))
