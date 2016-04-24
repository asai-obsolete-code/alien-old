#|
  This file is a part of alien project.
  Copyright (c) 2016 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Return of the Alien Technology to Classical Planning

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage alien-asd
  (:use :cl :asdf))
(in-package :alien-asd)


(defsystem alien
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :alexandria :iterate :pddl2)
  :components ((:module "src"
                :components
                ((:file "0-package"))))
  :description "Return of the Alien Technology to Classical Planning"
  :in-order-to ((test-op (test-op :alien.test))))
