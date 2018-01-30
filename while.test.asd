#|
  This file is a part of while project.
  Copyright (c) 2018 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage while.test-asd
  (:use :cl :asdf))
(in-package :while.test-asd)


(defsystem while.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of while"
  :license "LLGPL"
  :depends-on (:while
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :while)"))
))
