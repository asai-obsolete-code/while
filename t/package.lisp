#|
  This file is a part of while project.
  Copyright (c) 2018 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :while.test
  (:use :cl
        :while
        :fiveam
        :iterate :alexandria :trivia))
(in-package :while.test)



(def-suite :while)
(in-suite :while)

;; run test with (run! test-name) 

(test while

  )



