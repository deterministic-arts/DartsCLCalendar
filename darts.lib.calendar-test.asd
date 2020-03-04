
(in-package #:common-lisp-user)
(defpackage #:darts.asdf (:use #:common-lisp #:asdf))
(in-package #:darts.asdf)

(defsystem :darts.lib.calendar-test
  :name "darts.lib.calendar-test"
  :licence "MIT"
  :depends-on (#:stefil #:darts.lib.calendar)
  :serial t
  :components
  ((:module :test
    :components
    ((:file "package") 
     (:file "localrep")
     (:file "trailer")
     ))))
