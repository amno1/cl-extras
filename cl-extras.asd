
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-extras.system)
    (defpackage :cl-extras.system
      (:use :common-lisp :asdf))))


(in-package :cl-extras.system)

(defclass src (cl-source-file)
  ((type :initform "cl")))

(defsystem :cl-extras 
  :description "Few minor addons" 
  :author "Arthur Miller <arthur.miller@live.com>" 
  :licence "MIT" 
  :version "0.5.0"
  :serial t
  :encoding :utf-8
  :components ((:src "packages")
               (:src "cl-extras"))
  :depends-on ())
