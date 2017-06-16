#|
This file is a part of aurora project.
Copyright (c) 2017 Sacha El Masry (sacha@rojoynegroclub.com)
|#

(in-package :cl-user)
(defpackage aurora-test-asd
  (:use :cl :asdf))
(in-package :aurora-test-asd)

(defsystem aurora-test
  :author "Sacha El Masry"
  :license "BSD"
  :depends-on (:aurora
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "aurora"))))
  :description "Test system for aurora"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
