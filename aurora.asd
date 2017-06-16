#|
  This file is a part of aurora project.
  Copyright (c) 2017 Sacha El Masry (sacha@rojoynegroclub.com)
|#

#|
  Aurora is a video file resizer and metadata tagger.

  Author: Sacha El Masry (sacha@rojoynegroclub.com)
|#

(in-package :cl-user)
(defpackage aurora-asd
  (:use :cl :asdf))
(in-package :aurora-asd)

(defsystem aurora
  :version "0.2"
  :author "Sacha El Masry"
  :license "BSD"
  :depends-on (:alexandria
               :cl-strings)
  :components ((:module "src"
                :components
                ((:file "filesystem-interface")
                 (:file "string-manipulation")
                 (:file "aurora" :depends-on
                        ("filesystem-interface" "string-manipulation")))))
  :description "Aurora is a video file resizer and metadata tagger."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op aurora-test))))
