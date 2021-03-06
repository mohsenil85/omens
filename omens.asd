#|
  This file is a part of omens project.
|#

(in-package :cl-user)
(defpackage omens-asd
  (:use :cl :asdf))
(in-package :omens-asd)

(defsystem omens
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cl-charms)
  :components ((:module "src"
                :serial t
                :components 
                ((:file "package")
                 (:file "omens" :depends-on ("package"))
                 (:file "screen" :depends-on ("package"))
                 )))
  :description ""
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
  :in-order-to ((test-op (test-op omens-test))))
