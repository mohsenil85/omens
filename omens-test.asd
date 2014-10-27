#|
  This file is a part of omens project.
|#

(in-package :cl-user)
(defpackage omens-test-asd
  (:use :cl :asdf))
(in-package :omens-test-asd)

(defsystem omens-test
  :author ""
  :license ""
  :depends-on (:omens
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "omens"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
