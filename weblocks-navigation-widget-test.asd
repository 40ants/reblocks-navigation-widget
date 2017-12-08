#|
  This file is a part of weblocks-navigation-widget project.
|#

(in-package :cl-user)
(defpackage weblocks-navigation-widget-test-asd
  (:use :cl :asdf))
(in-package :weblocks-navigation-widget-test-asd)

(defsystem weblocks-navigation-widget-test
  :author ""
  :license ""
  :depends-on (:weblocks-navigation-widget
               :prove
               :hamcrest-prove)
  :components ((:module "t"
                :components
                ((:test-file "weblocks-navigation-widget"))))
  :description "Test system for weblocks-navigation-widget"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
