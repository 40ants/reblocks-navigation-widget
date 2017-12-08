(in-package :cl-user)
(defpackage weblocks-navigation-widget-test
  (:use :cl
        :weblocks-navigation-widget
        :prove
        :hamcrest.matchers))
(in-package :weblocks-navigation-widget-test)


(plan 1)

(subtest "Replace this test with real staff."
  (assert-that (foo 1 2 3)
               (contains 1 2 3)))

(finalize)
