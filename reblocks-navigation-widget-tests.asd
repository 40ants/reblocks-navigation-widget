(defsystem "reblocks-navigation-widget-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-navigation-widget/"
  :class :package-inferred-system
  :description "Provides tests for reblocks-navigation-widget."
  :source-control (:git "https://github.com/40ants/reblocks-navigation-widget")
  :bug-tracker "https://github.com/40ants/reblocks-navigation-widget/issues"
  :pathname "t"
  :depends-on ("reblocks-navigation-widget-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
