#-asdf3.1 (error "reblocks-navigation-widget requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "reblocks-navigation-widget"
  :description "A container widget which switches between children widgets when user changes an url."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-navigation-widget/"
  :source-control (:git "https://github.com/40ants/reblocks-navigation-widget")
  :bug-tracker "https://github.com/40ants/reblocks-navigation-widget/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("reblocks-navigation-widget/core")
  :in-order-to ((test-op (test-op "reblocks-navigation-widget-tests"))))
