(defsystem "reblocks-navigation-widget-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/reblocks-navigation-widget/"
  :class :package-inferred-system
  :description "Provides CI settings for reblocks-navigation-widget."
  :source-control (:git "https://github.com/40ants/reblocks-navigation-widget")
  :bug-tracker "https://github.com/40ants/reblocks-navigation-widget/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "reblocks-navigation-widget-ci/ci"))
