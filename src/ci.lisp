(defpackage #:weblocks-navigation-widget/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter))
(in-package weblocks-navigation-widget/ci)


(defworkflow linter
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))


(defworkflow docs
  :on-push-to "master"
  :jobs ((40ants-ci/jobs/docs:build-docs)))
