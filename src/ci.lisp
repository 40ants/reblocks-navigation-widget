(defpackage #:reblocks-navigation-widget/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter))
(in-package reblocks-navigation-widget/ci)


(defworkflow linter
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter)))


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs)))
