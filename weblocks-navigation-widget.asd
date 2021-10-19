(defsystem weblocks-navigation-widget
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("weblocks-navigation-widget/core"
               "weblocks-navigation-widget/changelog")
  :description "A container widget which switches between children widgets when user changes an url."
  :homepage "https://40ants.com/weblocks-navigation-widget/"
  :bug-tracker "https://github.com/40ants/weblocks-navigation-widget/issues"
  :source-control (:git "https://github.com/40ants/weblocks-navigation-widget")
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #P"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq)
                (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op weblocks-navigation-widget-test))))

