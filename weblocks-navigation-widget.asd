(defsystem weblocks-navigation-widget
  :version (:read-file-form "version.lisp-expr")
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "BSD"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("weblocks-navigation-widget/core"
               "weblocks-navigation-widget/changelog")
  :description "A container widget which switches between children widgets when user changes an url."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.rst"
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

