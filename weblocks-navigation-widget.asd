#|
  This file is a part of weblocks-navigation-widget project.
|#

#|
  A container widget which switches between children widgets when user changes an url.
|#


(in-package :cl-user)
(defpackage weblocks-navigation-widget-asd
  (:use :cl :asdf))
(in-package :weblocks-navigation-widget-asd)


(defsystem weblocks-navigation-widget
  :version (:read-file-form "version.lisp-expr")
  :author ""
  :license ""
  :depends-on (weblocks-ui)
  :components ((:module "src"
                :components
                ((:file "weblocks-navigation-widget"))))
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

