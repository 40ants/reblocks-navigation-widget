(defpackage weblocks-navigation-widget
  (:use :cl)
  (:export
   #:make-navigation-widget))
(in-package :weblocks-navigation-widget)


(weblocks.widget:defwidget navigation-widget (weblocks.ui.core:widget)
  ((current-widget :initform nil
                   :accessor get-current-widget)
   (path :initform nil
         :accessor get-path)
   (rules :initarg :rules
          :reader get-rules)))


(defun make-lambda-rules (rules)
  "Transforms a list of lists like that


  '(((\"/\"
      \"/s/.*\")
     (make-my-story-widget ...)
    (\"/about\" (make-about-page...)))

  into a similar list, but wrapping tail of each sublist in
  (lambda () ...):

  '((\"/\"
     (lambda ()
        (make-my-story-widget ...)))
    (\"/s/.*\"
     (lambda ()
        (make-my-story-widget ...)))
    (\"/about\"
     (lambda ()
        (make-about-page...)))

"
  `(list ,@(loop for (rules . code) in rules
                 appending (loop for rule in (typecase rules
                                               (list rules)
                                               (t (list rules)))
                                 collect `(list ,(format nil "^~A$" rule)
                                                (lambda ()
                                                  ,@code))))))

(defmacro make-navigation-widget (&rest rules)
  `(make-instance 'navigation-widget
                  :rules ,(make-lambda-rules rules)))


(defun search-rule (rules path)
  (loop for (rule-path func) in rules
        do (log:debug "Checking" rule-path "against" path)
        when (cl-ppcre:scan rule-path path)
          return func))


(defun get-new-widget-constructor (widget path)
  (search-rule (get-rules widget)
               path))


(defmethod weblocks.widget:render ((widget navigation-widget))
  (log:info "Rendering navigation widget")

  (let ((previous-path (get-path widget))
        (path (weblocks.request:request-path-info)))
    (unless (equal previous-path
                   path)
      ;; Create a new widget or switch to existing one
      ;; if path was changed
      (let* ((construct-new-widget (get-new-widget-constructor widget path)))
        (if construct-new-widget
            (setf (get-current-widget widget) (funcall construct-new-widget)
                  ;; Now we'll remember that path was changed
                  (get-path widget) path)
            (error "No widget constructor for path ~A" path)))))

  (when (get-current-widget widget)
    (weblocks.widget:render-widget
     (get-current-widget widget))))
