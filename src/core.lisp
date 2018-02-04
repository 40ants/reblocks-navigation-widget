(defpackage weblocks-navigation-widget/core
  (:nicknames #:weblocks-navigation-widget)
  (:use :cl)
  ;; just dependencies
  (:import-from #:log4cl)
  (:import-from #:weblocks/request)
  
  (:import-from #:weblocks/widget
                #:render)
  (:import-from #:weblocks-ui/core
                #:widget)
  (:import-from #:weblocks/response
                #:abort-processing)
  (:export
   #:make-navigation-widget
   #:defwidget))
(in-package weblocks-navigation-widget)


(weblocks/widget:defwidget navigation-widget (widget)
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
                                 collect `(list ,(typecase rule
                                                           (string (format nil "^~A$" rule))
                                                           (t rule))
                                                (lambda ()
                                                  ,@code))))))


(defmacro make-navigation-widget (&rest rules)
  `(make-instance 'navigation-widget
                  :rules ,(make-lambda-rules rules)))


(defmacro defwidget (class-name &rest rules)
  "Defines a new class with name <class-name>, inherited from `navigation-widget'.

   And a function `make-<class-name>' to make instances of this class."
  
  (let ((make-func-name (alexandria:symbolicate :make- class-name))
        (rules (make-lambda-rules rules)))
    `(progn (weblocks/widget:defwidget ,class-name (navigation-widget)
              ())
            
            (defun ,make-func-name (&rest args)
              (apply #'make-instance
                     ',class-name
                     :rules ,rules
                     args)))))


(defun search-rule (rules path)
  (loop for (rule-path func) in rules
        do (log:debug "Checking" rule-path "against" path)
        when (or (eql rule-path t) ;; path can be not a string but just 't
                 (cl-ppcre:scan rule-path path))
          return func))


(defun get-new-widget-constructor (widget path)
  (search-rule (get-rules widget)
               path))


(defmethod render ((widget navigation-widget))
  (log:info "Rendering navigation widget")

  (let ((previous-path (get-path widget))
        (path (weblocks/request:get-path)))
    (unless (equal previous-path
                   path)
      ;; Create a new widget or switch to existing one
      ;; if path was changed
      (let* ((construct-new-widget (get-new-widget-constructor widget path)))
        (if construct-new-widget
            (setf (get-current-widget widget) (funcall construct-new-widget)
                  ;; Now we'll remember that path was changed
                  (get-path widget) path)
            ;; TODO: Make this behaviour configurable
            (progn (log:error "No widget constructor for path ~A" path)
                   (abort-processing "Not found" :code 404))))))

  (when (get-current-widget widget)
    (render
     (get-current-widget widget))))
