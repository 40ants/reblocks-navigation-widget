(uiop:define-package #:reblocks-navigation-widget
  (:nicknames #:reblocks-navigation-widget/core)
  (:use :cl)
  ;; just dependencies
  (:import-from #:log4cl)
  (:import-from #:reblocks/request)
  
  (:import-from #:reblocks/widget
                #:render)
  (:import-from #:reblocks-ui/core
                #:ui-widget)
  (:import-from #:reblocks/response
                #:immediate-response)
  (:export #:defroutes
           #:navigation-widget
           #:current-widget
           #:current-path
           #:navigation-rules))
(in-package reblocks-navigation-widget)


(reblocks/widget:defwidget navigation-widget (ui-widget)
  ((current-widget :initform nil
                   :reader current-widget)
   (current-path :initform nil
                 :reader current-path)
   (rules :initarg :rules
          :reader navigation-rules))
  (:documentation "Base class for all navigation widgets.

                   When rendered it tracks if URL was changed and
                   creates a new child widget according to given navigation rules.

                   Usually you don't want to inherit from this class manually,
                   but instead use DEFROUTES macro."))


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


(defmacro defroutes (class-name &rest rules)
  "Defines a new class with name CLASS-NAME, inherited from NAVIGATION-WIDGET.

   And a function `make-{class-name}` to make instances of this class.

   Each entry in rules should be a list of two items. First item is a regex pattern to match URL path.
   Second item is a form to create a widget. A new widget will be created only if URL
   was changed.

   Here is an example of a widget with two rules:

   ```
   (defroutes tasks-routes
           (\"/tasks/\d+\" (make-task-page))
           (\"/tasks/\" (make-task-list \"Make my first Reblocks app\"
                                      \"Deploy it somewhere\"
                                      \"Have a profit\")))
   ```

   With these rules, when user opens URL `/tasks/` a widget returned by MAKE-TASK-LIST
   will be set as CURRENT-WIDGET and shown to the user. If user clicks on some task
   and URL change to `/tasks/100500`, then a widget for a task will be created by a call
   to MAKE-TASK-PAGE.

   **Pay attention** that widget creation form is responsible for extraction of the parameters
   from the URL. In above case, MAKE-TASK-PAGE should call REBLOCKS/REQUEST:GET-PATH and
   parse path to extract task's id. Probably this will change in future defroutes will catch
   matched path pieces.
"
  
  (let ((make-func-name (alexandria:symbolicate :make- class-name))
        (rules (make-lambda-rules rules)))
    `(progn (reblocks/widget:defwidget ,class-name (navigation-widget)
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
  (search-rule (navigation-rules widget)
               path))


(defmethod render ((widget navigation-widget))
  (log:debug "Rendering navigation widget")

  (with-slots (current-widget current-path)
      widget
    (let ((previous-path current-path)
          (path (reblocks/request:get-path)))
      (unless (equal previous-path
                     path)
        ;; Create a new widget or switch to existing one
        ;; if path was changed
        (let* ((construct-new-widget (get-new-widget-constructor widget path)))
          (if construct-new-widget
              (setf current-widget(funcall construct-new-widget)
                    ;; Now we'll remember that path was changed
                    current-path path)
              ;; TODO: Make this behaviour configurable
              (progn (log:error "No widget constructor for path ~A" path)
                     (immediate-response "Not found" :code 404))))))

    (when current-widget
      (render current-widget))))
