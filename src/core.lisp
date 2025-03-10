(uiop:define-package #:reblocks-navigation-widget
  (:nicknames #:reblocks-navigation-widget/core)
  (:use :cl)
  ;; just dependencies
  (:import-from #:log)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria)
  (:import-from #:reblocks/app)
  (:import-from #:reblocks/request)
  
  (:import-from #:reblocks/widget
                #:render)
  (:import-from #:reblocks/response
                #:immediate-response)
  (:import-from #:str
                #:trim-right)
  (:export #:defroutes
           #:navigation-widget
           #:current-widget
           #:current-path
           #:navigation-rules))
(in-package reblocks-navigation-widget)


(reblocks/widget:defwidget navigation-widget ()
  ((current-widget :initform nil
                   :reader current-widget)
   (current-path :initform nil
                 :reader current-path
                 :documentation "A whole path including the app's prefix.")
   (current-prefix :initform nil
                   :reader current-prefix
                   :documentation "A whole prefix including the rule matched to the current-widget.")
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
                                               (list
                                                (cond
                                                  ;; Here we allowing to pass a rule
                                                  ;; as list like (:prefix "/foo/bar")
                                                  ((typep (car rules)
                                                          'keyword)
                                                   (list rules))
                                                  ;; and otherwise it can be a list
                                                  ;; of separate paths:
                                                  ;; ("/foo" "/bar")
                                                  (t rules)))
                                               (t (list rules)))
                                 collect `(list ,(typecase rule
                                                   (cons
                                                    (destructuring-bind (rule-type rule-value)
                                                        rule
                                                      (case rule-type
                                                        ;; This is the case, when
                                                        ;; there will be a nested
                                                        ;; navigation widget and we
                                                        ;; need to match only by prefix,
                                                        ;; allowing other navigation widget
                                                        ;; to handle the rest of URI:
                                                        (:prefix
                                                         (format nil "^~A" rule-value))
                                                        (t
                                                         (error "Unsupported rule type: ~S"
                                                                rule-type))))
                                                    )
                                                   ;; Usual case when we don't want
                                                   ;; the rule match to any URLs starting
                                                   ;; from given text:
                                                   (string
                                                    (format nil "^~A$" rule))
                                                   (t rule))
                                                (lambda ()
                                                  ,@code))))))


(defmacro defroutes (class-name &body rules)
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

(declaim (ftype (function (list string) (values &optional function string))
                search-rule))

(defun search-rule (rules path)
  (loop for (rule-path func) in rules
        do (log:debug "Checking" rule-path "against" path)
           (when (eql rule-path t)
             ;; path can be not a string but just 't
             (return-from search-rule
               (values func
                       path)))
           (multiple-value-bind (start end)
               (cl-ppcre:scan rule-path path)
             (when (and start end)
               (return-from search-rule
                 (values func
                         (str:substring start end path))))))
  (values))


(defun get-new-widget-constructor (widget path)
  (search-rule (navigation-rules widget)
               path))


(defvar *current-prefix*)
(setf (documentation '*current-prefix* 'variable)
      "This var will be set during the RENDER method execution and will be equal to the path
       of currently active rule. This allows to process nested navigation widgets correctly.
       ")


(defun cut-prefix (path prefix)
  (cond
    ((str:starts-with-p prefix path)
     (str:substring (length prefix) nil path))
    (t
     path)))


(defun join-prefix (prefix path)
  (concatenate 'string prefix path))


(defmethod render ((widget navigation-widget))
  (with-slots (current-widget current-path current-prefix)
      widget
    
    (let ((previous-path current-path)
          (whole-path (reblocks/request:get-path)))
      (unless (equal previous-path
                     whole-path)
        ;; Create a new widget or switch to existing one
        ;; if path was changed
        (let* ((new-current-prefix
                 (if (boundp '*current-prefix*)
                     *current-prefix*
                     (trim-right (reblocks/app:get-prefix reblocks/variables:*current-app*)
                                 ;; We need to remove / from the right,
                                 ;; because navigation rules already started with a backslash.
                                 :char-bag "/")))
               (path (cut-prefix whole-path new-current-prefix)))
          (multiple-value-bind (construct-new-widget matched-path)
              (get-new-widget-constructor widget path)
            (cond
              (construct-new-widget
               (setf current-widget (funcall construct-new-widget)
                     ;; Now we'll remember that path was changed
                     current-path whole-path
                     current-prefix (join-prefix new-current-prefix
                                                 matched-path)))
              (t
               ;; TODO: Make this behaviour configurable, probably for
               ;; some cases it would be useful to show some default
               ;; widget.
               (log:error "No widget constructor for path ~A" path)
               (immediate-response "Not found" :code 404)))))))

    (when current-widget
      (let ((*current-prefix* current-prefix))
        (render current-widget)))))
