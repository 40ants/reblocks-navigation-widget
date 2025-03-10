(uiop:define-package #:reblocks-navigation-widget-docs/index
  (:use #:cl)
  (:import-from #:pythonic-string-reader
                #:pythonic-string-syntax)
  #+quicklisp
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:40ants-doc
                #:defsection
                #:defsection-copy)
  (:import-from #:reblocks-navigation-widget
                #:navigation-rules
                #:current-path
                #:current-widget
                #:navigation-widget
                #:defroutes)
  (:import-from #:reblocks-navigation-widget-docs/changelog
                #:@changelog)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@index
           #:@readme
           #:@changelog))
(in-package #:reblocks-navigation-widget-docs/index)

(in-readtable pythonic-string-syntax)


(defmethod docs-config ((system (eql (asdf:find-system "reblocks-navigation-widget-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  #-quicklisp
  (asdf:load-system :40ants-doc-theme-40ants)
  
  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))


(defsection @index (:title "Reblocks Navigation Widget"
                    :ignore-words ("URL"
                                   "BSD"
                                   "GIT"
                                   "REBLOCKS/REQUEST:GET-PATH"
                                   "MAKE-TASK-PAGE"
                                   "ASDF:PACKAGE-INFERRED-SYSTEM"
                                   "ASDF"
                                   "40A"
                                   "MAKE-TASK-LIST"))
  (reblocks-navigation-widget system)
  "
[![](https://github-actions.40ants.com/40ants/reblocks-navigation-widget/matrix.svg?only=ci.run-tests)](https://github.com/40ants/reblocks-navigation-widget/actions)

![Quicklisp](http://quickdocs.org/badge/reblocks-navigation-widget.svg)
"
  (@installation section)
  (@usage section)
  (@nested section)
  (@api section))


(defsection-copy @readme @index)


(defsection @installation (:title "Installation")
  """
You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :reblocks-navigation-widget)
```
""")


(defsection @usage (:title "Usage")
  "This addon for Reblocks frameworks allows you to define which widgets should be
   shown to the user dependening on URL's path.

   The main entry-point is DEFROUTES macro. Use it to define a subclass of
   navigation widget and then return this widget from the session initialization
   method of your Reblocks application.
   ")


(defsection @nested (:title "Nested routes"
                     :ignore-words ("PREFIX"
                                    "MAIN"
                                    "RENDER"
                                    "URI"
                                    "USERS"
                                    "NEW-USER-FORM"
                                    "ASDF"))
  "Starting from version 0.9.0, the library supports nested routes.
   This allows you to combine routes defined in separate systems.
  
   For example, lets pretend we have a separate ASDF system for user management.
   It defines it's routes like this:

   ```lisp
   (defroutes users
     (\"/\" (make-user-list))
     (\"/add\" (make-new-user-form)))
   ```

   And now, if we want to incorporate this user management app into our app having
   it's own routing, we can do this by using a prefix rule:

   ```lisp
   (defroutes main
     ;; a subroutes
     ((:prefix \"/users\") (make-users))
     ;; a usual widget
     (\"/stats\" (make-stats)))
   ```

   In it's turn, MAIN router could be used in app having it's own PREFIX `/admin`. Here is how
   this nested setup will be processed by navigation widgets.

   When user opens a page with URI `/admin/users/add`, then MAIN widget will be rendered
   first. It will see the app has `/admin` prefix and will cut it from the URI. Then MAIN routes
   widget will check `/users/add` against it's rules and will create USERS widget. RENDER method
   of USERS widget will be called with `*current-path*` set to `/admin/users`. In it's turn,
   USERS widget will cut `/admin/users` prefix from the whole URI `/admin/users/add` and will check
   `/add` against it's rules to find out it need to create a NEW-USER-FORM.")


(defsection @api (:title "API")
  (defroutes macro)
  (navigation-widget class)
  (current-widget (reader navigation-widget))
  (current-path (reader navigation-widget))
  (navigation-rules (reader navigation-widget)))


