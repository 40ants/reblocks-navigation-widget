(defpackage #:weblocks-navigation-widget/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package weblocks-navigation-widget/changelog)


(defchangelog (:ignore-words ("URL"
                              "DEFWIDGET"
                              "WEBLOCKS/WIDGET:RENDER"
                              "DEBUG"
                              "ASDF"
                              "MAKE-NAVIGATION-WIDGET"
                              "WEBLOCKS/WIDGET:DEFWIDGET"
                              "WEBLOCKS/RESPONSE:ABORT-PROCESSING"
                              "WEBLOCKS/RESPONSE:IMMEDIATE-RESPONSE"
                              "WEBLOCKS-UI/CORE:WIDGET"
                              "WEBLOCKS-UI/CORE:UI-WIDGET")
               ;; :package #:weblocks-navigation-widget
               )
  (0.7.0 2021-10-19
         "* Removed function MAKE-NAVIGATION-WIDGET.
          * Exported symbols, related to the widget class.
          * Added documentation.")
  (0.6.1 2021-03-12
         "Moved to WEBLOCKS-UI/CORE:UI-WIDGET because WEBLOCKS-UI/CORE:WIDGET
          is deprecated and will be removed soon.")
  (0.6.0 2019-07-23
         "Renamed DEFWIDGET to WEBLOCKS-NAVIGATION-WIDGET:DEFROUTES, so that it is more explicit
          and it doesn't clash with WEBLOCKS/WIDGET:DEFWIDGET.")
  (0.5.0 2019-01-22
         "Function WEBLOCKS/RESPONSE:ABORT-PROCESSING was replaced with
          WEBLOCKS/RESPONSE:IMMEDIATE-RESPONSE to work with `Weblocks >= 0.35.0`.")
  (0.4.1 2018-11-25
         "Message \"Rendering navigation widget\" now logged with DEBUG level.")
  (0.4.0 2018-02-04
         "Move to package inferred ASDF class.")
  (0.3.0 2018-01-11
         "Added a DEFWIDGET macro, which defines a new class,
          based on navigation-widget, along with rules and a
          `make-<new-class>` function.

          This can be useful when you need navigation widget with
          customized WEBLOCKS/WIDGET:RENDER method or attached dependencies. ")
  (0.2.0 2018-01-07
         "Fixed to work with weblocks >= 0.22.0.")
  (0.1.1 2017-12-10
         "Now if no regex for URL, Weblocks will return 404.
          But you can redefine this behavior, providing a rule with `t`
          instead of the regex.")
  (0.1.0 2017-12-10
         "Basic functionality."))
