(defpackage #:reblocks-navigation-widget/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package reblocks-navigation-widget/changelog)


(defchangelog (:ignore-words ("URL"
                              "DEFWIDGET"
                              "REBLOCKS/WIDGET:RENDER"
                              "DEBUG"
                              "ASDF"
                              "MAKE-NAVIGATION-WIDGET"
                              "REBLOCKS/WIDGET:DEFWIDGET"
                              "REBLOCKS/RESPONSE:ABORT-PROCESSING"
                              "REBLOCKS/RESPONSE:IMMEDIATE-RESPONSE"
                              "REBLOCKS-UI/CORE:WIDGET"
                              "REBLOCKS-UI/CORE:UI-WIDGET")
               ;; :package #:reblocks-navigation-widget
               )
  (0.7.0 2021-10-19
         "* Removed function MAKE-NAVIGATION-WIDGET.
          * Exported symbols, related to the widget class.
          * Added documentation.")
  (0.6.1 2021-03-12
         "Moved to REBLOCKS-UI/CORE:UI-WIDGET because REBLOCKS-UI/CORE:WIDGET
          is deprecated and will be removed soon.")
  (0.6.0 2019-07-23
         "Renamed DEFWIDGET to REBLOCKS-NAVIGATION-WIDGET:DEFROUTES, so that it is more explicit
          and it doesn't clash with REBLOCKS/WIDGET:DEFWIDGET.")
  (0.5.0 2019-01-22
         "Function REBLOCKS/RESPONSE:ABORT-PROCESSING was replaced with
          REBLOCKS/RESPONSE:IMMEDIATE-RESPONSE to work with `Reblocks >= 0.35.0`.")
  (0.4.1 2018-11-25
         "Message \"Rendering navigation widget\" now logged with DEBUG level.")
  (0.4.0 2018-02-04
         "Move to package inferred ASDF class.")
  (0.3.0 2018-01-11
         "Added a DEFWIDGET macro, which defines a new class,
          based on navigation-widget, along with rules and a
          `make-<new-class>` function.

          This can be useful when you need navigation widget with
          customized REBLOCKS/WIDGET:RENDER method or attached dependencies. ")
  (0.2.0 2018-01-07
         "Fixed to work with reblocks >= 0.22.0.")
  (0.1.1 2017-12-10
         "Now if no regex for URL, Reblocks will return 404.
          But you can redefine this behavior, providing a rule with `t`
          instead of the regex.")
  (0.1.0 2017-12-10
         "Basic functionality."))
