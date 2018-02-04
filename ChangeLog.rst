===========
 ChangeLog
===========

0.4.0 (2018-02-04)
==================

Move to package inferred ASDF class.

0.3.0 (2018-01-11)
==================

Added a ``defwidget`` macro, which defines a new class,
based on navigation-widget, along with rules and a
``make-<new-class>`` function.

This can be useful when you need navigation widget with
customized ``render`` method or attached dependencies.

0.2.0 (2018-01-07)
==================

* Fixed to work with weblocks >= 0.22.0.

0.1.1 (2017-12-10)
==================

* Now if no regex for URL, weblocks will return 404.
  But you can redefine this behavior, providing a rule with ``t``
  instead of the regex.


0.1.0 (2017-12-10)
==================

* Basic functionality.
