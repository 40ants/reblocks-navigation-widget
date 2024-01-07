<a id="x-28REBLOCKS-NAVIGATION-WIDGET-DOCS-2FINDEX-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Reblocks Navigation Widget

<a id="reblocks-navigation-widget-asdf-system-details"></a>

## REBLOCKS-NAVIGATION-WIDGET ASDF System Details

* Description: A container widget which switches between children widgets when user changes an url.
* Licence: Unlicense
* Author: Alexander Artemenko <svetlyak.40wt@gmail.com>
* Homepage: [https://40ants.com/reblocks-navigation-widget/][1f1d]
* Bug tracker: [https://github.com/40ants/reblocks-navigation-widget/issues][c83a]
* Source control: [GIT][a78a]
* Depends on: [log4cl][7f8b], [reblocks][184b]

[![](https://github-actions.40ants.com/40ants/reblocks-navigation-widget/matrix.svg?only=ci.run-tests)][7c86]

![](http://quickdocs.org/badge/reblocks-navigation-widget.svg)

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-DOCS-2FINDEX-3A-3A-40INSTALLATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Installation

You can install this library from Quicklisp, but you want to receive updates quickly, then install it from Ultralisp.org:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload :reblocks-navigation-widget)
```
<a id="x-28REBLOCKS-NAVIGATION-WIDGET-DOCS-2FINDEX-3A-3A-40USAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Usage

This addon for Reblocks frameworks allows you to define which widgets should be
shown to the user dependening on `URL`'s path.

The main entry-point is [`defroutes`][5f0d] macro. Use it to define a subclass of
navigation widget and then return this widget from the session initialization
method of your Reblocks application.

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-DOCS-2FINDEX-3A-3A-40NESTED-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Nested routes

Starting from version 0.9.0, the library supports nested routes.
This allows you to combine routes defined in separate systems.

For example, lets pretend we have a separate `ASDF` system for user management.
It defines it's routes like this:

```lisp
(defroutes users
  ("/" (make-user-list))
  ("/add" (make-new-user-form)))
```
And now, if we want to incorporate this user management app into our app having
it's own routing, we can do this by using a prefix rule:

```lisp
(defroutes main
  ;; a subroutes
  ((:prefix "/users") (make-users))
  ;; a usual widget
  ("/stats" (make-stats)))
```
In it's turn, `MAIN` router could be used in app having it's own `PREFIX` `/admin`. Here is how
this nested setup will be processed by navigation widgets.

When user opens a page with `URI` `/admin/users/add`, then `MAIN` widget will be rendered
first. It will see the app has `/admin` prefix and will cut it from the `URI`. Then `MAIN` routes
widget will check `/users/add` against it's rules and will create `USERS` widget. `RENDER` method
of `USERS` widget will be called with `*current-path*` set to `/admin/users`. In it's turn,
`USERS` widget will cut `/admin/users` prefix from the whole `URI` `/admin/users/add` and will check
`/add` against it's rules to find out it need to create a `NEW-USER-FORM`.

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-DOCS-2FINDEX-3A-3A-40API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## API

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-3ADEFROUTES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro](001b) `reblocks-navigation-widget:defroutes` class-name &body rules

Defines a new class with name `CLASS-NAME`, inherited from [`navigation-widget`][9fc2].

And a function `make-{class-name}` to make instances of this class.

Each entry in rules should be a list of two items. First item is a regex pattern to match `URL` path.
Second item is a form to create a widget. A new widget will be created only if `URL`
was changed.

Here is an example of a widget with two rules:

```
(defroutes tasks-routes
        ("/tasks/d+" (make-task-page))
        ("/tasks/" (make-task-list "Make my first Reblocks app"
                                   "Deploy it somewhere"
                                   "Have a profit")))
```
With these rules, when user opens `URL` `/tasks/` a widget returned by `MAKE-TASK-LIST`
will be set as [`current-widget`][2e97] and shown to the user. If user clicks on some task
and `URL` change to `/tasks/100500`, then a widget for a task will be created by a call
to `MAKE-TASK-PAGE`.

**Pay attention** that widget creation form is responsible for extraction of the parameters
from the `URL`. In above case, `MAKE-TASK-PAGE` should call `REBLOCKS/REQUEST:GET-PATH` and
parse path to extract task's id. Probably this will change in future defroutes will catch
matched path pieces.

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-3ANAVIGATION-WIDGET-20CLASS-29"></a>

### [class](2d98) `reblocks-navigation-widget:navigation-widget` (widget)

Base class for all navigation widgets.

When rendered it tracks if `URL` was changed and
creates a new child widget according to given navigation rules.

Usually you don't want to inherit from this class manually,
but instead use [`defroutes`][5f0d] macro.

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-3ACURRENT-WIDGET-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-NAVIGATION-WIDGET-3ANAVIGATION-WIDGET-29-29"></a>

### [reader](c7ce) `reblocks-navigation-widget:current-widget` (navigation-widget) (= nil)

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-3ACURRENT-PATH-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-NAVIGATION-WIDGET-3ANAVIGATION-WIDGET-29-29"></a>

### [reader](d429) `reblocks-navigation-widget:current-path` (navigation-widget) (= nil)

A whole path including the app's prefix.

<a id="x-28REBLOCKS-NAVIGATION-WIDGET-3ANAVIGATION-RULES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-NAVIGATION-WIDGET-3ANAVIGATION-WIDGET-29-29"></a>

### [reader](0b59) `reblocks-navigation-widget:navigation-rules` (navigation-widget) (:rules)


[1f1d]: https://40ants.com/reblocks-navigation-widget/
[2e97]: https://40ants.com/reblocks-navigation-widget/#x-28REBLOCKS-NAVIGATION-WIDGET-3ACURRENT-WIDGET-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20REBLOCKS-NAVIGATION-WIDGET-3ANAVIGATION-WIDGET-29-29
[5f0d]: https://40ants.com/reblocks-navigation-widget/#x-28REBLOCKS-NAVIGATION-WIDGET-3ADEFROUTES-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[9fc2]: https://40ants.com/reblocks-navigation-widget/#x-28REBLOCKS-NAVIGATION-WIDGET-3ANAVIGATION-WIDGET-20CLASS-29
[a78a]: https://github.com/40ants/reblocks-navigation-widget
[7c86]: https://github.com/40ants/reblocks-navigation-widget/actions
[001b]: https://github.com/40ants/reblocks-navigation-widget/blob/9af0c42e7ad88ca70ece26dc907c42c398d046d8/src/core.lisp#L104
[2d98]: https://github.com/40ants/reblocks-navigation-widget/blob/9af0c42e7ad88ca70ece26dc907c42c398d046d8/src/core.lisp#L20
[c7ce]: https://github.com/40ants/reblocks-navigation-widget/blob/9af0c42e7ad88ca70ece26dc907c42c398d046d8/src/core.lisp#L21
[d429]: https://github.com/40ants/reblocks-navigation-widget/blob/9af0c42e7ad88ca70ece26dc907c42c398d046d8/src/core.lisp#L23
[0b59]: https://github.com/40ants/reblocks-navigation-widget/blob/9af0c42e7ad88ca70ece26dc907c42c398d046d8/src/core.lisp#L29
[c83a]: https://github.com/40ants/reblocks-navigation-widget/issues
[7f8b]: https://quickdocs.org/log4cl
[184b]: https://quickdocs.org/reblocks

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]
