# LIST-NAMED-CLASS

In ANSI Common Lisp, all classes must be named by symbols.

This system makes it possible to name classes by lists of symbols instead, using
only standard MOP wizardry.

## Prerequisites

Your implementation must **not** assume that class names must be symbols. In
particular, `CLASS-NAME` may return any Lisp data and `SETF CLASS-NAME` may
accept any Lisp data as the newly set value. See
http://metamodular.com/CLOS-MOP/class-name.html and
http://metamodular.com/CLOS-MOP/setf-class-name.htm

In practice: works on SBCL (see below for details), doesn't work on CCL.
Untested everywhere else.

## Example

```common-lisp
LIST-NAMED-CLASS> (defclass (foo) () ())
#<LIST-NAMED-CLASS (FOO)>

LIST-NAMED-CLASS> (find-class '(foo))
#<LIST-NAMED-CLASS (FOO)>

LIST-NAMED-CLASS> (make-instance '(foo))
#<(FOO) {10116348B3}>

LIST-NAMED-CLASS> (defclass (bar) () ((slot :initarg :slot :accessor slot)))
#<LIST-NAMED-CLASS (BAR)>

LIST-NAMED-CLASS> (make-instance '(bar))
#<(BAR) {1011907C63}>

LIST-NAMED-CLASS> (setf (slot *) 42)
42

LIST-NAMED-CLASS> (slot **)
42

LIST-NAMED-CLASS> (defclass (bar baz) () ())
#<LIST-NAMED-CLASS (BAR BAZ)>

LIST-NAMED-CLASS> (defgeneric foo (bar baz)
                    (:method ((bar (bar baz)) (baz (bar baz))) 42))
#<COMMON-LISP:STANDARD-GENERIC-FUNCTION LIST-NAMED-CLASS::FOO (1)>

LIST-NAMED-CLASS> (foo (make-instance '(bar baz)) (make-instance '(bar baz)))
42

LIST-NAMED-CLASS> (defmethod foo ((bar string) (baz (bar baz))) :nook)
#<STANDARD-METHOD LIST-NAMED-CLASS::FOO
  (STRING (CLASS #<LIST-NAMED-CLASS (BAR BAZ)>)) {10181D4FB3}>

LIST-NAMED-CLASS> (foo "a" (make-instance '(bar baz)))
:NOOK
```

## Usage

Package `LIST-NAMED-CLASS` contains symbols `DEFCLASS`, `DEFGENERIC`,
`DEFMETHOD`, `FIND-CLASS`, `MAKE-INSTANCE`, and `CHANGE-CLASS` that are meant to
be shadowing-imported into your package. The most convenient way to do so is to
use `UIOP:DEFINE-PACKAGE`'s `:MIX` option:

```
(uiop:define-package my-package
  (:mix #:list-named-class #:cl))
```

All classes named after lists are subclasses of class `LIST-NAMED-CLASS`.

All instances of classes named after lists are subclasses of class
`LIST-NAMED-INSTANCE`.

## SBCL patch

SBCL has a bug that prevents `DEFCLASS` with accessors in slot definitions from
working correctly.
See [this](https://bugs.launchpad.net/sbcl/+bug/1796568) for details.

Add the following to your SBCL init script to work around this bug for the time
being.

```common-lisp
#+sbcl
(in-package #:sb-pcl)

#+sbcl
(progn
  (defmethod add-reader-method ((class slot-class) generic-function slot-name slot-documentation source-location)
    (add-method generic-function
                (make-a-method 'standard-reader-method
                               ()
                               (list (let ((name (class-name class)))
                                       (if (and name (symbolp name)) name 'object)))
                               (list class)
                               (make-reader-method-function class slot-name)
                               (or slot-documentation "automatically generated reader method")
                               :slot-name slot-name
                               :object-class class
                               :method-class-function #'reader-method-class
                               'source source-location)))
  (defmethod add-writer-method ((class slot-class) generic-function slot-name slot-documentation source-location)
    (add-method generic-function
                (make-a-method 'standard-writer-method
                               ()
                               (list 'new-value
                                     (let ((name (class-name class)))
                                       (if (and name (symbolp name)) name 'object)))
                               (list *the-class-t* class)
                               (make-writer-method-function class slot-name)
                               (or slot-documentation "automatically generated writer method")
                               :slot-name slot-name
                               :object-class class
                               :method-class-function #'writer-method-class
                               'source source-location)))
  (defmethod add-boundp-method ((class slot-class) generic-function slot-name slot-documentation source-location)
    (add-method generic-function
                (make-a-method (constantly (find-class 'standard-boundp-method))
                               class
                               ()
                               (list (let ((name (class-name class)))
                                       (if (and name (symbolp name)) name 'object)))
                               (list class)
                               (make-boundp-method-function class slot-name)
                               (or slot-documentation "automatically generated boundp method")
                               :slot-name slot-name
                               'source source-location))))

```

## TODO

Automated tests. These will be handy when testing on new implementations.

## License

Copyright © 2018 Michał "phoe" Herda

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the “Software”), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.