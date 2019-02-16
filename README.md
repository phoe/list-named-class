# LIST-NAMED-CLASS

In ANSI Common Lisp, all classes must be named by symbols.

This system makes it possible to name classes by lists of symbols instead, using
only standard MOP wizardry.

Tested on SBCL, ECL and CCL.

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

LIST-NAMED-CLASS> (defclass (parent) () ())
#<LIST-NAMED-CLASS (PARENT)>

LIST-NAMED-CLASS> (defclass (child) ((parent)) ())
#<LIST-NAMED-CLASS (CHILD)>

LIST-NAMED-CLASS> (defclass parent-2 () ())
#<STANDARD-CLASS LIST-NAMED-CLASS::PARENT-2>

LIST-NAMED-CLASS> (defclass child-2 ((parent) parent-2) ())
#<STANDARD-CLASS CHILD-2>
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
