;;; test.lisp

(uiop:define-package #:list-named-class/test
    (:mix #:list-named-class
          #:cl
          #:1am)
  (:shadow #:test #:run)
  (:export #:run))

(in-package #:list-named-class/test)

(defvar *list-named-class-tests* '())

(defun run ()
  (1am:run *list-named-class-tests*))

(defmacro define-test (name &body body)
  `(let ((1am:*tests* '()))
     (1am:test ,name ,@body)
     (pushnew ',name *list-named-class-tests*)))

(define-test test-define
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (is (find-class '#1#))
              (is (equal (class-name (find-class '#1#)) '#1#)))
    (setf (find-class '#1#) nil)
    (is (not (find-class '#1# nil)))))

(define-test test-make-instance
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (let ((instance (make-instance '#1#)))
                (is (eq (class-of instance) (find-class '#1#)))))
    (setf (find-class '#1#) nil)))

(define-test test-change-class-1
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defclass #2=#:baz () ())
              (let ((instance (make-instance '#1#)))
                (change-class instance '#2#)
                (is (eq (class-of instance) (find-class '#2#)))))
    (setf (find-class '#1#) nil)
    (setf (find-class '#2#) nil)))

(define-test test-change-class-2
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defclass #2='#:baz () ())
              (let ((instance (make-instance '#2#)))
                (change-class instance '#1#)
                (is (eq (class-of instance) (find-class '#1#)))))
    (setf (find-class '#1#) nil)
    (setf (find-class '#2#) nil)))

(define-test test-defgeneric-1
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defgeneric frob (x)
                (:method (x) (declare (ignore x)) 42))
              (is (= (eval '(frob (make-instance '#1#))) 42)))
    (setf (find-class '#1#) nil)
    (fmakunbound 'frob)))

(define-test test-defgeneric-2
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defgeneric frob (x)
                (:method ((x #1#)) (declare (ignore x)) 42))
              (is (= (eval '(frob (make-instance '#1#))) 42)))
    (setf (find-class '#1#) nil)
    (fmakunbound 'frob)))

(define-test test-defmethod-1
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defgeneric frob (x))
              (defmethod frob (x) (declare (ignore x)) 42)
              (is (= (eval '(frob (make-instance '#1#))) 42)))
    (setf (find-class '#1#) nil)
    (fmakunbound 'frob)))

(define-test test-defmethod-2
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defgeneric frob (x))
              (defmethod frob ((x #1#)) (declare (ignore x)) 42)
              (is (= (eval '(frob (make-instance '#1#))) 42)))
    (setf (find-class '#1#) nil)
    (fmakunbound 'frob)))

(define-test test-defmethod-3
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defclass #2=#:baz () ())
              (defgeneric frob (x y))
              (defmethod frob ((x #1#) (y #2#)) (declare (ignore x)) 42)
              (is (= (eval '(frob (make-instance '#1#) (make-instance '#2#)))
                     42)))
    (setf (find-class '#1#) nil)
    (setf (find-class '#2#) nil)
    (fmakunbound 'frob)))

(define-test test-defmethod-4
  (unwind-protect
       (progn (defclass #1=(#:foo #:bar) () ())
              (defclass #2=#:baz () ())
              (defgeneric frob (x y))
              (defmethod frob ((x #2#) (y #1#)) (declare (ignore x)) 42)
              (is (= (eval '(frob (make-instance '#2#) (make-instance '#1#)))
                     42)))
    (setf (find-class '#1#) nil)
    (setf (find-class '#2#) nil)
    (fmakunbound 'frob)))
