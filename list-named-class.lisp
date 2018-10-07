;;;; list-named-class.lisp

(uiop:define-package #:list-named-class
  (:mix #:closer-mop #:common-lisp #:alexandria)
  (:shadow #:defclass #:defmethod #:defgeneric
           #:find-class #:change-class #:make-instance)
  (:export #:defclass #:defmethod #:defgeneric
           #:find-class #:change-class #:make-instance
           #:list-named-class #:list-named-instance))

(in-package #:list-named-class)

(cl:defclass list-named-object () ())

(cl:defmethod print-object ((object list-named-object) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (handler-case (prin1 (class-name (class-of object)) stream)
      (error () (princ "<error while printing>" stream)))))

(cl:defclass list-named-class (list-named-object standard-class) ())

(cl:defmethod print-object ((object list-named-class) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (handler-case (prin1 (class-name object) stream)
      (error () (princ "<error while printing>" stream)))))

(cl:defclass list-named-instance () ())

(cl:defmethod print-object ((object list-named-instance) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (handler-case (prin1 (class-name (class-of object)) stream)
      (error () (princ "<error while printing>" stream)))))

(cl:defmethod validate-superclass ((c list-named-class) (s standard-class)) t)

(defvar *list-named-classes* (make-hash-table :test #'equal))

(defun find-list-named-class (name &optional (errorp t))
  (assert (and (consp name) (proper-list-p name) (every #'symbolp name)) (name)
          "NAME must be a non-empty list of symbols, not ~S." name)
  (let ((result (gethash name *list-named-classes*)))
    (when (and errorp (null result))
      (error "Class named ~S was not found." name))
    result))

(defun (setf find-list-named-class) (new-value name &optional (errorp t))
  (declare (ignore errorp))
  (assert (and (consp name) (proper-list-p name) (every #'symbolp name)) (name)
          "NAME must be a non-empty list of symbols, not ~S." name)
  (assert (typep new-value '(or class null)) (new-value)
          "NEW-VALUE must be a class metaobject or NIL, not ~S." new-value)
  (if new-value
      (setf (gethash name *list-named-classes*) new-value)
      (remhash name *list-named-classes*)))

(defun find-class (name &optional (errorp t) environment)
  (if (symbolp name)
      (cl:find-class name errorp environment)
      (find-list-named-class name errorp)))

(defun (setf find-class) (new-value name &optional (errorp t) environment)
  (if (symbolp name)
      (setf (cl:find-class name errorp environment) new-value)
      (setf (find-list-named-class name errorp) new-value)))

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (if (consp name)
      (let ((gensym (gensym)))
        `(unwind-protect
              (progn (setf (find-class ',gensym)
                           (find-list-named-class ',name nil))
                     (cl:defclass ,gensym (,@direct-superclasses
                                           list-named-instance)
                       ,direct-slots
                       (:metaclass list-named-class)
                       ,@options)
                     (setf (find-list-named-class ',name) (find-class ',gensym))
                     (find-class ',gensym))
           (let ((class (find-class ',gensym)))
             (setf (find-class ',gensym nil) nil)
             (setf (class-name class) ',name))))
      `(cl:defclass ,name ,direct-superclasses ,direct-slots ,@options)))

(defun %list-named-types (lambda-list)
  (let* ((lambda-list (copy-tree lambda-list))
         (alist (loop with gensym = (gensym)
                      for elt in lambda-list
                      for x = (when (consp elt) (cadr elt))
                      until (member elt lambda-list-keywords)
                      if (and (consp x) (not (eql (car x) 'cl:eql)))
                        collect (cons gensym x)
                        and do (setf (cdr elt) (list gensym)
                                     gensym (gensym)))))
    (values lambda-list alist)))

(defmacro defmethod (name &rest args)
  (let* ((args (copy-list args))
         (qualifiers (nreverse (loop for arg = (car args) until (listp arg)
                                     collect (pop args)))))
    (destructuring-bind (lambda-list . body) args
      (multiple-value-bind (lambda-list alist) (%list-named-types lambda-list)
        (if alist
            `(unwind-protect
                  (progn (setf ,@(mappend (lambda (x)
                                            `((find-class ',(car x))
                                              (find-list-named-class ',(cdr x))))
                                          alist))
                         (cl:defmethod ,name ,@qualifiers ,lambda-list ,@body))
               (setf ,@(mappend (lambda (x) `((find-class ',(car x)) nil))
                                alist)))
            `(cl:defmethod ,name ,@qualifiers ,@args))))))

(cl:defgeneric make-instance (class &rest initargs &key &allow-other-keys)
  (:method ((class symbol) &rest initargs &key &allow-other-keys)
    (apply #'cl:make-instance (find-class class) initargs))
  (:method ((class cons) &rest initargs &key &allow-other-keys)
    (apply #'cl:make-instance (find-class class) initargs))
  (:method ((class class) &rest initargs &key &allow-other-keys)
    (apply #'cl:make-instance class initargs)))

(cl:defgeneric change-class
    (instance new-class &rest initargs &key &allow-other-keys)
  (:method (instance (new-class symbol) &rest initargs &key &allow-other-keys)
    (apply #'cl:change-class instance (find-class new-class) initargs))
  (:method (instance (new-class cons) &rest initargs &key &allow-other-keys)
    (apply #'cl:change-class instance (find-class new-class) initargs))
  (:method ((instance standard-object) (new-class class)
            &rest initargs &key &allow-other-keys)
    (apply #'cl:change-class instance new-class initargs)))

(defmacro defgeneric (fun-name lambda-list &rest options)
  (let* ((options (copy-tree options))
         (predicate (lambda (x) (eql (car x) :method)))
         (methods (remove-if-not predicate options))
         (non-methods (remove-if predicate options))
         (alist '()))
    (dolist (method methods)
      (loop for arg = (car method) until (listp arg) do (pop method))
      (multiple-value-bind (new-lambda-list new-alist)
          (%list-named-types (car method))
        (setf (car method) new-lambda-list)
        (nconcf alist new-alist)))
    (if alist
        `(unwind-protect
              (progn
                (setf ,@(mappend (lambda (x)
                                   `((find-class ',(car x))
                                     (find-list-named-class ',(cdr x))))
                                 alist))
                (cl:defgeneric ,fun-name ,lambda-list ,@non-methods ,@methods))
           (setf ,@(mappend (lambda (x) `((find-class ',(car x)) nil))
                            alist)))
        `(cl:defgeneric ,fun-name ,lambda-list ,@options))))
