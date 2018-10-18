;;;; protest.lisp

(in-package #:list-named-class)

(export 'define-protocol-class)

(defmacro define-protocol-class
    (name direct-superclasses direct-slots &rest options)
  (if (or (consp name) (some #'consp direct-superclasses))
      (let ((gensym (gensym)))
        (multiple-value-bind (superclasses alist)
            (%defclass-superclasses direct-superclasses)
          `(unwind-protect
                (progn (setf (find-class ',gensym)
                             (find-class ',name nil)
                             ,@(mappend (lambda (x)
                                          `((find-class ',(car x))
                                            (find-class ',(cdr x))))
                                        alist))
                       (protest/base:define-protocol-class ,gensym
                           (,@superclasses
                            ,@(when (consp name)
                                `(list-named-instance)))
                         ,direct-slots
                         ,@(when (consp name)
                             `(,(or (find :metaclass options :key #'car)
                                    '(:metaclass list-named-class))))
                         ,@(remove :metaclass options :key #'car))
                       (setf (find-class ',name) (find-class ',gensym))
                       (find-class ',gensym))
             (let ((class (find-class ',gensym)))
               (setf (find-class ',gensym) nil
                     ,@(mappend (lambda (x) `((find-class ',(car x)) nil))
                                alist)
                     (class-name class) ',name)))))
      `(protest/base:define-protocol-class ,name
         ,direct-superclasses ,direct-slots ,@options)))
