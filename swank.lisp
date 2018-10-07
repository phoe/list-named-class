;;;; swank.lisp
!
#+swank
(in-package #:swank)

#+swank
(defun class-from-class-name-form* (class-name-form)
  (when (and (listp class-name-form)
             (every #'symbolp class-name-form))
    (let* ((class-name class-name-form)
           (class (list-named-class:find-class class-name nil)))
      (when (and class
                 (not (swank-mop:class-finalized-p class)))
        ;; Try to finalize the class, which can fail if
        ;; superclasses are not defined yet
        (ignore-errors (swank-mop:finalize-inheritance class)))
      class)))

#+swank
(defun extra-keywords/make-instance-list-named (operator args)
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form* class-name-form)))
      (if class
          (multiple-value-bind (slot-init-keywords class-aokp)
              (extra-keywords/slots class)
            (multiple-value-bind (allocate-instance-keywords ai-aokp)
                (applicable-methods-keywords
                 #'allocate-instance (list class))
              (multiple-value-bind (initialize-instance-keywords ii-aokp)
                  (ignore-errors
                   (applicable-methods-keywords
                    #'initialize-instance
                    (list (swank-mop:class-prototype class))))
                (multiple-value-bind (shared-initialize-keywords si-aokp)
                    (ignore-errors
                     (applicable-methods-keywords
                      #'shared-initialize
                      (list (swank-mop:class-prototype class) t)))
                  (values (append slot-init-keywords
                                  allocate-instance-keywords
                                  initialize-instance-keywords
                                  shared-initialize-keywords)
                          (or class-aokp ai-aokp ii-aokp si-aokp)
                          (list (list 'quote class-name-form)))))))
          (extra-keywords/make-instance operator args)))))

#+swank
(defmethod extra-keywords ((operator (eql 'list-named-class:make-instance))
                           args)
  (multiple-value-or (extra-keywords/make-instance-list-named operator args)
                     (call-next-method)))

#+swank
(defun extra-keywords/change-class* (operator args)
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form* class-name-form)))
      (if class
          (multiple-value-bind (slot-init-keywords class-aokp)
              (extra-keywords/slots class)
            (declare (ignore class-aokp))
            (multiple-value-bind (shared-initialize-keywords si-aokp)
                (ignore-errors
                 (applicable-methods-keywords
                  #'shared-initialize
                  (list (swank-mop:class-prototype class) t)))
              ;; FIXME: much as it would be nice to include the
              ;; applicable keywords from
              ;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, I don't really see
              ;; how to do it: so we punt, always declaring
              ;; &ALLOW-OTHER-KEYS.
              (declare (ignore si-aokp))
              (values (append slot-init-keywords shared-initialize-keywords)
                      t
                      (list class-name-form))))
          (extra-keywords/change-class operator args)))))

#+swank
(defmethod extra-keywords ((operator (eql 'list-named-class:change-class))
                           args)
  (multiple-value-bind (keywords aok determiners)
      (extra-keywords/change-class* operator (cdr args))
    (if keywords
        (values keywords aok
                (cons (car args) determiners))
        (call-next-method))))
