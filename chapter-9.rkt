; chapter 9 code of essentials of programming languages book EPL
#lang racket
; Object & Classes, OOP

; ExpVal = Int | Bool | Proc | Listof(ExpVal) | Obj
; DenVal = Ref(ExpVal)

; when interpretation starts, we have `initialize-class-env` that sets an
; environment that refers to all class mathods and variables

;;; Object Representation
;; 1. class name 2. a list of references to its fields
;; 3. method calls are like Python where you pass self as argument

(define-datatype object object? 
    (an-object
      (class-name identifier?)
      (fields (list-of reference?))))

(define-datatype method method?
    (a-method
      (vars (list-of symbol?))
      (body expression?)
      (super-name symbol?)
      (field-names (list-of symbol?))))

(define new-object                      
    (lambda (class-name)
      (an-object
        class-name
        (map 
          (lambda (field-name)
            (newref (list 'uninitialized-field field-name)))
          (class->field-names (lookup-class class-name))))))

(define value-of
    (lambda (exp env)
      (cases expression exp
        
        (new-object-exp (class-name rands)
          (let ((args (values-of-exps rands env))
                (obj (new-object class-name)))
            (apply-method
              (find-method class-name 'initialize)
              obj
              args)
            obj))

        (self-exp ()
          (apply-env env '%self))

        (method-call-exp (obj-exp method-name rands)
          (let ((args (values-of-exps rands env))
                (obj (value-of obj-exp env)))
            (apply-method
              (find-method (object->class-name obj) method-name)
              obj
              args)))
      
        (super-call-exp (method-name rands)
          (let ((args (values-of-exps rands env))
                (obj (apply-env env '%self)))
            (apply-method
              (find-method (apply-env env '%super) method-name)
              obj
              args)))        
        )))

;; methods are like procedures but they have no saved env
;; they keep track of the fields they refer, when a method is applied,
;; it expands its environment

(define method-decls->method-env
    (lambda (m-decls super-name field-names)
      (map
        (lambda (m-decl)
          (cases method-decl m-decl
            (a-method-decl (method-name vars body)
              (list method-name
                (a-method vars body super-name field-names)))))
        m-decls)))

(define apply-method                    
    (lambda (m self args)
      (cases method m
        (a-method (vars body super-name field-names)
          (value-of body
            (extend-env vars (map newref args)
              (extend-env-with-self-and-super
                self super-name
                (extend-env field-names (object->fields self)
                  (empty-env)))))))))

; the new class override the super class, so just append
(define merge-method-envs
    (lambda (super-m-env new-m-env)
      (append new-m-env super-m-env)))

;; Class Representation, class environment(global)
;; when initializing add the super classes fields
(define-datatype class class?
    (a-class
      (super-name (maybe symbol?))
      (field-names (list-of symbol?))
      (method-env method-environment?)))

(define the-class-env '())

(define add-to-class-env!
    (lambda (class-name class)
      (set! the-class-env
        (cons
          (list class-name class)
          the-class-env))))

; object is the super class of all classes
(define initialize-class-env!
    (lambda (c-decls)
      (set! the-class-env 
        (list
          (list 'object (a-class #f '() '()))))
      (for-each initialize-class-decl! c-decls)))

(define initialize-class-decl!
    (lambda (c-decl)
      (cases class-decl c-decl
        (a-class-decl (c-name s-name f-names m-decls)
          (let ((f-names
                  (append-field-names
                    (class->field-names (lookup-class s-name))
                    f-names)))
            (add-to-class-env!
              c-name
              (a-class s-name f-names
                (merge-method-envs
                  (class->method-env (lookup-class s-name))
                  (method-decls->method-env
                    m-decls s-name f-names)))))))))

;;;; Typed OO
;;;; 1. interface 2. subtype polymorphism
;;;; 3. casting 4. instanceof test

;;;; subtyping is covaraint in result type and contravariant in argument type
;;;; -> useful when replacing check-equal-type with check-is-subtype!
;;;; -> like t1 = c1 -> d1, t2 = c2 -> d2, then t1 <: t2

;;;; is-subclass? -> recursion to compare, much like union-find

(define-datatype static-class static-class?
    (a-static-class
      (super-name (maybe identifier?))
      (interface-names (list-of identifier?))
      (field-names (list-of identifier?))
      (field-types (list-of type?))
      (method-tenv method-tenv?))
    (an-interface
      (method-tenv method-tenv?)))

(define add-class-decl-to-static-class-env!
    (lambda (c-decl)
      (cases class-decl c-decl 
        (an-interface-decl (i-name abs-m-decls)
          (let ((m-tenv
                  (abs-method-decls->method-tenv abs-m-decls)))
            (check-no-dups! (map car m-tenv) i-name)
            (add-static-class-binding!
              i-name (an-interface m-tenv))))
        (a-class-decl (c-name s-name i-names
                        f-types f-names m-decls)
          (let ((i-names
                  (append
                    (static-class->interface-names
                      (lookup-static-class s-name))
                    i-names))
                (f-names
                  (append-field-names
                    (static-class->field-names
                      (lookup-static-class s-name))
                    f-names))
                (f-types
                  (append
                    (static-class->field-types
                      (lookup-static-class s-name))
                    f-types))
                (method-tenv
                  (let ((local-method-tenv
                          (method-decls->method-tenv m-decls)))
                    (check-no-dups!
                      (map car local-method-tenv) c-name)
                    (merge-method-tenvs
                      (static-class->method-tenv
                        (lookup-static-class s-name))
                      local-method-tenv))))
            (check-no-dups! i-names c-name)
            (check-no-dups! f-names c-name)
            (check-for-initialize! method-tenv c-name)
            (add-static-class-binding! c-name
              (a-static-class
                s-name i-names f-names f-types method-tenv)))))))

;;;; Checker -> make sure the object definitions correspond to calls
;;;;  -> check the static class environment(including interfaces) and make sure
;;;;   the class implements it

(define type-of
    (lambda (exp tenv)
      (cases expression exp

        (new-object-exp (class-name rands)
            (let ((arg-types (types-of-exps rands tenv))
                  (c (lookup-static-class class-name)))
              (cases static-class c
                (an-interface (method-tenv)
                  (report-cant-instantiate-interface class-name))
                (a-static-class (super-name i-names
                                  field-names field-types method-tenv)
                  ;; check the call to initialize
                  (type-of-call
                    (find-method-type
                      class-name
                      'initialize) 
                    arg-types
                    rands
                    exp)
                  ;; and return the class name as a type
                  (class-type class-name)))))

        (self-exp ()
          (apply-tenv tenv '%self))

        (method-call-exp (obj-exp method-name rands)
          (let ((arg-types (types-of-exps rands tenv))
                (obj-type (type-of obj-exp tenv)))
            (type-of-call
              (find-method-type
                (type->class-name obj-type)
                method-name)
              arg-types
              rands
              exp)))

        (super-call-exp (method-name rands)
          (let ((arg-types (types-of-exps rands tenv))
                (obj-type (apply-tenv tenv '%self)))
            (type-of-call
              (find-method-type
                (apply-tenv tenv '%super)
                method-name)
              arg-types
              rands
              exp)))

        (cast-exp (exp class-name)
          (let ((obj-type (type-of exp tenv)))
            (if (class-type? obj-type)
              (class-type class-name)
              (report-bad-type-to-cast obj-type exp))))

          (instanceof-exp (exp class-name)
            (let ((obj-type (type-of exp tenv)))
              (if (class-type? obj-type)
                (bool-type)
                (report-bad-type-to-instanceof obj-type exp))))

        )))