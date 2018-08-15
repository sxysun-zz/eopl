; chapter 4 code of essentials of programming languages book EPL
#lang racket
; About states and computational effects
; basically talk about diffrences between values and references
; ExpVal = Int | Bool | Func | Ref(ExpVal)

;; Two designs: explicit references and implicit references
; explicit: definition of setRef, deRef, use global varaible to model
; implicit: everything is referenced

;; Parameter passing variants
; call by value(eager): normal, evaluate first, then pass without reference
; call by reference(eager): just call by reference
; call by name(lazy, good for programs with
;  no effects, because like beta reduction, it replaces things and
;  ignore evaluation order): use thunk(a tuple of an expression and
;  and environment) to replace values
; call by need(lazy): once thunk is evaluated, use the evaluated to replace
;  thunk in the symbol table, memorization

; call by need
(define value-of-operand
    (lambda (exp env)
      (cases expression exp
        (var-exp (var) (apply-env env var)) ; no deref!
        (else
          (newref (a-thunk exp env))))))

; call by reference
(define value-of
    (lambda (exp env)
      (cases expression exp
		(call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of-operand rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps))))))

; references data structure
(define newref
    (lambda (val)
      (let ((next-ref (length the-store)))
        (set! the-store
              (append the-store (list val)))
        (when (instrument-newref)
            (eopl:printf 
             "newref: allocating location ~s with initial contents ~s~%"
             next-ref val))                     
        next-ref)))

(define deref 
    (lambda (ref)
      (list-ref the-store ref)))

(define setref!                       
    (lambda (ref val)
      (set! the-store
        (letrec
          ((setref-inner
             ;; returns a list like store1, except that position ref1
             ;; contains val. 
             (lambda (store1 ref1)
               (cond
                 ((null? store1)
                  (report-invalid-reference ref the-store))
                 ((zero? ref1)
                  (cons val (cdr store1)))
                 (else
                   (cons
                     (car store1)
                     (setref-inner
                       (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref)))))