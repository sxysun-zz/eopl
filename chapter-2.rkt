; chapter 2 code of essentials of programming languages book EPL
; data abstaction

; basically use interfaces for abstract data types, make things obaque, not transparent
#lang racket

; naive environment interface
(define env0
  (λ () (list 'env0)))

(define extend-env
  (λ (var val env)
    (list 'extend-env var val env)))

(define find-ele
  (λ (env var)
    (cond
      ((eqv? (car env) 'env0)
       (error "no bindings for ~s" var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? var saved-var)
             saved-val
             (find-ele saved-env var))))
      (else
       (error "bad environment: ~s" env)))))

; procedural representation of environment
; it is same as taking out an interface
; like the find-ele does not rely on implementation now
(define p-env0
  (λ ()
    (λ (var)
      (error "no bindings for ~s" var))))

(define p-extend-env
  (λ (svar sval senv)
    (λ (var)
      (if (eqv? var svar)
          sval
          (p-find-ele senv var)))))

(define p-find-ele
  (λ (env var)
    (env var)))

; the idea is to have `constructors` and `observers`
; like in OOP

;; Tips of designing an inerface for recursive data type(like AST)
; include one constructor, predicator, extractor for every data type

;;; lambda calculus full interface
  ;; var-exp : Var -> Lc-exp
  (define var-exp
    (lambda (var)
      `(var-exp ,var)))

  ;; lambda-exp : Var * Lc-exp -> Lc-exp
  (define lambda-exp
    (lambda (var lc-exp)
      `(lambda-exp ,var ,lc-exp)))

  ;; app-exp : Lc-exp * Lc-exp -> Lc-exp
  (define app-exp
    (lambda (lc-exp1 lc-exp2)
      `(app-exp ,lc-exp1 ,lc-exp2)))

  ;; var-exp? : Lc-exp -> Bool
  (define var-exp?
    (lambda (x)
      (and (pair? x) (eq? (car x) 'var-exp))))

  ;; lambda-exp? : Lc-exp -> Bool
  (define lambda-exp?
    (lambda (x)
      (and (pair? x) (eq? (car x) 'lambda-exp))))

  ;; app-exp? : Lc-exp -> Bool 
  (define app-exp?
    (lambda (x)
      (and (pair? x) (eq? (car x) 'app-exp))))
  ;; var-exp->var : Lc-exp -> Var
  (define var-exp->var
    (lambda (x)
      (cadr x)))

  ;; lambda-exp->bound-var : Lc-exp -> Var
  (define lambda-exp->bound-var
    (lambda (x)
      (cadr x)))

  ;; lambda-exp->body : Lc-exp -> Lc-exp
  (define lambda-exp->body
    (lambda (x)
      (caddr x)))

  ;; app-exp->rator : Lc-exp -> Lc-exp
  (define app-exp->rator
    (lambda (x)
      (cadr x)))

  ;; app-exp->rand : Lc-exp -> Lc-exp
  (define app-exp->rand
    (lambda (x)
      (caddr x)))

  ;; occurs-free? : Sym * Lcexp -> Bool
  (define occurs-free?
    (lambda (search-var exp)
      (cond
        ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
        ((lambda-exp? exp) 
         (and
           (not (eqv? search-var (lambda-exp->bound-var exp)))
           (occurs-free? search-var (lambda-exp->body exp))))
        (else
          (or
            (occurs-free? search-var (app-exp->rator exp))
            (occurs-free? search-var (app-exp->rand exp)))))))

;; parser and un-parser
; WARNING: code below is just pasted from others, not working
(define parse-expression
    (lambda (datum)
      (cond
        ((symbol? datum) (var-exp datum))
        ((pair? datum)
         (if (eqv? (car datum) 'lambda)
           (lambda-exp
             (car (cadr datum))
             (parse-expression (caddr datum)))
           (app-exp
             (parse-expression (car datum))
             (parse-expression (cadr datum)))))
        (else (report-invalid-concrete-syntax datum)))))

(define unparse-lc-exp
    (lambda (exp)
      (cases lc-exp exp
        (var-exp (var) var)
        (lambda-exp (bound-var body) 
          (list 'lambda (list bound-var)
            (unparse-lc-exp body)))
        (app-exp (rator rand)
          (list 
            (unparse-lc-exp rator) (unparse-lc-exp rand))))))