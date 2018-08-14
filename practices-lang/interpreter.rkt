#lang racket

; taken from yin wang

;; type-inferencer

; lexical scope
(struct closure (fun env))
; environment
(define env0 `())
(define ext-env
  (lambda (x v env)
    (cons (cons x v) env)))
(define search
  (lambda (x env)
    (cond
      ((eq? (car env) `()) #f)
      ((eq? (car (car env)) x) (cdr (car env)))
      (else (search x (cdr env))))))

(define infer
  (lambda (exp env)
    (match exp
      ((? symbol? x)
       (let ((sed (search x env)))
         (cond
           ((not sed)
            (display "undefined var"))
           (else sed))))
      ((? number? x) `int)
      ((? string? x) `string)
      ((? boolean? x) `boolean)
      (`(lambda (,x) ,e)
       (closure exp env))
      (`(`let ((,x ,e1)) ,e2)
       (let ((t (infer e1 env)))
         (infer e2 (ext-env x t env))))
      (`(if ,t ,e1 ,e2)
       (let ((b (infer t env)))
         (cond
           ((eq? b `bool)
            (let ((t1 (infer e1 env))
                  (t2 (infer e2 env)))
              (cond
                ((eq? t1 t2) t1)
                (else
                 (display "branch type mismatch"))))))))
      (`(,e1 ,e2)
       (let ((t1 (infer e1 env))
             (t2 (infer e2 env)))
         (match t1
           ((closure `(lambda (,x) ,e) env-old)
            (infer e (ext-env x t2 env-old))))))
      (`(,e1 ,op ,e2)
       (let ((t1 (infer e1 env))
             (t2 (infer e2 env)))
         (cond
           ((not (eq? t1 `int)) (display "first operand not int"))
           ((not (eq? t2 `int)) (display "second operand not int"))
           (else `int)))))))

;(infer `((lambda (y) ((lambda (x) ((y * x) * x)) (1 + (2 * -3)))) 2) env0)

;; simple interpreter
(define interp
  (lambda (exp env)
    (match exp
      ((? symbol? x)
       (let ((sed (search x env)))
         (cond
           ((not sed)
            (display "undefined var"))
           (else sed))))
      ((? number? x) x)
      ((? string? x) x)
      ((? boolean? x) x)
      (`(λ ,x . ,e)
       (closure exp env))
      (`(`let ((,x ,e1)) ,e2)
       (let ((t (interp e1 env)))
         (interp e2 (ext-env x t env))))
      (`(if ,t ,e1 ,e2)
       (let ((b (interp t env)))
        (let ((t1 (interp e1 env))
              (t2 (interp e2 env)))
          (cond
            (b (t1))
            (else
             (t2))))))
      (`(,e1 ,e2)
       (let ((t1 (interp e1 env))
             (t2 (interp e2 env)))
         (match t1
           ((closure `(λ ,x . ,e) env-old)
            (interp e (ext-env x t2 env-old))))))
      (`(,x ,op ,y)
       (let ((px (interp x env))
             (py (interp y env)))
         (match op
           (`+ (+ px py))
           (`- (- px py))
           (`* (* px py))
           (`/ (/ px py))))))))

;(interps `((lambda (y) ((lambda (x) ((y * x) * x)) (1 + (2 * -3)))) 2) env0)