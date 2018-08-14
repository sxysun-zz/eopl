; chapter 1 code of essentials of programming languages book EPL
; basically just FP basics, I already know this very well so not completed the exercises
#lang racket

; list operations
(define list-len
  (λ (list)
    (if (null? list)
        0
        (+ 1 (list-len (cdr list))))))

(define nth-ele
  (λ (list n)
      (if (null? list)
          (error "list too short")
          (if (zero? n)
              (car list)
              (nth-ele (cdr list) (- n 1))))))

; lambda calculus FV
(define is-FV?
  (λ (var expr)
    (cond
      ((symbol? exp) (eqv? var expr))
      ((eqv? (car exp) 'lambda)
       (and
        (not (eqv? var (car (cdr expr))))
        (is-FV? var (caddr expr))))
      (else
       (or
        (is-FV? var (car expr))
        (is-FV? var (cadr expr)))))))