#lang racket

; toy lambda calculus extension interprter

(define interp
  (λ (expr env)
    