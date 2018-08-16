; chapter 6 code of essentials of programming languages book EPL
#lang racket
; About turning any program into CPS, aka iterative control behaviour

; CPS interpreter use tail calls, thus a bounded amount of control context
;  -> iterative control behaviour

;;;; CPS benifits:
;; 1. desugar continuations to functions, lowere complexity
;; 2. advanced control flow and computational effects(side-effect)

; writing CPS programs
(define fact
  (λ (n)
    (if (eqv? 0 n)
        1
        (* n (fact (- n 1))))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val)
      (fact1-cont (saved-n saved-cont)
                  (apply-cont saved-cont (* saved-n val))))))

(define fact-cps
  (λ (n cont)
    (if (eqv? 0 n)
        (apply-cont cont 1)
        (fact-cps (- n 1) (fact1-cont n cont)))))

;; procedual style
;;  -> so not enumeration in apply-cont based on data structure
(define end-cont
  (λ ()
    (λ (val) val)))

(define fact1-cont
  (λ (n saved-cont)
    (λ (val)
      (apply-cont saved-cont (* n val)))))

(define apply-cont
  (λ (cont val)
    (cont val)))

;; inlining: inline all the uses of continuations
;; like partially applied functions, currying, not really tail-recursion
;; because the argument is not evaluated immediately
(define fact
  (λ (n)
    (fact-cps n (λ (val) val))))

(define fact-cps
  (λ (n cont)
    (if (zero? n)
        (cont 1)
        (fact-cps (- n 1) (λ (val) (cont (* n val)))))))

;;; CPS Recipe:
;; 1. pass each procedure an argument `cont`
;; 2. constant returns -> return (cont const)
;; 3. procedure call in  tial position -> call procedure with cc
;; 4. procedure call in operand position -> evaluate the call in
;;  new continuation that gives a name to the result and continue
;;  with the computation

(define fib
  (λ (n)
    (fib-cps n (λ (val) val))))

(define fib-cps
  (λ (n cont)
    (if (< n 2)
        (cont 1)
        (fib-cps (- n 1)
                 (λ (val1)
                   (fib-cps (- n 2)
                            (λ (val2)
                              (cont (+ val1 val2)))))))))

;;; Converting to CPS, a tranlator
;; distinguish between simple and non simple operands
;; for procedures/ non-simples we CPS it, make it
;;  evaluate first and extend continuation with lambda
;;  that have the non-simple's position replaced by vars
;; just see CPS evaluation examples at page 252

(define cps-of-exps
    (lambda (exps builder)
      (let cps-of-rest ((exps exps))
        ;; cps-of-rest : Listof(InpExp) -> TfExp
        (let ((pos (list-index
                     (lambda (exp)
                       (not (inp-exp-simple? exp)))
                     exps)))
          (if (not pos)
            (builder (map cps-of-simple-exp exps))
            (let ((var (fresh-identifier 'var)))
              (cps-of-exp
                (list-ref exps pos)
                (cps-proc-exp (list var)
                  (cps-of-rest
                    (list-set exps pos (var-exp var)))))))))))

(define cps-of-exp
    (lambda (exp cont)
      (cases expression exp
        (const-exp (num) (make-send-to-cont cont (cps-const-exp num)))
        (var-exp (var) (make-send-to-cont cont (cps-var-exp var)))
        (proc-exp (vars body) 
          (make-send-to-cont cont
            (cps-proc-exp (append vars (list 'k%00))
              (cps-of-exp body (cps-var-exp 'k%00)))))
        (zero?-exp (exp1)
          (cps-of-zero?-exp exp1 cont))
        (diff-exp (exp1 exp2)
          (cps-of-diff-exp exp1 exp2 cont))
        (sum-exp (exps)
          (cps-of-sum-exp exps cont))
        (if-exp (exp1 exp2 exp3)
          (cps-of-if-exp exp1 exp2 exp3 cont))
        (let-exp (var exp1 body)
          (cps-of-let-exp var exp1 body cont))
        (letrec-exp (ids bidss proc-bodies body)
          (cps-of-letrec-exp ids bidss proc-bodies body cont))
        (call-exp (rator rands)
          (cps-of-call-exp rator rands cont)))))

(define cps-of-simple-exp
    (lambda (exp)
      (cases expression exp
        (const-exp (num) (cps-const-exp num))
        (var-exp (var) (cps-var-exp var))
        (diff-exp (exp1 exp2)
          (cps-diff-exp
            (cps-of-simple-exp exp1)
            (cps-of-simple-exp exp2)))
        (zero?-exp (exp1)
          (cps-zero?-exp
            (cps-of-simple-exp exp1)))
        (proc-exp (ids exp) 
          (cps-proc-exp (append ids (list 'k%00))
            (cps-of-exp exp (cps-var-exp 'k%00))))
        (sum-exp (exps)
          (cps-sum-exp
            (map cps-of-simple-exp exps)))
        (else 
          (report-invalid-exp-to-cps-of-simple-exp exp)))))

(define make-send-to-cont
    (lambda (cont bexp)
      (cps-call-exp cont (list bexp))))

(define cps-of-zero?-exp
    (lambda (exp1 k-exp)
      (cps-of-exps (list exp1)
        (lambda (new-rands)
          (make-send-to-cont
            k-exp
            (cps-zero?-exp 
              (car new-rands)))))))

(define cps-of-sum-exp
    (lambda (exps k-exp)
      (cps-of-exps exps
        (lambda (new-rands)
          (make-send-to-cont
            k-exp
            (cps-sum-exp new-rands))))))

(define cps-of-diff-exp
    (lambda (exp1 exp2 k-exp)
      (cps-of-exps
        (list exp1 exp2)
        (lambda (new-rands)
          (make-send-to-cont
            k-exp
            (cps-diff-exp
              (car new-rands)
              (cadr new-rands)))))))

(define cps-of-if-exp
    (lambda (exp1 exp2 exp3 k-exp)
      (cps-of-exps (list exp1)
        (lambda (new-rands)
          (cps-if-exp (car new-rands)
            (cps-of-exp exp2 k-exp)
            (cps-of-exp exp3 k-exp))))))

(define cps-of-let-exp
    (lambda (id rhs body k-exp)
      (cps-of-exps (list rhs)
        (lambda (new-rands)
          (cps-let-exp id 
            (car new-rands)
            (cps-of-exp body k-exp))))))

(define cps-of-letrec-exp
    (lambda (proc-names idss proc-bodies body k-exp)
      (cps-letrec-exp
        proc-names
        (map
          (lambda (ids) (append ids (list 'k%00)))
          idss)
        (map
          (lambda (exp) (cps-of-exp exp (cps-var-exp 'k%00)))
          proc-bodies)
        (cps-of-exp body k-exp))))

(define cps-of-call-exp
    (lambda (rator rands k-exp)
      (cps-of-exps (cons rator rands)
        (lambda (new-rands)
          (cps-call-exp
            (car new-rands)
            (append (cdr new-rands) (list k-exp)))))))

;;; Modeling Computational Effects
;; simple expressions have no effects
;; (thus no procedure calls, because potential non-termination)

; example assume print() returns 38 and has side-effects
(print-exp (rator)
           (cps-of-exps (list rator)
                        (lambda (simples)
                          (cps-printk-exp
                           (car simples)
                           (make-send-to-cont k-exp
                                              (cps-const-exp 38))))))

;; this translation will make print()'s side effects and its
;;  returned value explicit, distinguished in code