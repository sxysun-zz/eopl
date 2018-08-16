; chapter 5 code of essentials of programming languages book EPL
#lang racket
; About CPS interpreters, CPS <-> SSA
; CPS allow: exceptions, callcc, threading

; control behaviours: iterative(tail recursion) and recursive
; it is the evaluation of operands, not calling of procedures, that makes
;  the control context grow

; continuation: a data type that allows to track and manipulate
;  contexts

;; continuation -> control context
;; environment -> data context

;; CPS Interpreter
;; explicitly make continuation a third argument to interp() aside exp, env

; The continuation of an expression represents a procedure that takes
; the result of the expression and completes the computation

; end of continuation only appear once in the program
(define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))

(define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont () 
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
              (zero? (expval->num val)))))
        (let-exp-cont (var body saved-env saved-cont)
          (value-of/k body
            (extend-env var val saved-env) saved-cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        (rator-cont (rand saved-env saved-cont)
          (value-of/k rand saved-env
            (rand-cont val saved-cont)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))
        )))

(define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont 
            (proc-val (procedure var body env))))
        ; tail calls do not grow continuation
        ; that is tail recursion happens using same continuation
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
          (value-of/k exp1 env
            (zero1-cont cont)))
        (let-exp (var exp1 body)
          (value-of/k exp1 env
            (let-exp-cont var body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))        
        (call-exp (rator rand) 
          (value-of/k rator env
            (rator-cont rand env cont)))
   )))

;; Imperative/ Trampoline interpreter
;; a 0-argument tail call is same as a jump
;; use rejisters to transform the calls, still have continuations first
;; to avoid wrong assignment of variables, set! in meta-lang


;; Exception Interpreter:
;; try/ catch/ throw

;; the body of the try finished normally-- don't evaluate the handler
(define apply-cont
    (lambda (cont val)
      (cases continuation cont
        ;; rest same as before, aka use apply-cont
        (try-cont (var handler-exp saved-env saved-cont)
          (apply-cont saved-cont val))
        ;; val is the value of the argument to raise
        (raise1-cont (saved-cont)
          ;; we put the short argument first to make the trace more readable.
          (apply-handler val saved-cont))
        )))

;; unwind the continuation until we find a handler for raise
(define apply-handler
    (lambda (val cont)
      (cases continuation cont
        ;; interesting cases
        (try-cont (var handler-exp saved-env saved-cont)
          (value-of/k handler-exp
            (extend-env var val saved-env)
            saved-cont))

        (end-cont () (eopl:error 'apply-handler "uncaught exception!"))

        ;; otherwise, just look for the handler...
        (diff1-cont (exp2 saved-env saved-cont)
          (apply-handler val saved-cont))
        (diff2-cont (val1 saved-cont)
          (apply-handler val saved-cont))
        (if-test-cont (exp2 exp3 env saved-cont)
          (apply-handler val saved-cont))
        (unop-arg-cont (unop saved-cont)
          (apply-handler val saved-cont))
        (rator-cont (rand saved-env saved-cont)
          (apply-handler val saved-cont))
        (rand-cont (val1 saved-cont)
          (apply-handler val saved-cont))
        (raise1-cont (cont)
          (apply-handler val cont))
        )))

;;; Multi-Threaded Interpreter
;; multiple computations proceed at once but use same address space
;; use same shared memory using assignments

;; State ::= running | runnable | blocked

;; scheduler -> keep the ready queue as part of its state -> keep a timer
;;  to make sure threds compute certain number of steps then interrupted
;;  and put back to the ready queue

;; language uses implicit refs, that is call by reference, but then
;;  threads will collide and create syncronization problems

;; with mutex to provide locks for threading sync
(define apply-cont                    
    (lambda (cont val)
      (if (time-expired?)
          
        (begin
          (place-on-ready-queue!
            (lambda () (apply-cont cont val)))
          (run-next-thread))
        
        (begin
          (decrement-timer!)
          (cases continuation cont

            (end-main-thread-cont ()
              (set-final-answer! val)
              (run-next-thread))
  
            (end-subthread-cont ()
              (run-next-thread))
            
            (spawn-cont (saved-cont)
              (let ((proc1 (expval->proc val)))
                (place-on-ready-queue!
                  (lambda ()
                    (apply-procedure proc1
                      (num-val 28)
                      (end-subthread-cont))))
              (apply-cont saved-cont (num-val 73))))

            (wait-cont (saved-cont)
              (wait-for-mutex
                (expval->mutex val)
                (lambda () (apply-cont saved-cont (num-val 52)))))

            (signal-cont (saved-cont)
              (signal-mutex
                (expval->mutex val)
                (lambda () (apply-cont saved-cont (num-val 53))))))))))

(define value-of/k                    
    (lambda (exp env cont)
      (when (trace-interp)
        (eopl:printf "value-of/k: ~s~%" exp))
      
      (cases expression exp
        
        (spawn-exp (exp)
          (value-of/k exp env
            (spawn-cont cont)))

        (yield-exp ()
          (place-on-ready-queue!
            (lambda () (apply-cont cont (num-val 99))))
          (run-next-thread))

        (mutex-exp ()
          (apply-cont cont (mutex-val (new-mutex))))  

        (wait-exp (exp)
          (value-of/k exp env
            (wait-cont cont)))

        (signal-exp (exp)
          (value-of/k exp env
            (signal-cont cont))))))
