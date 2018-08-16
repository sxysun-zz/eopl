; chapter 7 code of essentials of programming languages book EPL
#lang racket
; Types -> safe evaluation, certain types will be inhibited

; Type ::= int | bool | Type -> Type

;;;; for functions use optional types like in 
;;;;  (define (lambda (x) x)) :: t -> t

; Checker & Inferrer

;; Inferrer -> list the equations, then use substitution of the equations
;;  to find conflicts, or if the equations reach a tautology then means the
;;  current substitutions make a correct inference. and Conflicts means:
;;  1. bool = int
;;  2. occurrence check(t = t -> int,
;;   no variable bound in the substitution(on lhs)
;;   occurs in rhs of the substitution)

;; data structure of substitution: list of pairs (type-var . type)
(define empty-subst (lambda () '()))

;; this automatically refreshes the current subst
;; aka. add itself to list,
;;  then refresh every related subst rules in current subst using this rule
(define extend-subst
    (lambda (subst tvar ty)
      (cons
        (cons tvar ty)
        (map 
          (lambda (p)
            (let ((oldlhs (car p))
                  (oldrhs (cdr p)))
              (cons
                oldlhs
                (apply-one-subst oldrhs tvar ty))))
          subst))))

;; t0[tv = t1]
(define apply-one-subst
    (lambda (ty0 tvar ty1)
      (cases type ty0
        (int-type () (int-type))
        (bool-type () (bool-type))
        (proc-type (arg-type result-type)
          (proc-type
            (apply-one-subst arg-type tvar ty1)
            (apply-one-subst result-type tvar ty1)))
        (tvar-type (sn)
          (if (equal? ty0 tvar) ty1 ty0)))))

;; type * subst -> type
(define apply-subst-to-type
    (lambda (ty subst)
      (cases type ty
        (int-type () (int-type))
        (bool-type () (bool-type))
        (proc-type (t1 t2)
          (proc-type
            (apply-subst-to-type t1 subst)
            (apply-subst-to-type t2 subst)))
        (tvar-type (sn)
          (let ((tmp (assoc ty subst))) ; assoc returns mathing pair or #f
            (if tmp
              (cdr tmp)
              ty))))))

;;; The Unifier -> maintain no-occurrence

(define no-occurrence?
    (lambda (tvar ty)
      (cases type ty
        (int-type () #t)
        (bool-type () #t)
        (proc-type (arg-type result-type)
          (and
            (no-occurrence? tvar arg-type)
            (no-occurrence? tvar result-type)))
        (tvar-type (serial-number) (not (equal? tvar ty))))))

;; make sure that the rule added is no-occurrence
;; subst satisfies no-occurrence invaraint
;; returns subst that adds t1 = t2
(define unifier
    (lambda (ty1 ty2 subst exp)
      (let ((ty1 (apply-subst-to-type ty1 subst))
            (ty2 (apply-subst-to-type ty2 subst)))
        (cond
          ((equal? ty1 ty2) subst); if same type, then delete this trivial       
          ((tvar-type? ty1)
           (if (no-occurrence? ty1 ty2)
             (extend-subst subst ty1 ty2)
             (report-no-occurrence-violation ty1 ty2 exp)))
          ((tvar-type? ty2)
           (if (no-occurrence? ty2 ty1)
             (extend-subst subst ty2 ty1)
             (report-no-occurrence-violation ty2 ty1 exp)))
          ((and (proc-type? ty1) (proc-type? ty2))
           (let ((subst (unifier
                          (proc-type->arg-type ty1)
                          (proc-type->arg-type ty2)
                          subst exp)))
             (let ((subst (unifier
                            (proc-type->result-type ty1)
                            (proc-type->result-type ty2)
                            subst exp)))
               subst)))
          (else (report-unification-failure ty1 ty2 exp))))))

;;; Finding type of an Expression
; () -> Type
(define fresh-tvar-type
    (let ((sn 0))
      (lambda ()
        (set! sn (+ sn 1))
        (tvar-type sn))))

; Optiona Type -> Type
(define otype->type
    (lambda (otype)
      (cases optional-type otype
        (no-type () (fresh-tvar-type))
        (a-type (ty) ty))))

;; type-of : Exp * Tenv(program var -> type expr) * Subst(type var -> type) -> Type
;; returns two values:
;;  1. a type expression
;;  2. a substitution in which to interpret the type variables
;;   in that expression
;; WARNING: result not normalized, aka. t1 -> t1, t2 -> t2 possible
;;  need take another pass of renumbering type vars

(define type-of
    (lambda (exp tenv subst)
      (cases expression exp
        
        (const-exp (num) (an-answer (int-type) subst))
      
        (zero?-exp (exp1)
          (cases answer (type-of exp1 tenv subst)
            (an-answer (type1 subst1)
              (let ((subst2 (unifier type1 (int-type) subst1 exp)))
                (an-answer (bool-type) subst2)))))

        (diff-exp (exp1 exp2)
          (cases answer (type-of exp1 tenv subst)
            (an-answer (type1 subst1)
              (let ((subst1 (unifier type1 (int-type) subst1 exp1)))
                (cases answer (type-of exp2 tenv subst1)
                  (an-answer (type2 subst2)
                    (let ((subst2
                            (unifier type2 (int-type) subst2 exp2)))
                      (an-answer (int-type) subst2))))))))

        (if-exp (exp1 exp2 exp3)
          (cases answer (type-of exp1 tenv subst)
            (an-answer (ty1 subst)
              (let ((subst (unifier ty1 (bool-type) subst
                             exp1)))
                (cases answer (type-of exp2 tenv subst)
                  (an-answer (ty2 subst)
                    (cases answer (type-of exp3 tenv subst)
                      (an-answer (ty3 subst)
                        (let ((subst (unifier ty2 ty3 subst exp)))
                          (an-answer ty2 subst))))))))))

        (var-exp (var) (an-answer (apply-tenv tenv var) subst))

        (let-exp (var exp1 body)
          (cases answer (type-of exp1 tenv subst)
            (an-answer (rhs-type subst)
              (type-of body
                (extend-tenv var rhs-type tenv)
                subst))))

        (proc-exp (var otype body)
          (let ((arg-type (otype->type otype)))
            (cases answer (type-of body
                            (extend-tenv var arg-type tenv)
                            subst)
              (an-answer (result-type subst)
                (an-answer
                  (proc-type arg-type result-type)
                  subst)))))

        (call-exp (rator rand)
          (let ((result-type (fresh-tvar-type)))
            (cases answer (type-of rator tenv subst)
              (an-answer (rator-type subst)
                (cases answer (type-of rand tenv subst)
                  (an-answer (rand-type subst)
                    (let ((subst
                            (unifier rator-type
                              (proc-type rand-type result-type)
                              subst
                              exp)))
                      (an-answer result-type subst))))))))

        (letrec-exp (proc-result-otype proc-name 
                      bvar proc-arg-otype 
                      proc-body
                      letrec-body)
          (let ((proc-result-type
                  (otype->type proc-result-otype)) 
                (proc-arg-type
                  (otype->type proc-arg-otype)))
            (let ((tenv-for-letrec-body
                    (extend-tenv 
                      proc-name
                      (proc-type proc-arg-type proc-result-type)
                      tenv)))
              (cases answer (type-of proc-body
                              (extend-tenv
                                bvar proc-arg-type tenv-for-letrec-body)
                              subst)
                (an-answer (proc-body-type subst)
                  (let ((subst 
                          (unifier proc-body-type proc-result-type subst
                            proc-body))) 
                    (type-of letrec-body
                      tenv-for-letrec-body
                      subst)))))))
