; chapter 3 code of essentials of programming languages book EPL
#lang racket
; about the binding and scoping of variables
; just some basic stuff about pl, so I did not write much

; byte code -> virtual machine

; the user write AST by hand -> Lisp

; the way EOPL did letrec is same as what I did in Alayi-lang

; lexical and dynamic scoping

;; lexical addressing
;;  -> to record the lexical depth of variables
;; basically it is: code -> translator(get AST to be nameless, record
;; variable values in array, the variables in AST are replaced with their
;; corresponding array indices, thus avoiding conflict definition and
;; nameless), then interpreter(just normal interpreter with faster
;; variable table accessing)

;; note: the nameless-env aka the array has to be used twice, once in
;; interpreter and once in translator, to record vars or vals with
;; respect to their lexical address