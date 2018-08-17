; chapter 8 code of essentials of programming languages book EPL
#lang racket
; Modules -> use type systems to create and enforce abstraction boundries
;  aka. better scoping

; Module -> interface and body, also typed -> like declare point = int * int
;  each module expression contain a small environment

;; Checker -> make sure implementation fits the interface -> type checking
;   -> expander, make the named types and quantified ones expand
;   -> expand first then check

;; good type system for module: -> opaque and transparent

;; transparent type -> transparent t = int

;; opaque type -> interface: opaque t; body: type t = int;
;; opaque t -> t behaves like a primitive type

; Type ::= int | bool | from m take t(opaque type, like primitive) | type -> type

;;; Module Procedures
;; functions that take a module type as argument/ return one
;; but also the types can depend on modules, like creating a new module

;;; subtyping -> t1 has at least many things as t2