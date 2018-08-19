# eopl
my notes for the book Essentials of Programming Languages
including some exercises and codes

and some topics are from the book PLAI by sk / Matt Might's blog

codes written in PLT Scheme

#### Index(toaru majutsu?)

0. chapter-3 -> scoping
1. chapter-4 -> references, evaluation strategies(call by ?)
2. chapter-5 -> CPS interpreter, as IR opposed to SSA, advanced control-flow
3. chapter-6 -> CPS translator, make any code into CPS, expose side-effect
4. chapter-7 -> Hindley-Milner
5. chapter-8 -> Module, struct
6. chapter-9 -> interfaces & classes, subtype polymorphism

#### Good ideas:

0. first class function/continuation/macro
1. CPS for advanced control(exception, threading, return) and SAT solver embedding
2. Type inference -> equation solving
3. different evaluation strategies(need, name, value, ref)
4. struct/module
5. OOP(subtyping polymorphism implementation, type checker, method with super and self)

#### Yet to add: 

1. garbage collection -> sound and complete -> mark-sweep
2. macro -> innate support for desugaring -> define-syntax -> hygienic macro(no pollution of namespace, lexical scope)
3. rich type system(dependent, kinds, typeclasses)
4. reflection -> inspect and modify its own structure at runtime(like python's getattr())
5. contract -> assertions, dynamic verification
6. implicit(backup for type inference)
7. embedded program synthesis and static analysis