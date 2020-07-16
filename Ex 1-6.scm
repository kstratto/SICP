#lang racket
(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (avg x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (avg guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new (improve guess x) x)))
; When Alyssa tries to use sqrt-iter-new, which uses new-if, to compute square roots,
; she will get caught in infinite recursion. This is due to the fact that, as a function,
; Scheme uses applicative order when evaluating a call to new-if.
; In other words it attempts to evaluate all of the arguments that get passed to new-if,
; which in this case includes a call to sqrt-iter-new, that in turn calls new-if again.
; The regular (if <predicate> <consequent> <alternative>) expression is first evaluated
; by evaluating the <predicate>. If the <predicate> evaluates to a true value, then
; the interpreter evaluates the <consequent> and returns its value. Otherwise it evaluates
; the <alternative> and returns its value. This gives a stopping point for the recursion,
; since at some point the predicate (good-enough? guess x) will evaluate to a true function
; and the if expression will then return guess without attempting to evaluate the
; alternative and further recurse.