#lang racket
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; With an interpreter that uses applicative-order evaluation, attempting to evaluate
; the expression (test 0 (p)) will cause the interpreter to get stuck into an infinite
; loop as it continually evaluates the self-referential expression (p), which
; evaluates to itself when it is called.
; With an interpreter that uses normal-order evaluation, attempting to evaluate
; the expression (test 0 (p)) will give the result 0. This is because 0 and (p) are first
; passed to the body of test, without being evaluated. Then the predicate (= x 0) is
; evaluated to true, so test then evaluates to 0.