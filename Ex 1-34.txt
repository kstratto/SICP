#lang racket
(define (f g) (g 2))
(define (square x) (* x x))

; When we ask the interpreter to evaluate the combination (f f) it performs the following steps
; (f f)
; (f 2)
; (2 2)
; Since 2 is not a procedure that can be applied to a given argument,
; this raises an error.