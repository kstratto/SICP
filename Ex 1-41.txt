#lang racket
; Takes a procedure f which takes one argument x and returns a procedure
; which applies f twice to x.
(define (double f)
  (lambda (x) (f (f x))))
(define (inc x) (+ 1 x))

; When we run (((double (double double)) inc) 5), the output is 5 + 16 = 21
; This is because the inner (double double) gives a procedure equivalent to
; (quadruple f), which produces a procedure that applies f four times to the
; original argument.
; Thus, (double (double double)) is equivalent to (double quadruple), which
; in turn is equivalent to (applie-16-times f), which produces a procedure
; that applies f 16 times to the original argument.
; This is because ((double quadruple) f) is equivalent to (quadruple (quadruple f)).