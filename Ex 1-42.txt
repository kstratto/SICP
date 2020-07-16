#lang racket
; Takes two functions f(x) and g(x) and returns f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))
(define (inc x) (+ 1 x))
(define (square x) (* x x))