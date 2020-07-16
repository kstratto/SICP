#lang racket
(define (square x)
  (* x x))
(define (sq-sum x y)
  (+ (square x) (square y)))
(define (two-largest-sq-sum x y z)
  (cond ((and (< x y) (< x z)) (sq-sum y z))
        ((and (< y x) (< y z)) (sq-sum x z))
        ((and (< z x) (< z y)) (sq-sum x y))
        (else (sq-sum x y))))