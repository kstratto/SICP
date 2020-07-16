#lang racket
(define (double n) (* 2 n))
(define (halve n) (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))
(define (iter-mult a b k)
  (cond ((= b 0) k)
        ((even? b) (iter-mult (double a) (halve b) k))
        (else (iter-mult a (- b 1) (+ k a)))))
(define (fast-mult a b)
  (iter-mult a b 0))