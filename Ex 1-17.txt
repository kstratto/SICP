#lang racket
(define (double n) (* 2 n))
(define (halve n) (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))
(define (recur-fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (recur-fast-mult (double a) (halve b)))
        (else (+ a (recur-fast-mult a (- b 1))))))