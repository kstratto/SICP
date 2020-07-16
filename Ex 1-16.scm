#lang racket
(define (even? n)
  (= (remainder n 2) 0))
(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (* b b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))))
(define (fast-expt b n)
  (expt-iter b n 1))