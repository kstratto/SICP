#lang racket
; Recursive definition of product
(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recur term (next a) next b))))

; Iterative definition of product
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; Definition of factorial in terms of product
(define (factorial n)
  (define (identity n) n)
  (define (inc n) (+ n 1))
  (product-recur identity 1 inc n))

; Definition of product formula for approximating pi/4
; Use the fact that a formula for the kth factor in numerator is
; N(k) = 2*floor(k/2) + 2
; Use the fact that a formula for the kth factor in denominator is
; D(k) = 2*ceiling(k/2) + 1
(define (pi-prod a b)
  (define (pi-term k)
    (define (num-term k)
      (+ (* 2.0 (floor (/ k 2))) 2))
    (define (denom-term k)
      (+ (* 2.0 (ceiling (/ k 2))) 1))
    (/ (num-term k) (denom-term k)))
  (define (inc k) (+ k 1))
  (product-iter pi-term a inc b))