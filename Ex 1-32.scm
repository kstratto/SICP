#lang racket
; Recursive definition of accumulate
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-recur combiner null-value term (next a) next b))))

; Iterative definition of accumulate
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; Definition of sum in terms of accumulate
(define (sum term a next b)
  (accumulate-recur + 0 term a next b))

; Definition of product in terms of accumulate
(define (product term a next b)
  (accumulate-iter * 1 term a next b))

; Exercise 1.29 done using accumulate
(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
          ((= (remainder k 2) 1) (* 4 (f (+ a (* k h)))))
          (else (* 2 (f (+ a (* k h)))))))
  (* (sum simpson-term 0 inc n) (/ h 3)))

; Exercise 1.31 done using accumulate
(define (pi-prod a b)
  (define (pi-term k)
    (define (num-term k)
      (+ (* 2.0 (floor (/ k 2))) 2))
    (define (denom-term k)
      (+ (* 2.0 (ceiling (/ k 2))) 1))
    (/ (num-term k) (denom-term k)))
  (define (inc k) (+ k 1))
  (product pi-term a inc b))