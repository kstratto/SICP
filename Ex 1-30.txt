#lang racket
; Recursive definition of sum
(define (sum-recur term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recur term (next a) next b))))

; Iterative definition of sum
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube n) (* n n n))
(define (identity n) n)
(define (inc n) (+ n 1))

; Procedure implementing Simpson's rule for approximating definite integrals
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
          ((= (remainder k 2) 1) (* 4 (f (+ a (* k h)))))
          (else (* 2 (f (+ a (* k h)))))))
  (* (sum-iter simpson-term 0 inc n) (/ h 3)))

;> (simpson cube 0 1 100.0)
;0.24999999999999992
;> (simpson cube 0 1 1000.0)
;0.2500000000000003
          