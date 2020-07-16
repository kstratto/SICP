#lang racket
; Recursive definition of filtered-accumulate
(define (filtered-accumulate-recur filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filtered-accumulate-recur filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate-recur filter combiner null-value term (next a) next b))))

; Iterative definition of filtered-accumulate
(define (filtered-accumulate-iter filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

; Definition of sum in terms of filtered-accumulate
(define (sum term a next b)
  (define (filter n) true)
  (filtered-accumulate-recur filter + 0 term a next b))

; Definition of product in terms of filtered-accumulate
(define (product term a next b)
  (define (filter n) true)
  (filtered-accumulate-iter filter * 1 term a next b))

; Exercise 1.29 done using filtered-accumulate
(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
          ((= (remainder k 2) 1) (* 4 (f (+ a (* k h)))))
          (else (* 2 (f (+ a (* k h)))))))
  (* (sum simpson-term 0 inc n) (/ h 3)))

; Exercise 1.31 done using filtered-accumulate
(define (pi-prod a b)
  (define (pi-term k)
    (define (num-term k)
      (+ (* 2.0 (floor (/ k 2))) 2))
    (define (denom-term k)
      (+ (* 2.0 (ceiling (/ k 2))) 1))
    (/ (num-term k) (denom-term k)))
  (define (inc k) (+ k 1))
  (product pi-term a inc b))

; Compute sum of square of prime numbers in interval a to b
; Uses Miller-Rabin test for primality
(define (square x) (* x x))
(define (miller-rabin n)
   (if (= n 1) false (miller-rabin-test (- n 1) n)))
(define (miller-rabin-test a n) 
   (cond ((= a 0) true) 
         ; expmod is congruent to 1 modulo n 
         ((= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n)) 
         (else false)))  
(define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (let ((x (expmod base (/ exp 2) m))) 
            (if (non-trivial-sqrt? x m) 0 (remainder (square x) m)))) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))
(define (non-trivial-sqrt? n m) 
   (cond ((= n 1) false) 
         ((= n (- m 1)) false) 
         (else (= (remainder (square n) m) 1))))
(define (sum-squared-primes a b)
  (filtered-accumulate-iter miller-rabin + 0 square a inc b))

; Compute product of all positive integers less than n that are coprime to n
(define (gcd a b)
   (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (totive-product n)
   (define (coprime-n a) (= 1 (gcd a n)))
   (define (identity x) x)
   (filtered-accumulate-recur coprime-n * 1 identity 1 inc (- n 1)))