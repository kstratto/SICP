#lang racket
(define (factor-exponent x n)
  ; Computes how many times the factor n evenly divides into x
  (define (iter num factor exponent)
    (if (= (remainder num factor) 0)
        (iter (/ num factor) factor (+ 1 exponent))
        exponent))
  (iter x n 0))

; To see why we can represent pairs of nonnegative integers using only
; numbers and arithmetic operations if we represent the pair (a, b) as
; the integer that is the product (2^a)(3^b), we show that there is a
; one-to-one correspondence between the set of all pairs (a, b) of nonnegative
; integers and the set of all integers of the form (2^a)(2^b).
; This correspondence is given by the mapping (a, b) |--> (2^a)(3^b).
; This mapping is surjective due to the fact that we can peform prime factorization
; on an integer n = (2^a)(3^b) to identify the exponents a, b of 2 and 3, respectively
; in the factorization. This mapping is injective to the uniqueness of prime
; factorization.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (factor-exponent z 2))

(define (cdr z)
  (factor-exponent z 3))