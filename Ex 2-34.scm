#lang racket
; Implement Horner's rule to evaluate the polynomial
; a_nx^n + a_{n-1}x^{n - 1} + ... + a_1x + a_0
; at a given value x by structuring the computation as
; (...(a_nx + a_{n - 1})x + ... + a_1)x + a_0
; In other words, start with a_n, multiply by x, add a_{n - 1}, multiply by x,
; and so on, until we reach a_0
; Assume the coefficients of the polynomial are arranged in a sequence,
; from a_0 through a_n.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coeffs)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coeffs))

; Example of evaluating 1 + 3x + 5x^3 + x^5 at x = 2
(horner-eval 2 (list 1 3 0 5 0 1))