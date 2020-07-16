#lang racket
; This procedure computes the kth element of the nth row of Pascal's triangle.
; We use the convention that the very top of the triangle is the 0th row
; and that the left-most element in a row is the 0th element
(define (pascal k n)
  (if (or (= k 0) (= k n))
      1
      (+ (pascal k (- n 1)) (pascal (- k 1) (- n 1)))))