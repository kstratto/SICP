#lang racket
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))))))
(define (f-iter n0 n1 n2 count)
  (if (< count 1)
      n0
      (f-iter n1 n2 (+ n2 (* 2 n1) (* 3 n0)) (- count 1))))
(define (f n)
  (f-iter 0 1 2 n))
; Note that while the recursive version of f is defined for all real numbers,
; the iterative version of f will not terminate if n is not a non-negative integer.
; One modification is to use the truncate function as follows.
(define (f-fixed n)
  (define dec-part (- n (truncate n)))
  (if (> n 0)
      (f-iter dec-part (+ dec-part 1) (+ dec-part 2) n)
      (f-iter n (+ n 1) (+ n 2) n)))