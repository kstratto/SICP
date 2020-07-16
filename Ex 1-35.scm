#lang racket
; To see that the golden ratio phi = (1 + sqrt(5))/2 is a fixed point of the transformation
; x |-> 1 + 1/x, simply compute 1 + 1/phi and see that it is equal to phi.
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? x1 x2)
    (< (abs (- x1 x2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Use fixed point to estimate the value of phi
(fixed-point
 (lambda (x) (+ 1 (/ 1 x)))
 1.0)