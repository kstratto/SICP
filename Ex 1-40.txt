#lang racket
; Function for computing fixed points of f
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

; Function for approximating derivative of g
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

; Express Newton's method as fixed-point process
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Procedure which takes in coefficients a b c and returns
; the cubic function f(x) = x^3 + ax^2 + bx + c
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))
; (newtons-method (cubic 1 2 3) 1) returns -1.2756822036498454
; which is in line with the value I obtained with numerically solving
; x^3 + x^2 + 2x + 3 = 0 on my graphing calculator.