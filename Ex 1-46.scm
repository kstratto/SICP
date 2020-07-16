#lang racket
; iterative-improve takes two procedures as arguments
; good-enough? is a method for checking whether a guess is good enough
; improve is a method for improving a guess
; returns a procedure that takes a guess as an argument and keeps
; improving the guess until it is good enough
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x)
        x
        ((iterative-improve good-enough? improve) (improve x)))))

(define tol 0.0001)

; Rewrite sqrt from Section 1.1.7 in terms of iterative-improve
(define (iterative-improve-sqrt x)
  ((iterative-improve
    (lambda (y) (< (abs (- (* y y) x)) tol))
    (lambda (y) (/ (+ (/ x y) y) 2)))
   1.0))

; Rewrite fixed-point from Section 1.3.3 in terms of iterative-improve
(define (iterative-improve-fixed-point f first-guess)
  (define (close-enough? x1 x2)
    (< (abs (- x1 x2)) tol))
  ((iterative-improve
    (lambda (x) (close-enough? x (f x)))
    f)
   first-guess))