#lang racket
; Modify fixed-point to display sequence of approximations generated.
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? x1 x2)
    (< (abs (- x1 x2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Use fixed point to find approximate solution to x^x = 1000
; Takes 34 steps without average damping
(fixed-point
 (lambda (x) (/ (log 1000) (log x)))
 2.0)

; Taks 9 steps with average damping
(fixed-point
 (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
 2.0)