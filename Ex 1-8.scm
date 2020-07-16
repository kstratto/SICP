#lang racket
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (pct-good-enough? new-guess prev-guess)
  (< (/ (abs (- new-guess prev-guess)) prev-guess) 0.001))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (abs-cubert-iter guess x)
  (if (abs-good-enough? guess x)
      guess
      (abs-cubert-iter (improve guess x) x)))
(define (pct-cubert-iter guess x)
  (if (pct-good-enough? (improve guess x) guess)
      guess
      (pct-cubert-iter (improve guess x) x)))
(define (cubert x)
  (cond ((= x 0) 0)
        ((< (abs x) 0.01) (pct-cubert-iter 1.0 x))
        (else (abs-cubert-iter 1.0 x))))
; Note that as written, this implementation for estimating cube roots fails due to
; division by zero when x = -2, as the second improved guess is 0.
; The approximation ((x/y^2) + 2y)/3 works best for x > 0.