#lang racket
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sin (/ angle 3.0)))))

; Since the angle a is divide by 3.0 repeatedly until we obtain an angle that is less than
; or equal to the threshold (in this case 0.1), the number of times we recurse
; is equal to the ceiling of log(a, 3), the logarithm of a in base 3. In addition, since
; each level of recursion only invokes one additional call to the sine procedure,
; the number of steps is also the ceiling of log(a, 3).
; In other words, both the space and number of steps for (sine a) have an order of
; growth of Theta(ln(a))