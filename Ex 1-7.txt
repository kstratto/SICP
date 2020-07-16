#lang racket
(define (square x) (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (avg x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (avg guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))
; As written, our good-enough? test checks to see whether or not the square of the guess
; is at most a distance of 0.001 of x. For values small values of x, such as x <= 0.0001,
; since we are starting with an initial guess of 1.0 and iterating downward toward x,
; the first guess whose square is less than 0.001 will be accepted as being close enough.
; This is because the magnitude of the difference between x and any number with square
; less than 0.001 will be less than 0.001 as well.
; For example using this square root function with x = 0.0001 will return 0.0323 when
; it should return 0.01.
; For large values of x, limited machine precision means that the computer cannot represent
; small differences between large numbers. This will result in the improve function
; constantly returning the same value over and over again for certain values of x,
; causing the sqrt function to infinitely recurse.
; For example x = 10^13 is one such value where this occurs.
(define (new-good-enough? new-guess prev-guess)
  (< (/ (abs (- new-guess prev-guess)) prev-guess) 0.001))
(define (new-sqrt-iter guess x)
  (if (new-good-enough? (improve guess x) guess)
      guess
      (new-sqrt-iter (improve guess x) x)))
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))
; This new square root procedure says that a guess is good enought if the next guess
; is within 0.1% of the current guess. While it improves accuracy for small numbers,
; it provides less-accurate square roots for large numbers due to the fact that
; a 0.1% difference between large numbers can still be quite large in an absolute
; sense (e.g. 0.1% of 10^10 is 10^7).
; Also, this version of the square root procedure will hang if x = 0, since
; each new guess will always be half of the previous guess, so the threshold for
; new-good-enough? to evaluate to true is never met.
; One way to address the issue of hanging when x = 0 is to hard-code the x = 0 case.