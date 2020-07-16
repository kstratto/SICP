#lang racket
; See the notes here regarding limitations on the implementation of random in Racket.
; http://community.schemewiki.org/?sicp-ex-1.24
; Requiring the below library allows us to use the more-robust randome-integer procedure.
(#%require (lib "27.ss" "srfi"))
(define (square x) (* x x))
(define (expmod a b m)
  (cond ((= b 0) 1)
        ((= (remainder b 2) 0)
         (remainder (square (expmod a (/ b 2) m)) m))
        (else
         (remainder (* a (expmod a (- b 1) m)) m)))) 
(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
; I moved displaying the number from timed-prime-test to report-prime to clean up the output
; a bit and only print out prime numbers.
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display "***")
  (display elapsed-time))
; Note that the Racket language does not include a runtime primitive, so instead I am using
; current-inexact-milliseconds as a substitute
; See the Racket reference on time https://docs.racket-lang.org/reference/time.html
; for some more information
; When calling fast-prime? I am using an arbitrary value of times = 100
(define (start-prime-test n start-time)
  (cond ((fast-prime? n 100)
      (report-prime n (- (current-inexact-milliseconds) start-time)))))
(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (search-for-primes n upper)
  (cond ((= (remainder n 2) 0) (search-for-primes (+ n 1) upper))
        ((or (< n upper) (= n upper))
         (timed-prime-test n)
         (search-for-primes (+ n 2) upper))
        (else (display "\nDone"))))
; When using fast-prime, I followed the discussion regarding this exercise linked above
; and experimented with extremely large numbers: 1e45, 1e90, 1e135, and 1e180.
; As with the previous few exercises, the times are reported in milliseconds.
; 1e45
; 52.947265625
; 34.053466796875
; 39.94775390625
;
; 1e90
; 105.00048828125
; 109.0
; 122.0
;
; 1e135
; 201.053955078125
; 186.001708984375
; 174.005126953125
;
; 1e180
; 318.9541015625
; 390.993896484375
; 426.982177734375
;
; With these huge numbers, as also mentioned in the discussion linked above, there are
; additional computational costs assorted with performing operations on them. This
; results in a runtime that more than doubles when the number of digits is doubled,
; going against the theoretical expectations of logarithmic growth.