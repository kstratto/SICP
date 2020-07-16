#lang racket
(define (square x) (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2) 3 (+ n 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
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
(define (start-prime-test n start-time)
  (cond ((prime? n)
      (report-prime n (- (current-inexact-milliseconds) start-time)))))
(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (search-for-primes n upper)
  (cond ((= (remainder n 2) 0) (search-for-primes (+ n 1) upper))
        ((or (< n upper) (= n upper))
         (timed-prime-test n)
         (search-for-primes (+ n 2) upper))
        (else (display "\nDone"))))
; Note that in 2020 computers are fast enough that checking for primes smaller than
; 1,000,000 do not provide meaningful timing information. As such, search for the first
; three primes larger than 1e9, 1e10, 1e11, and 1e12.
; The following are the reported times using the revised smallest-divisor procedure, with
; the times I obtained in Exercise 1.22 with the original smallest-divisor procedure
; in parentheses.
;(search-for-primes 1000000000 1000000021) 1e9
;1000000007***1.0 (3.0517578125)
;1000000009***1.9990234375 (3.000244140625)
;1000000021***2.00048828125 (2.983642578125)
;
;(search-for-primes 10000000000 10000000061) 1e10
;10000000019***7.00146484375 (8.0390625)
;10000000033***4.9990234375 (8.999267578125)
;10000000061***5.000732421875 (7.96337890625)
;
;(search-for-primes 100000000000 100000000057) 1e11
;100000000003***16.04052734375 (27.000732421875)
;100000000019***15.9970703125 (28.00537109375)
;100000000057***15.999755859375 (26.99560546875)
;
;(search-for-primes 1000000000000 1000000000063) 1e12
;1000000000039***52.10791015625 (86.052978515625)
;1000000000061***53.989990234375 (86.0517578125)
;1000000000063***53.999267578125 (85.052001953125)
;
; While we would expect the modification to half the runtime because it halves the number
; of steps, the actual result is that the runtimes with the modified smallest-divisor
; procedure are roughly 60% of those with the original. This discrepancy is due to
; the small amount of additional overhead from the if statement in the modified
; procedure.