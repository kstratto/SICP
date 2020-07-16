#lang racket
(define (square x) (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
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
;(search-for-primes 1000000000 1000000021) 1e9
;1000000007***3.0517578125
;1000000009***3.000244140625
;1000000021***2.983642578125
;
;(search-for-primes 10000000000 10000000061) 1e10
;10000000019***8.0390625
;10000000033***8.999267578125
;10000000061***7.96337890625
;
;(search-for-primes 100000000000 100000000057) 1e11
;100000000003***27.000732421875
;100000000019***28.00537109375
;100000000057***26.99560546875
;
;(search-for-primes 1000000000000 1000000000063) 1e12
;1000000000039***86.052978515625
;1000000000061***86.0517578125
;1000000000063***85.052001953125

; As we can see, increasing n by a factor of 10 approximately triples the time it takes
; to test each each prime number. This is in line with the fact that the testing algorithm
; has order of growth Theta(sqrt(n)), as sqrt(10) is a little more than 3.