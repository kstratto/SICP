#lang racket
(define (square x) (* x x))
(define (expmod a b m)
  (cond ((= b 0) 1)
        ((= (remainder b 2) 0)
         (remainder (square (expmod a (/ b 2) m)) m))
        (else
         (remainder (* a (expmod a (- b 1) m)) m))))
(define (fermat-test n)
  (define (try-it a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (try-it (+ a 1)))
          (else false)))
  (try-it 1))

;> (fermat-test 561)
;#t
;> (fermat-test 1105)
;#t
;> (fermat-test 1729)
;#t
;> (fermat-test 2465)
;#t
;> (fermat-test 2821)
;#t
;> (fermat-test 6601)
;#t