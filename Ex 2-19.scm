#lang racket
; Redefine change-counting program from Section 1.2.2 to use lists
; to easily change the currency used by the procedure.
; Do so by rewriting the procedure cc such that its second argument is a list
; of the values of the coins rather than an integer specifying which coins to use.

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

; no-more? checks whether or not list of coin values is empty
(define no-more? null?)

; except-first-denomination returns list of all items, except the first
(define except-first-denomination cdr)

; first-denomination returns the first value in a nonempty list
(define first-denomination car)

; The answer produced by cc is independent of the order of the list coin-values.
; This is because the algorithm in cc is order-independent:
; The number of ways to change amount a using n kinds of coins is equal to the sum of
; 1. The number of ways to change amount a using all but the first kind of coin
; 2. The number of ways to change amount a - d using all n kinds of coins,
;    where d is the denomination of the first kind of coin.
; While it is convenient to order the list by denomination, this description of the
; change counting algorithm doesn't specify how to order the list when determining
; the first kind of coin to use.