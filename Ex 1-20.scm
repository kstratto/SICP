#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; Here is the substitution for evaluating (gcd 206 40) using applicative order evaluation
; For convenience, we will denote (remainder a b) by (r a b)
; (gcd 206 40)
; (gcd 40 (r 206 40)) = (gcd 40 6)
; (gcd 6 (r 40 6)) = (gcd 6 4)
; (gcd 4 (r 6 4)) = (gcd 4 2)
; (gcd 2 (r 4 2)) = (gcd 2 0) = 2
; Here, the remainder operation is performed 4 times.
; To contrast, this is the substitution for evaluating (gcd 206 40) using normal
; order evaluation. Recall that (if <pred> <cons> <alt>) is a special form where
; the predicate is evaluated first to determine whether to then evaluate the consequent
; or alternative expression
; (gcd 206 40)
; (if (= 40 0) ...)
; (gcd 40 (r 206 40))
; (if (= (r 206 40) 0) ...) = (if (= 6 0) ...)
; (gcd (r 206 40) (r 40 (r 206 40)))
; (if (= (r 40 (r 206 40)) 0) ... ) = (if (= 4 0) ...)
; (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
; (if (= (r (r 206 40) (r 40 (r 206 40))) 0) ...) = (if (= 2 0) ...)
; (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
; (if (= (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) 0) ...) = (if (= 0 0) ...)
; (r (r 206 40) (r 40 (r 206 40))) = 2
; We can see that with normal-order evaluation, the remainder operation is performed a total
; of 14 times when evaluating predicates in the special form if and then 4 times
; in the evaluation of the final reduction phase. Thus, normal-order operation
; results in a total of 18 evaluations of the remainder operation, in contrast to the 4
; evaluations with applicative-order evaluation.