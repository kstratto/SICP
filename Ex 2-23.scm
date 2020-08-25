#lang racket
; for-each is similar to map, except the procedure is only called for its effect.
; In other words, for-each just applies the procedure to each of the elements in turn,
; while the values returned by applying the procedure to the elements are not used at all.
; for-each is generally used with procedures that perform an action, such as printing.
; While for-each is already implemented in Racket, we give one for this exercise.

(define (for-each proc items)
  ; Use cond with else instead of if to allow for block execution of statements
  (cond ((null? items) true)
        (else
         (proc (car items))
         (for-each proc (cdr items)))))