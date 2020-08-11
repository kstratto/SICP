#lang racket
; Procedure reverse takes a list as an argument and returns a list of the same elements
; in reverse order.
; Ex: (reverse (list 1 4 9 16 25)) = (25 16 9 4 1)
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))