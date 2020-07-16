#lang racket
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; If b is a positive number, add it to a.
; Otherwise, subtract b from a.
; This is equivalent to adding the absolute value of b to a.