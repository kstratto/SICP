#lang racket
; fringe takes a tree (represented as a list) as an argument and returns a list whose
; elements are all the leaves of the tree arranged in left-to-right order
; Example: x = ((1 2) (3 4))
; (fringe x) = (1 2 3 4)
; (fringe (list x x)) = (1 2 3 4 1 2 3 4)
(define (fringe x)
  (cond ((null? x) '())
        ((not (list? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))