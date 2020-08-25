#lang racket
; square-list takes a list of numbers and returns a list of the squares of those numbers

; Hands-on definition in terms of directly iterating over the list
(define (square-list-direct items)
  (if (null? items)
      '()
      (cons (expt (car items) 2)
            (square-list-direct (cdr items)))))

; Definition using map
(define (square-list-map items)
  (map (lambda (x) (expt x 2)) items))