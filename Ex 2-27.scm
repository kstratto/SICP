#lang racket
; Modify reverse procedure from Exercise 2.18 to produce a deep-reverse procedure
; that takes a list as argument and returns as its value the list with its elements
; reversed and with all sublists deep-reversed as well.
; For example, deep reversing ((1 2) (3 4)) should give ((4 3) (2 1)) compared to how
; shallow reversing gives ((3 4) (1 2))

; Base Case 1: x is the empty list -> just return itself
; Base Case 2: x is not a list (i.e. x is a number) -> just return itself
; Reduction step: Return reverse of the list where deep-reverse is applied to each
; element of x.
(define (deep-reverse x)
  (cond ((null? x) x)
        ((not (list? x)) x)
        (else (reverse (map deep-reverse x)))))