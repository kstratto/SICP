#lang racket
; Can represent a set as a list of distinct elements, and represent the set of all
; subsets of the set (the powerset) as a set of lists.
; For example, if the set is (1 2 3), then the powerset is
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).
; The following is a procedure that generates the powerset of a set.
; Note that the order of the elements in the powerset of the example is a hint
; as to how to implement the procedure.

(define (powerset s)
  (if (null? s)
      (list '()) ; The powerset of the empty set is the set containing the empty set
      (let ((rest (powerset (cdr s))))
        ; If s is nonempty with elements (x1 x2 ... xn),
        ; then the powerset of s is equal to the union of the powerset of (x2 ... xn)
        ; with the the set formed by of adding x1 to each element of the powerset
        ; of (x2 ... xn)
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define s (list 1 2 3))
(powerset s)
(define r (list 1 2 3 4))
(powerset r)