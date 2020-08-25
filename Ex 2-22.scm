#lang racket
; Louis Reasoner's first attempt at rewriting the hands-on square-list procedure
; as an interative process instead of a recursive one.
(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (expt (car things) 2) answer))))
  (iter items '()))
; This definition produces the answer list in the reverse order of the one desired
; because of the order of the arguments to cons. The first argument to cons is the
; first element of the pair it constructs, so this first attempt at an interative
; process can be thought of inserting the squared value at the front of the answer list
; instead of appending it to the end as desired. We can demonstrate this by working through
; an example and keeping track of the things and answer lists.
; (square-list-1 (list 1 2 3 4)):
; 1. things = (1 2 3 4), answer = ()
; 2. things = (2 3 4), answer = (1)
; 3. things = (3 4), answer = (4 1) because to get to this step we used (cons 4 (1))
; 4. things = (4), answer = (9 4 1)
; 5. things = (), answer = (16 9 4 1)

; Louis Reasoner's second attempt, where he interchanges the arguments to cons.
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (expt (car things) 2)))))
  (iter items '()))
; While this attempt does put the numbers in the answer in the correct order, it still doesn't
; work as Louis intended. This is because simply switching the order of the arguments
; to cons ignores the way that lists are constructed in Scheme.
; Recall that (list 1 2 3 4) is shorthand for (cons 1 (cons 2 (cons 3 (cons 4 '())))),
; so switching the order of the arguments to cons as Louis did means that we instead
; get nested pairs. This is a valid way if implementing lists, but it isn't the implemenation
; that is used in Scheme.

; One way to properly overcome the issues Louis faced is to use append
(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (expt (car things) 2))))))
  (iter items '()))