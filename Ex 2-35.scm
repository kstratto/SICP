#lang racket
; Redefining the count-leaves procedure from Section 2.22 as an accumulation
; Recall that count-leaves counted the number of leaves in a tree

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; The key idea is that count-leaves will essentially be the length procedure defined
; in Exercise 2.33, but we need to enumerate the leaves of t before passing it to
; accumulate.
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (enumerate-tree t)))
; Alternatively, can enumerate the tree, map every leaf to 1, and accumulate the sum
(define (count-leaves-alt t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list 1 (list 2 (list 3 4)) 5))
(count-leaves-alt (list 1 (list 2 (list 3 4)) 5))