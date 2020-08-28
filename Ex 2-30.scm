#lang racket
; The square-tree procedure takes a tree whose leaves are numbers as an argument
; and returns a tree of the same shape, where each number is squared.
; Ex: t = (1 (2 (3 4) 5) (6 7))
;     (square-tree t) = (1 (4 (9 16) 25) (36 49))

; Define square-tree directly (without using higher-order procedures such as map
(define (square-tree-direct tree)
        ; If tree is empty, return empty tree
  (cond ((null? tree) '())
        ; If we reach a leaf, return its square
        ((not (pair? tree)) (* tree tree))
        ; Otherwise, combine square-tree applied to the car of tree with
        ; square-tree applied to the cdr of tree
        (else (cons (square-tree-direct (car tree))
                    (square-tree-direct (cdr tree))))))

; Define square tree by using map and recursion
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
(square-tree-direct t)
(square-tree-map t)
