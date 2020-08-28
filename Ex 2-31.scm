#lang racket
; Define a procedure tree-map with the property that square-tree from Exercise 2.30
; could be defined as follows:
; (define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  ; View tree as a sequence of sub-trees
  ; Map over the sequence and apply proc to each sub-tree in turn
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

; Defining square-tree in terms of tree-map
(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

; Defining scale-tree in terms of tree-map
(define (scale-tree tree factor)
  (tree-map (lambda (x) (* x factor)) tree))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
(square-tree t)
(scale-tree t 10)