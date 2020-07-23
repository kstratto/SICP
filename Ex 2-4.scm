#lang racket
; Alternative procedural representation of pairs

; Represent pair (x y) as a procedure which takes a single argument m
; and applies it to (x y)
(define (cons x y)
  (lambda (m) (m x y)))
; Represent (car z) by applying z to a procedure which takes two arguments
; and returns the first argument
(define (car z)
  (z (lambda (p q) p)))

; Represent (cdr z) by applying z to a procedure which takes two aruments
; and returns the second argument
(define (cdr z)
  (z (lambda (p q) q)))

; Example of how this representation works, by examining the substitution
; model for (car (cons x y)) and verifying that it yields x

; Start with (car (cons x y))
; (car (cons x y))
; Substitute in definition of (cons x y)
; (car (lambda (m) (m x y)))
; Substitute z = (lambda (m) (m x y)) into definition of (car z)
; ((lambda (m) (m x y)) (lambda (p q) p))
; Substitute m = (lambda (p q) p) into (lambda (m) (m x y))
; ((lambda (p q) p) x y)
; Substitute p = x, q = y into (lambda (p q) p)
; x