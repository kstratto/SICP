#lang racket
; Representation of zero as a Church numeral
; https://en.wikipedia.org/wiki/Church_encoding
; In this representation, nonnegative integers n are mapped
; to the n-fold composition of any function, f
; We can think of zero as not applying the function at all
(define zero (lambda (f) (lambda (x) x)))

; Representation of the operation of adding 1
; We can think of this as taking the representation of n,
; which corresponds to the n-fold composition of a function,
; and composing it one more time
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

; Direct definiton of one
(define one (lambda (f) (lambda (x) (f x))))

; Direct definition of two
(define two (lambda (f) (lambda (x) (f (f (x))))))

; Note that we can also directly define any nonnegative integer
; with the help of the results from Exercise 1.43
; Takes two functions f(x) and g(x) and returns f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

; Procedure which takes two arguments f(x) and n
; returns procedure which applies f to the argument n times.
; This is an iterative definition
(define (repeated f n)
  (define (iter i result)
    (if (< i n)
        (iter (+ i 1) (compose result f))
        result))
  (iter 1 f))

; Alternative definitions of one and two using the repeated function
(define one-alt (lambda (f) (lambda (x) ((repeated f 1) x))))
(define two-alt (lambda (f) (lambda (x) ((repeated f 2) x))))

; Direct definiton of additition of m and n should be the (m+n)-fold
; composition
(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))