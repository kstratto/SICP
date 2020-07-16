#lang racket
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

; Procedure which smooths a function f at the value x
; by taking the average of f(x - dx), f(x), and f(x + dx)
(define dx 0.0001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

; N-fold smoothing using the smooth and repaated functions
(define (n-fold-smooth f n)
  ((repeated smooth n) f))