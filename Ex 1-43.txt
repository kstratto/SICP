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

; This is a recursive definition for (repeated f n)
(define (repeated-recur f n)
  (if (= n 1)
      f
      (compose f (repeated-recur f (- n 1)))))