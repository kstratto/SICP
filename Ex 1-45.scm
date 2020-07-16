#lang racket
; Function for average damping: f(x) |-> (x + f(x))/2
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

; Function for computing fixed points of f
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? x1 x2)
    (< (abs (- x1 x2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Function to compute fixed point of transformation of function f
(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

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

; Based on experimenting, it appears that it requires floor(log(n, 2))
; average damps to make a fixed point search for the nth root
; using y |-> x/y^(n - 1) to converge.
(define (n-root x n)
  (let ((num-damps (floor (log n 2))))
    (fixed-point-of-transform
     (lambda (y) (/ x (expt y (- n 1))))
     (repeated average-damp num-damps)
     1.0)))