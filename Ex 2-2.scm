#lang racket
; Constructor for representing a point as a pair of coordinates (x, y)
(define (make-point x y)
  (cons x y))

; Selectors for x and y coordinates
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

; Constructor for representing a segment as a pair of points (p1, p2),
; where p1 = (x1, y1) and p2 = (x2, y2)
(define (make-segment p1 p2)
  (cons p1 p2))

; Selectors for start point, p1, and end point, p2
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

; Procedure for computing the midpoint of a segment
(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

; Helper function for printing points
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))