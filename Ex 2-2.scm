#lang racket
; Constructor for representing a point as a pair of coordinates (x, y)
(define (make-point x y)
  (cons x y))

; Selectors for x and y coordinates
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

; Function for averaging two points together
; i.e. computing midpoint between p1 and p2
(define (avg-pts p1 p2)
  (let ((x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
    (make-point (/ (+ x1 x2) 2)
                (/ (+ y1 y2) 2))))

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
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (avg-pts p1 p2)))

; Helper function for printing points
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))