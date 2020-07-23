#lang racket
; Instead of representing rectangles, we will represent arbitrary
; parallelograms in a plane.
; One way by representing parallograms as a pair of vectors,
; so implicitly they will have one vertex corresponding with the origin.
; Another way is by starting with three points and then converting those
; points into a pair of vectors.

; Constructor for representing a point as a pair of coordinates (x, y)
(define (make-point x y)
  (cons x y))

; Selectors for x and y coordinates
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

; Function for computing distance between two points
(define (dist p1 p2)
  (let ((x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2)))
    (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2)))))

; Constructor for representing a segment as a pair of points (p1, p2),
; where p1 = (x1, y1) and p2 = (x2, y2)
(define (make-segment p1 p2)
  (cons p1 p2))

; Selectors for start point, p1, and end point, p2
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

; Function for computing length of a segment
(define (len s)
  (dist (start-segment s) (end-segment s)))

; Constructor for representing a vector as segment which has start point at the origin
; and arbitrary end point.
(define (make-vector p)
  (make-segment (make-point 0 0) p))

; Selector for end point of vector
(define (end-vector v)
  (cdr v))

; Function for computing determinant of matrix generated by pair of vectors in plane
(define (det v1 v2)
  (let ((p1 (end-vector v1))
        (p2 (end-vector v2)))
    (- (* (x-point p1) (y-point p2))
       (* (y-point p1) (x-point p2)))))

; Constructor for representing parallelogram as a pair of vectors
(define (make-parallelogram-vectors v1 v2)
  (cons v1 v2))

; Constructor for representing parallelogram as a pair of vectors,
; starting from three points
(define (make-parallelogram-points p1 p2 p3)
  (let ((v1 (make-vector
             (make-point (- (x-point p2) (x-point p1))
                         (- (y-point p2) (y-point p1)))))
        (v2 (make-vector
             (make-point (- (x-point p3) (x-point p1))
                         (- (y-point p3) (y-point p1))))))
    (cons v1 v2)))

; Selectors for the two sides which generate the parallelogram
(define (side-1 par)
  (car par))
(define (side-2 par)
  (cdr par))

; Function for computing perimeter of parallelogram
(define (perim par)
  (let ((s1 (side-1 par))
        (s2 (side-2 par)))
    (* 2 (+ (len s1) (len s2)))))

; Function for computing area of parallelogram using determinant formula
; If parallelogram is generated by vectors a = (a1, a2) and b = (b1 b2),
; then area is the absolute value of the determinant of the matrix M
; with vectors a and b as rows/columns
; In other words, area is equal to abs(a1*b2 - a2*b1)
(define (area par)
  (let ((s1 (side-1 par))
        (s2 (side-2 par)))
    (abs (det s1 s2))))
