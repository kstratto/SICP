#lang racket
; Exercises 2.7-2.16 form an extended exercise regarding implementing
; interval arithmetic and associated operations

; Represent adding intervals as creating an interval with
; minimum value given by sum of the lower bounds,
; maximum value given by sum of the upper bounds
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y ))))

; Represent multiplying intervals by having upper and lower bounds
; be the minimum and maximum of the products of the bounds, respectively
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Represent dividing two intervals as multiplying the first by the reciprocal
; of the second.
; Note that if the bounds of the original interval are (a, b), then the bounds of
; the reciprocal interval are (1/b, 1/a)
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

; Exercise 2.7
; Specify implementation of an interval as a pair
(define (make-interval a b)
  (cons a b))
(define (upper-bound x)
  (cdr x))
(define (lower-bound x)
  (car x))

; Exercise 2.8
; Given two intervals x = (x1, x2) and y = (y1, y2),
; the minimum value of the difference x - y would be x1 - y2
; and the maximum value of the difference x - y would be x2 - y1.
; Note that this interpretation of interval subtraction means that subtraction
; and addition of intervals are NOT inverses of each other.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
; An alternative way would be to define sub-interval in terms of add-interval
; by constructing the "negative" of y = (y1, y2) as -y = (-y2, -y1).
; This is more in-line with how dividing two intervals is defined in terms
; of multiplication.
(define (alt-sub-interval x y)
  (add-interval
   x
   (make-interval (* -1 (upper-bound y)) (* -1 (lower-bound y)))))