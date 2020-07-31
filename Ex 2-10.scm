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
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Represent dividing two intervals as multiplying the first by the reciprocal
; of the second.
; Note that if the bounds of the original interval are (a, b), then the bounds of
; the reciprocal interval are (1/b, 1/a)
; First check whether or not interval y contains zero, and throws an error if it does.
(define (div-interval x y)
  (if (contains-zero? y)
      (error "Interval y contains zero" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

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

; Exercise 2.9
; Let x = (x1, x2) and y = (y1, y2) be two arbitrary intervals, with widths
; w_x = (x2 - x1)/2 and w_y = (y2 - y1)/2, respectively.
; Then x + y = (x1 + y1, x2 + y2) has width w_{x + y} = ((x2 + y2) - (x1 + y1))/2.
; In other words, w_{x + y} = (x2 - x1)/2 + (y2 - y1)/2 = w_x + w_y.
; Therefore, the width of the sum of two intervals is the sum of the widths.
; Similarly, since we defined x - y as x + (-y), where -y and y have the same width,
; we see that the width of x - y is w_{x - y} = w_x + w_y
; In fact, the same argument applied to y - x = y + (-x) shows that the width
; of y - x is also w_{y - x} = w_x + w_y.
; Multiplication, and therefore also division, does not behave in this way.
; There are examples of intervals a, b, c, d for which w_a = w_c and w_b = w_d,
; but w_{a*b} != w_{c*d}.
; For example, consider a = c = (0,1), b = (0, 2) and d = (1, 3).
; Here a and c have the same width, 0.5, and b and d have the same width, 1.
; However, a*b = (0, 2) has a width of 1 while a*d = (0, 3) has a width of 1.5.
; Thus, the width of the product or quotient of two intervals is not a function of only
; the widths of the two intervals being multiplied or divided.

; Exercise 2.10
; Modify the code for div-interval to check whether or not the divisor is an interval
; which contains zero and signal an error if this occurs.
; To do this, we write a helper procedure contains-zero?
; This procedure takes in an interval and returns true if it spans zero, false otherwise.
(define (contains-zero? x)
  (and (<= (lower-bound x) 0) (<= 0 (upper-bound x))))