#lang racket
; Exercises 2.7-2.16 form an extended exercise regarding implementing
; interval arithmetic and associated operations

; Represent adding intervals as creating an interval with
; minimum value given by sum of the lower bounds,
; maximum value given by sum of the upper bounds
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y ))))

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
  
; Exercise 2.11
; Suppose our two intervals are x = (a, b) and y = (c, d).
; Recall that our definition for multiplication xy involved finding the minimum
; and maximum of the products of the bounds (i.e. ac, ad, bc, bd) and using the 
; minimum as the lower bound and the maximum as the upper bound of the product.
; In other words, xy = (min(ac, ad, bc, bd), max(ac, ad, bc, bd))
; The nine cases we can break mul-interval into are as follows.
; 1. If a, b, c, d are all positive, then xy = (ac, bd)
; 2. If a, b, c, d are all negative, then xy = (bd, ac)
; 3. If a, b are positive; c, d are negative, then xy = (bc, ad)
; 4. If a, b are negative; c, d are positive, then xy = (ad, bc)
; 5. If a, b, d are positive; c is negative, then xy = (bc, bd)
; 6. If a is negative; b, c, d are positive, then xy = (ad, bd)
; 7. If a, b, c are negative; d is positive, then xy = (ad, ac)
; 8. If a, c, d are negative; b is positive, then xy = (bc, ac)
; 9. If a, c are negative; b, d are positive, then xy = (min(ad, bc), max(ac, bd))
; To see why Case 9 involves more than two multiplications, consider the following
; two examples.
; Example 1: x = (-2, 1), y = (-3, 4) 
; Then min(ad, bc) = min(-8, -3) = -8 and max(ac, bd) = max(6, 4) = 6,
; so xy = (-8, 6)
; Example 2: x = (-2, 1), y = (-10, 1)
; Then min(ad, bc) = min(-2, -10) = -10 and max(ac, bd) = max(20, 1) = 20,
; so xy = (-10, 20)
; We now rewrite mul-interval above to use this perspective of breaking up the
; multiplication into cases based on the endpoints of the intervals.

; Represent multiplying intervals by having upper and lower bounds
; be the minimum and maximum of the products of the bounds, respectively
(define (mul-interval x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (>= a 0) (>= b 0) (>= c 0) (>= d 0))
           (make-interval (* a c) (* b d)))
		  ((and (< a 0) (< b 0) (< c 0) (< d 0))
           (make-interval (* b d) (* a c)))
		  ((and (>= a 0) (>= b 0) (< c 0) (< d 0))
           (make-interval (* b c) (* a d)))
		  ((and (< a 0) (< b 0) (>= c 0) (>= d 0))
           (make-interval (* a d) (* b c)))
		  ((and (>= a 0) (>= b 0) (< c 0) (>= d 0))
           (make-interval (* b c) (* b d)))
		  ((and (< a 0) (>= b 0) (>= c 0) (>= d 0))
           (make-interval (* a d) (* b d)))
		  ((and (< a 0) (< b 0) (< c 0) (>= d 0))
           (make-interval (* a d) (* a c)))
		  ((and (< a 0) (>= b 0) (< c 0) (< d 0))
           (make-interval (* b c) (* a c)))
		  ((and (< a 0) (>= b 0) (< c 0) (>= d 0))
           (make-interval (min (* a d) (* b c)) (max (* a c) (* b d)))))))
		   
; Original definition of multiplication for comparison
(define (orig-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
				   
; Alternate constructor and selectors for representing intervals as a center value
; and an additive tolerance
; For example, represent (3.35, 3.65) as 3.5 +- 0.15
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
  
; Exercise 2.12
; Define make-center-percent to represent an interval as a center value and
; a percentage tolerance.
; For example, represent (2.7, 3.3) as 3.0 +- 10%
; Note that the percent tolerance will be p%, so to convert to a decimal it is p/100
(define (make-center-percent c p)
  (let ((perc-diff (* c (/ p 100.0))))
    (make-interval (- c perc-diff) (+ c perc-diff))))
(define (percent i)
  (* (/ (width i) (center i)) 100.0))
  
; Exercise 2.13
; Suppose we have two intervals written in center-percent form.
; x1 = c1 +- p1% = (c1*(1 - p1/100), c1*(1 + p1/100))
; x2 = c2 += p2% = (c2*(1 - p2/100), c2*(1 + p2/100))
; Assuming, for simplicity, that all values are positive, the product of x1 and x2 is
; x1*x2 = (c1*c2*(1 - p1/100)*(1 - p2/100), c1*c2*(1 + p1/100)*(1 + p2/100)).
; For sufficiently small values of p1 and p2, the product (p1/100)*(p2/100)
; is negligible, so x1*x2 is approximately equal to the interval
; (c1*c2*(1 - (p1 + p2)/100), c1*c2*(1 + (p1 + p2)/100)).
; In other words, for sufficiently small tolerances and strictly positive intervals,
; the tolerance of the product of two intervals is approximately equal to the sum
; of the tolerances of the individual intervals.

; Exercise 2.14
; Two algebraically equivalent formulas for the resistance of two parallel resistors
; give different answers when working with intervals.
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
; One example that shows the behavoir which Lem encounters.
; > (define r1 (make-center-percent 6.8 10))
; > (define r2 (make-center-percent 4.7 5))
; > (par1 r1 r2)
; (2.201031010873943 . 3.4873689182805854)
; > (par2 r1 r2)
; (2.581558809636278 . 2.97332259363673)

; When written in center-percent form, the result from par1 is 2.84 +- 22.6%,
; while the result from par2 is 2.78 +- 7.1%.
; Another issue that arrises with interval arithmetic is the notion of multiplicative inverses.
; If we have an interval A, which in center-percent form is a +- p%, then we should have
; A/A = 1 from a purely algebraic perspective.
; Regardless of the actual value (of say the resistance), dividing a nonzero number
; by itself should give 1.
; However, because interval arithmetic as defined in this extended exercise cannot distinguish
; between the computation A/A and A/B where B is the same interval, but corresponds to
; a potentially different actual value, dividing an interval by itself introduces extra
; uncertainty.