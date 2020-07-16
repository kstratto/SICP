#lang racket
; Make rational number by reducing numerator and denominator to lowest terms
; before constructing pair
; Modified to normalize the sign per the following examples
; 1/2 and -1/2 are unchanged, -1/-2 becomes 1/2, and 1/-2 becomes -1/2
(define (make-rat n d)
  (let ((g (gcd n d))
        (num (if (> (* n d) 0)
                 (abs n)
                 (-(abs n))))
        (denom (abs d)))
    (cons (/ num g) (/ denom g))))

; Accessors for numerator, denominator of a rational number
(define (numer x) (car x))
(define (denom x) (cdr x))

; Definitions for adding, subtracting, multiplying, dividing rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mult-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

; Definitions for checking equality of two rational numbers
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Procedure for printing rational number as n/d
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))