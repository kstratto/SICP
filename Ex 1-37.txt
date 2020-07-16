#lang racket
; Iterative version of k-term finite continued fraction
; Iterates by having a running total that is equal to ni / (di + previous total)
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (let ((ni (n i))
              (di (d i)))
          (iter (- i 1) (/ ni (+ di result))))))
  (iter k 0))

; Recursive version of k-term finite continued fraction
; Uses recursive relationship that k-term continued fraction is equal to
; n1/ (d1 + (k-1)-term continued fraction)
(define (cont-frac-recur n d k)
  (define (recur i)
    (if (> i k)
        0
        (let ((ni (n i))
              (di (d i)))
          (/ ni (+ di (recur (+ i 1)))))))
  (recur 1))

; To accurately estimate 1/phi = 2/(1 + sqrt(5)) to four decimal places, we need k > 11
;(cont-frac-iter (lambda (i) 1.0)
;                  (lambda (i) 1.0)
;                  11)
;0.6180555555555556
;> (cont-frac-iter (lambda (i) 1.0)
;                  (lambda (i) 1.0)
;                  12)
;0.6180257510729613