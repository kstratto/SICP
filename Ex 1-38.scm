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

; Call to cont-frac to approximate e using Euler's continued fraction expansion for e - 2
(cont-frac-iter (lambda (i) 1.0)
                  (lambda (i)
                    (if (= 2 (remainder i 3))
                         (* 2 (+ 1 (floor (/ i 3))))
                         1))
                  10)