#lang racket
(define (square x) (* x x))

; I like this solution from the Scheme Wiki.
; http://community.schemewiki.org/?sicp-ex-1.28
; This solution does not take a probabilistic approach: (miller-rabin n) applies the test
; to all values 0 < a < n.
; Note that it uses the "let" special form, which had not been introduced in the
; book at this point.
(define (miller-rabin n)
   (if (= n 1) false (miller-rabin-test (- n 1) n)))
  
(define (miller-rabin-test a n) 
   (cond ((= a 0) true) 
         ; expmod is congruent to 1 modulo n 
         ((= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n)) 
         (else false))) 
  
(define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (let ((x (expmod base (/ exp 2) m))) 
            (if (non-trivial-sqrt? x m) 0 (remainder (square x) m)))) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m)))) 
  
(define (non-trivial-sqrt? n m) 
   (cond ((= n 1) false) 
         ((= n (- m 1)) false) 
         (else (= (remainder (square n) m) 1))))