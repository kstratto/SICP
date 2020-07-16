#lang racket
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define(recur-add a b)
  (if (= a 0)
      b
      (inc (recur-add (dec a) b))))
; When evaluating (recur-add 4 5), we have the following.
; To save space, I will use (+ 4 5) to denote (recur-add 4 5).
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; Since this process involves a chain of deferred operations, it is a recursive process.
(define(iter-add a b)
  (if (= a 0)
      b
      (iter-add (dec a) (inc b))))
; When evaluating (iter-add 4 5), we have the following.
; To save space, I will use (+ 4 5) to denote (iter-add 4 5)
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; Since this process can be described by the state variables a and b, with the rule
; that to describe how to move from state to state, this process is an iterative one.
; In particular, if we were only given the values of a and b at a given step of the process,
; such as a = 2 and b = 7, we could still continue the iteration and successfully
; return the correct value of the function. In contrast, the deferrment of operations
; in recur-add means that we can't jump in part way and simply pick up from there.