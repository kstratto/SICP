#lang racket
; Use dotted-tail notation to write procedure same-parity that takes one or more integers
; and returns a list of all the arguments that have the same even-odd parity as the first
; argument.

; Version which uses the filter function, which isn't introduced officially until
; Section 2.2.3.
(define (same-parity x . y)
  (let ((parity (modulo x 2)))
    (filter (lambda (z) (= parity (modulo z 2)))
            (append (list x) y))))