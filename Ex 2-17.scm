#lang racket
; Procedure last-pair returns the list that contains only the last element of a given
; (nonempty) list.
; Ex: (last-pair (list 23 72 149 34)) = (34)
(define (last-pair items)
  (let ((len (length items)))
    (list (list-ref items (- len 1)))))

; Note that while the list-ref and length procedures are explicitly defined in the text
; of SICP, I am using the implementations of those procedures included in the Racket
; language for convenience.

; Alternate solution which does not rely on checking the length
(define (last-pair-alt items)
  (let ((rest (cdr items)))
    (if (null? rest)
        items
        (last-pair-alt rest))))